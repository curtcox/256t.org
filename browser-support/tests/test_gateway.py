import base64
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
import os
from pathlib import Path
import tempfile
import threading
import unittest
from unittest.mock import patch
from urllib.error import HTTPError
from urllib.request import Request, urlopen

from t256_helper.providers import get_url, github_headers
from t256_helper.resolver import Resolver, ResolutionError
from t256_helper.server import Gateway
from t256_helper.transform import css_urls, transform_page
from t256_helper.uri import compute_cid, parse_uri


class GatewayTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.resolver = Resolver(self.root / "state")
        self.server = Gateway(self.resolver, 0)
        self.thread = threading.Thread(target=self.server.serve_forever, daemon=True)
        self.thread.start()

    def tearDown(self):
        self.server.shutdown(); self.server.server_close(); self.thread.join()
        self.temp.cleanup()

    def request(self, path, origin=None, admin=False, body=None, headers=None, method=None):
        headers = dict(headers or {})
        if origin: headers["Origin"] = origin
        if admin: headers["Authorization"] = "Bearer " + self.server.admin
        if body is not None: headers["Content-Type"] = "application/json"
        request = Request(self.server.origin + path, headers=headers, data=json.dumps(body).encode() if body is not None else None, method=method)
        return urlopen(request, timeout=5)

    def test_admin_separation_and_rebinding(self):
        with self.assertRaises(HTTPError) as failure:
            self.request("/api/config", admin=True, origin="https://evil.example")
        self.assertEqual(failure.exception.code, 403)
        self.assertEqual(json.load(self.request("/api/config", admin=True))["version"], 1)
        with self.assertRaises(HTTPError) as failure:
            urlopen(Request(self.server.origin + "/health", headers={"Host": "evil.example"}))
        self.assertEqual(failure.exception.code, 403)

    def test_settings_index_import_and_cache_controls_require_management(self):
        folder = self.root / "ordinary"; folder.mkdir()
        content = b"content under an ordinary filename" * 5
        (folder / "report.txt").write_bytes(content)
        cid = compute_cid(content)
        config = dict(self.resolver.config)
        config["providers"] = [{"id": "documents", "type": "folder", "path": str(folder)}]
        self.resolver.configure(config)
        for path, body in [("/api/import-index", {"id": "documents", "mapping": {cid: "report.txt"}}),
                           ("/api/cache/clear", {}), ("/api/cache", None)]:
            with self.assertRaises(HTTPError) as failure: self.request(path, body=body)
            self.assertEqual(failure.exception.code, 403)
        result = json.load(self.request("/api/import-index", admin=True, body={"id": "documents", "mapping": {cid: "report.txt"}}))
        self.assertEqual(result, {"indexed": 1})
        self.assertEqual(self.request("/s/" + self.server.view + "/raw/" + cid).read(), content)
        self.assertEqual(json.load(self.request("/api/cache", admin=True))["entries"], 1)
        result = json.load(self.request("/api/cache/clear", admin=True, body={}))
        self.assertEqual(result["removed"], 1)
        self.assertEqual(result["bytes"], 0)
        for mapping in [{cid: "../private"}, {cid + ".txt": "report.txt"}]:
            with self.assertRaises(HTTPError) as failure:
                self.request("/api/import-index", admin=True, body={"id": "documents", "mapping": mapping})
            self.assertEqual(failure.exception.code, 400)
        self.assertEqual(self.resolver.load_index("documents"), {cid: "report.txt"})
        with self.assertRaises(HTTPError) as failure:
            self.request("/api/import-index", admin=True, body=[])
        self.assertEqual(failure.exception.code, 400)

    def test_startup_does_not_depend_on_reverse_dns(self):
        with patch('socket.getfqdn', side_effect=AssertionError('Unexpected DNS lookup')):
            server = Gateway(self.resolver, 0)
            self.assertGreater(server.server_port, 0)
            server.server_close()

    def test_grant_cors_and_revocation(self):
        config = dict(self.resolver.config); config["allowed_origins"] = ["https://publisher.example"]
        self.resolver.configure(config)
        token = self.server.grant("https://publisher.example")
        path = "/s/" + token + "/resource/" + compute_cid(b"hello") + ".txt"
        response = self.request(path, origin="https://publisher.example")
        self.assertEqual(response.read(), b"hello")
        self.assertEqual(response.headers["Access-Control-Allow-Origin"], "https://publisher.example")
        with self.assertRaises(HTTPError): self.request(path, origin="https://other.example")
        config["allowed_origins"] = []; self.resolver.configure(config)
        with self.assertRaises(HTTPError): self.request(path)

    def test_gateway_verifies_before_serving_and_transforms_html(self):
        folder = self.root / "files"; folder.mkdir()
        script = "t256:" + compute_cid(b"window.test = 1") + ".js"
        html = '<!doctype html><script src="' + script + '"></script><p>verified page</p>'
        cid = compute_cid(html.encode())
        (folder / cid).write_text(html)
        config = dict(self.resolver.config); config["providers"] = [{"id": "files", "type": "folder", "path": str(folder)}]
        self.resolver.configure(config)
        url = self.server.content_url(parse_uri("t256:" + cid + ".html"), navigation=True)
        response = urlopen(url)
        self.assertIn("sandbox allow-scripts", response.headers["Content-Security-Policy"])
        self.assertEqual(response.headers["X-T256-Representation"], "transformed")
        body = response.read().decode()
        self.assertIn(self.server.origin + "/s/" + self.server.view + "/resource/", body)
        self.assertEqual((self.resolver.cache / cid).read_bytes(), html.encode())
        (folder / cid).write_text("incorrect content")
        (self.resolver.cache / cid).unlink()
        with self.assertRaises(HTTPError) as failure: urlopen(url)
        self.assertEqual(failure.exception.code, 404)

    def test_module_opaque_origin_and_assets(self):
        uri = parse_uri("t256:" + compute_cid(b"export const value = 42;") + ".mjs")
        response = self.request("/s/" + self.server.view + "/resource/" + uri.cid + uri.suffix, origin="null")
        self.assertEqual(response.headers["Access-Control-Allow-Origin"], "null")
        self.assertEqual(response.read(), uri.inline)
        self.assertEqual(self.request("/implementations/ecmascript/uri.mjs").status, 200)

    def test_provider_http_size_and_redirect(self):
        payload = b"remote verified content" * 8
        cid = compute_cid(payload)
        class Provider(BaseHTTPRequestHandler):
            def do_GET(inner):
                if inner.path == "/redirect":
                    inner.send_response(302); inner.send_header("Location", "https://other.example/file"); inner.end_headers(); return
                inner.send_response(200); inner.end_headers(); inner.wfile.write(payload)
            def log_message(inner, *args): pass
        server = ThreadingHTTPServer(("127.0.0.1", 0), Provider)
        thread = threading.Thread(target=server.serve_forever, daemon=True); thread.start()
        base = "http://127.0.0.1:" + str(server.server_port)
        try:
            config = dict(self.resolver.config); config["providers"] = [{"id": "http", "type": "http", "url": base}]
            self.resolver.configure(config)
            self.assertEqual(self.resolver.resolve("t256:" + cid)[1], payload)
            with self.assertRaises(RuntimeError): get_url(base + "/redirect", 1024, 2)
            with self.assertRaises(RuntimeError): get_url(base + "/content", 10, 2)
        finally:
            server.shutdown(); server.server_close(); thread.join()

    def test_github_auth_is_only_in_headers(self):
        provider = {"token_env": "T256_TEST_CREDENTIAL"}
        with patch.dict(os.environ, {"T256_TEST_CREDENTIAL": "test-secret"}):
            self.assertEqual(github_headers(provider)["Authorization"], "Bearer test-secret")
            with self.assertRaises(RuntimeError): github_headers({**provider, "api_url": "http://api.example"})

    def test_verified_ranges_and_head(self):
        content = bytes(range(128))
        folder = self.root / "media"; folder.mkdir()
        cid = compute_cid(content); (folder / cid).write_bytes(content)
        config = dict(self.resolver.config)
        config["providers"] = [{"id": "media", "type": "folder", "path": str(folder)}]
        self.resolver.configure(config)
        path = "/s/" + self.server.view + "/resource/" + cid + ".mp4"
        for value, start, end in [("bytes=5-9", 5, 9), ("bytes=120-", 120, 127), ("bytes=-7", 121, 127),
                                  ("bytes=0-9999999999999999999999999", 0, 127), ("bytes=-999", 0, 127)]:
            with self.subTest(value=value):
                response = self.request(path, headers={"Range": value}, origin="null")
                self.assertEqual(response.status, 206)
                self.assertEqual(response.headers["Content-Range"], f"bytes {start}-{end}/128")
                self.assertEqual(response.read(), content[start:end + 1])
                self.assertEqual(response.headers["Accept-Ranges"], "bytes")
                self.assertIn("Content-Range", response.headers["Access-Control-Expose-Headers"])
        for value in ["bytes=128-", "bytes=-0", "bytes=9999999999999999999999999-"]:
            with self.subTest(value=value), self.assertRaises(HTTPError) as failure:
                self.request(path, headers={"Range": value})
            self.assertEqual(failure.exception.code, 416)
            self.assertEqual(failure.exception.headers["Content-Range"], "bytes */128")
        for value in ["bytes=5-3", "bytes=", "bytes=0-1,4-5", "items=0-5"]:
            self.assertEqual(self.request(path, headers={"Range": value}).read(), content)
        self.assertEqual(self.request(path, headers={"Range": "bytes=0-1", "If-Range": '"unknown"'}).status, 200)
        head = self.request(path, headers={"Range": "bytes=0-1"}, method="HEAD")
        self.assertEqual(head.status, 200)
        self.assertEqual(head.headers["Content-Length"], "128")
        self.assertEqual(head.read(), b"")
        (self.resolver.cache / cid).unlink(); (folder / cid).write_bytes(bytes(128))
        with self.assertRaises(HTTPError) as failure:
            self.request(path, headers={"Range": "bytes=0-1"})
        self.assertEqual(failure.exception.code, 404)

    def test_empty_and_transformed_range_responses(self):
        prefix = "/s/" + self.server.view
        with self.assertRaises(HTTPError) as failure:
            self.request(prefix + "/raw/" + compute_cid(b""), headers={"Range": "bytes=0-"})
        self.assertEqual(failure.exception.headers["Content-Range"], "bytes */0")
        css = b"p { color: red }"
        path = prefix + "/resource/" + compute_cid(css) + ".css"
        response = self.request(path, headers={"Range": "bytes=0-1"})
        self.assertEqual(response.status, 200)
        self.assertEqual(response.read(), css)
        self.assertIsNone(response.headers["Accept-Ranges"])


class TransformTests(unittest.TestCase):
    def test_css_and_import_map_without_rewriting_javascript(self):
        uri = "t256:" + compute_cid(b"test") + ".png"
        make_url = lambda item, navigation=False: "https://gateway.example/" + item.cid + item.suffix
        css = '/* url(' + uri + ') */ .x{content:"' + uri + '";background:url("' + uri + '")}'
        result = css_urls(css, make_url)
        self.assertIn('content:"' + uri + '"', result)
        self.assertIn('background:url("https://gateway.example/', result)
        page = '<script>const text="' + uri + '";</script><script type="importmap">' + json.dumps({"imports": {uri: uri}}) + '</script>'
        result = transform_page(page, make_url)
        self.assertIn('const text="' + uri + '"', result)
        self.assertIn('"' + uri + '": "https://gateway.example/', result)


if __name__ == "__main__": unittest.main()
