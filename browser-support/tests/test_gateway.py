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

    def request(self, path, origin=None, admin=False, body=None):
        headers = {}
        if origin: headers["Origin"] = origin
        if admin: headers["Authorization"] = "Bearer " + self.server.admin
        if body is not None: headers["Content-Type"] = "application/json"
        request = Request(self.server.origin + path, headers=headers, data=json.dumps(body).encode() if body is not None else None)
        return urlopen(request, timeout=5)

    def test_admin_separation_and_rebinding(self):
        with self.assertRaises(HTTPError) as failure:
            self.request("/api/config", admin=True, origin="https://evil.example")
        self.assertEqual(failure.exception.code, 403)
        self.assertEqual(json.load(self.request("/api/config", admin=True))["version"], 1)
        with self.assertRaises(HTTPError) as failure:
            urlopen(Request(self.server.origin + "/health", headers={"Host": "evil.example"}))
        self.assertEqual(failure.exception.code, 403)

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
