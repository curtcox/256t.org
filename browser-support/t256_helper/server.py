"""Loopback-only gateway. Management and content capabilities are separate."""

import hashlib
import hmac
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from socketserver import TCPServer
import json
import os
from pathlib import Path
import re
import secrets
import signal
import threading
from urllib.parse import parse_qs, quote, urlsplit
import webbrowser

from .resolver import ResolutionError, save_json
from .transform import css_urls, transform_page
from .uri import media_type, parse_uri


ASSETS = Path(__file__).resolve().parents[1] / "web"


def byte_range(value, length):
    """One HTTP byte range; None means ignore unsupported or malformed syntax."""
    match = re.fullmatch(r"bytes=([0-9]*)-([0-9]*)", value.strip(), re.IGNORECASE)
    if not match or not any(match.groups()):
        return None
    first, last = match.groups()
    # Range numbers can exceed platform integers. Values above the object length
    # only need a sentinel, including when padded with arbitrary leading zeros.
    def number(text):
        digits = text.lstrip("0") or "0"
        return length + 1 if len(digits) > 18 else int(digits)
    if first:
        start = number(first)
        end = number(last) if last else length - 1
        if last and end < start:
            return None
        if start >= length:
            raise ValueError("Unsatisfiable byte range")
        return start, min(end, length - 1)
    suffix = number(last)
    if not suffix or not length:
        raise ValueError("Unsatisfiable byte range")
    return max(0, length - suffix), length - 1


class Gateway(ThreadingHTTPServer):
    daemon_threads = True
    allow_reuse_address = True

    def server_bind(self):
        # HTTPServer normally performs a reverse-DNS lookup here. A loopback-only
        # helper does not need one, and macOS cold DNS lookups can delay startup.
        TCPServer.server_bind(self)
        self.server_name = "localhost"
        self.server_port = self.server_address[1]

    def __init__(self, resolver, port=25600):
        super().__init__(("127.0.0.1", port), Handler)
        self.resolver = resolver
        self.origin = "http://127.0.0.1:" + str(self.server_port)
        self.admin = secrets.token_urlsafe(32)
        self.view = secrets.token_urlsafe(32)
        self.settings_url = self.origin + "/#" + self.admin

    def grant(self, origin):
        if not isinstance(origin, str):
            raise ValueError("Expected a website origin")
        with self.resolver.lock:
            epoch = self.resolver.origin_epochs.get(origin)
            if epoch is None:
                raise ValueError("This origin has not been granted provider access")
            # Re-adding a revoked origin must issue a fresh capability. Unrelated
            # provider edits preserve grants to origins that remained allowed.
            return hmac.new(self.admin.encode(), (origin + "\0" + epoch).encode(), hashlib.sha256).hexdigest()

    def content_url(self, uri, token=None, navigation=False):
        return self.origin + "/s/" + (token or self.view) + ("/view/" if navigation else "/resource/") + uri.cid + uri.suffix


class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def log_message(self, format, *args):
        # Paths contain bearer capabilities and must not be written to access logs.
        pass

    def response(self, status, content=b"", mime="application/json", headers=None):
        self.send_response(status)
        self.send_header("Content-Type", mime)
        self.send_header("Content-Length", str(len(content)))
        self.send_header("X-Content-Type-Options", "nosniff")
        self.send_header("Referrer-Policy", "no-referrer")
        self.send_header("Cache-Control", "no-store")
        self.send_header("Connection", "close")
        origin = self.headers.get("Origin")
        if origin == self.server.origin or origin in self.server.resolver.config.get("allowed_origins", []) or getattr(self, "opaque_content", False):
            self.send_header("Access-Control-Allow-Origin", origin)
            self.send_header("Vary", "Origin")
            self.send_header("Access-Control-Expose-Headers", "Accept-Ranges, Content-Range, X-T256-Source-CID, X-T256-Representation")
        for key, value in (headers or {}).items():
            self.send_header(key, value)
        self.end_headers()
        if self.command != "HEAD":
            self.wfile.write(content)
        self.close_connection = True

    def json(self, status, value):
        self.response(status, json.dumps(value).encode("utf-8"))

    def valid_host(self):
        return self.headers.get("Host") == "127.0.0.1:" + str(self.server.server_port)

    def admin(self):
        origin = self.headers.get("Origin")
        return origin in (None, self.server.origin) and hmac.compare_digest(
            self.headers.get("Authorization", ""), "Bearer " + self.server.admin)

    def content_access(self, token):
        origin = self.headers.get("Origin")
        if hmac.compare_digest(token, self.server.view):
            return origin in (None, self.server.origin, "null")
        with self.server.resolver.lock:
            for allowed in self.server.resolver.origin_epochs:
                if hmac.compare_digest(token, self.server.grant(allowed)):
                    return origin in (None, allowed, "null")
        return False

    def do_OPTIONS(self):
        if not self.valid_host():
            return self.json(403, {"error": "Invalid loopback host"})
        origin = self.headers.get("Origin")
        if origin != self.server.origin and origin not in self.server.resolver.config.get("allowed_origins", []):
            return self.json(403, {"error": "Origin has not been granted provider access"})
        self.response(204, headers={"Access-Control-Allow-Methods": "GET, HEAD, POST, OPTIONS",
                                    "Access-Control-Allow-Headers": "Authorization, Content-Type, Range"})

    def do_HEAD(self):
        self.do_GET()

    def do_GET(self):
        if not self.valid_host():
            return self.json(403, {"error": "Invalid loopback host"})
        parts = urlsplit(self.path)
        if parts.path == "/health":
            return self.json(200, {"service": "t256-helper", "version": 1})
        assets = {"/": "settings.html", "/settings.mjs": "settings.mjs", "/loader.mjs": "loader.mjs", "/loader.js": "loader.js", "/demo.html": "demo.html", "/demo.js": "demo.js"}
        cid_assets = {"/implementations/ecmascript/cid.js", "/implementations/ecmascript/uri.mjs"}
        public_assets = {"/resolve.html", "/browser-support/web/resolver.mjs", "/browser-support/web/web-client.mjs", "/browser-support/web/loader.mjs", "/browser-support/web/protocol-handler.mjs", "/browser-support/web/connection-preference.mjs"}
        if parts.path in public_assets:
            path = ASSETS.parents[1] / parts.path.lstrip("/")
            return self.response(200, path.read_bytes(), "text/html; charset=utf-8" if path.suffix == ".html" else "text/javascript; charset=utf-8")
        if parts.path in cid_assets:
            path = ASSETS.parents[1] / parts.path.lstrip("/")
            return self.response(200, path.read_bytes(), "text/javascript; charset=utf-8")
        if parts.path in assets:
            path = ASSETS / assets[parts.path]
            if not path.exists():
                return self.json(404, {"error": "Asset not found"})
            mime = "text/html; charset=utf-8" if path.suffix == ".html" else "text/javascript; charset=utf-8"
            headers = {"Content-Security-Policy": "default-src 'self'; script-src 'self'; style-src 'self' 'unsafe-inline'; frame-ancestors 'none'"}
            if parts.path == "/demo.html":
                headers["Content-Security-Policy"] = "default-src 'self'; script-src 'self' blob: 'unsafe-inline'; img-src 'self' blob:; worker-src blob:; style-src 'self' 'unsafe-inline'; frame-ancestors 'none'"
            return self.response(200, path.read_bytes(), mime, headers)
        if parts.path == "/api/config":
            return self.json(200, self.server.resolver.config) if self.admin() else self.json(403, {"error": "Management access required"})
        if parts.path == "/api/cache":
            return self.json(200, self.server.resolver.cache_stats()) if self.admin() else self.json(403, {"error": "Management access required"})
        if parts.path == "/api/index":
            if not self.admin():
                return self.json(403, {"error": "Management access required"})
            try:
                identifier = parse_qs(parts.query).get("id", [""])[0]
                return self.json(200, self.server.resolver.export_index(identifier))
            except ValueError as error:
                return self.json(400, {"error": str(error)})
            except OSError:
                return self.json(502, {"error": "Index could not be read"})
        route = parts.path.split("/")
        if len(route) < 4 or route[1] != "s" or not self.content_access(route[2]):
            return self.json(403, {"error": "A valid content access capability is required"})
        token, operation = route[2:4]
        self.opaque_content = self.headers.get("Origin") == "null"
        try:
            if operation == "resolve" and len(route) == 4:
                value = parse_qs(parts.query).get("uri", [""])[0]
                uri, _, provenance = self.server.resolver.resolve(value)
                return self.json(200, {"uri": uri.text, "length": uri.length, "type": media_type(uri),
                                       "url": self.server.content_url(uri, token), **provenance})
            if operation not in ("resource", "view", "raw") or len(route) != 5:
                return self.json(404, {"error": "Unknown content operation"})
            uri, content, provenance = self.server.resolver.resolve("t256:" + route[4])
            mime = media_type(uri)
            transformed = False
            make_url = lambda item, navigation=False: self.server.content_url(item, token, navigation)
            if operation != "raw" and mime == "text/css":
                content = css_urls(content.decode("utf-8-sig"), make_url).encode("utf-8")
                transformed = True
            if operation == "view" and mime == "text/html":
                content = transform_page(content.decode("utf-8-sig"), make_url).encode("utf-8")
                transformed = True
            if transformed:
                mime += "; charset=utf-8"
            headers = {"X-T256-Source-CID": uri.cid, "X-T256-Representation": "transformed" if transformed else "original"}
            if media_type(uri) in ("text/html", "image/svg+xml"):
                # Content has an opaque origin and cannot acquire management access.
                origin = self.server.origin
                headers["Content-Security-Policy"] = "sandbox allow-scripts allow-downloads allow-top-navigation-by-user-activation; default-src 'none'; script-src " + origin + " 'unsafe-inline'; style-src " + origin + " 'unsafe-inline'; img-src " + origin + " data: blob:; font-src " + origin + "; media-src " + origin + "; frame-src " + origin + "; connect-src " + origin
            if not transformed:
                headers["Accept-Ranges"] = "bytes"
                # Resolve and verify the entire source before returning any slice.
                # HEAD ignores Range, and If-Range cannot match absent validators.
                if self.command == "GET" and self.headers.get("Range") and not self.headers.get("If-Range"):
                    try:
                        selected = byte_range(self.headers["Range"], len(content))
                    except ValueError:
                        headers["Content-Range"] = "bytes */" + str(len(content))
                        return self.response(416, b"", mime, headers)
                    if selected is not None:
                        start, end = selected
                        headers["Content-Range"] = f"bytes {start}-{end}/{len(content)}"
                        return self.response(206, content[start:end + 1], mime, headers)
            return self.response(200, content, mime, headers)
        except ResolutionError as error:
            self.json(404, {"error": str(error), "failures": error.failures})
        except (ValueError, UnicodeError) as error:
            self.json(400, {"error": str(error)})
        except (OSError, RuntimeError):
            self.json(502, {"error": "Provider retrieval failed"})

    def do_POST(self):
        if not self.valid_host() or not self.admin():
            return self.json(403, {"error": "Management access required"})
        try:
            length = int(self.headers.get("Content-Length", "0"))
            if length <= 0 or length > 1024 * 1024:
                return self.json(413, {"error": "Invalid management request size"})
            value = json.loads(self.rfile.read(length))
            if not isinstance(value, dict):
                raise ValueError("Management requests must be JSON objects")
            if self.path == "/api/config":
                self.server.resolver.configure(value)
                return self.json(200, {"saved": True})
            if self.path == "/api/index":
                count = self.server.resolver.index(value["id"])
                return self.json(200, {"indexed": count})
            if self.path == "/api/import-index":
                count = self.server.resolver.import_index(value["id"], value["mapping"])
                return self.json(200, {"indexed": count})
            if self.path == "/api/cache/clear":
                return self.json(200, {"removed": self.server.resolver.clear_cache(), **self.server.resolver.cache_stats()})
            if self.path == "/api/grant":
                origin = value["origin"]
                token = self.server.grant(origin)
                return self.json(200, {"base": self.server.origin + "/s/" + token})
            if self.path == "/api/open":
                uri = parse_uri(value["uri"])
                return self.json(200, {"url": self.server.content_url(uri, navigation=True)})
            if self.path == "/api/check":
                uri, _, provenance = self.server.resolver.resolve(value["uri"])
                return self.json(200, {"uri": uri.text, "length": uri.length, "type": media_type(uri), **provenance})
            return self.json(404, {"error": "Unknown management operation"})
        except ResolutionError as error:
            self.json(404, {"error": str(error), "failures": error.failures})
        except (ValueError, KeyError, StopIteration) as error:
            self.json(400, {"error": str(error) or "Provider not found"})
        except (OSError, RuntimeError):
            self.json(502, {"error": "Provider operation failed"})


def serve(resolver, port=25600, open_browser=False):
    server = Gateway(resolver, port)
    save_json(resolver.directory / "runtime.json", {"pid": os.getpid(), "origin": server.origin,
                                                  "settings_url": server.settings_url, "view": server.view,
                                                  "admin": server.admin})
    print("t256 helper listening at " + server.origin, flush=True)
    if open_browser:
        webbrowser.open(server.settings_url)
    def terminate(signum, frame):
        raise KeyboardInterrupt
    previous_signal = signal.signal(signal.SIGTERM, terminate)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.server_close()
        runtime_path = resolver.directory / "runtime.json"
        try:
            if json.loads(runtime_path.read_text())["pid"] == os.getpid():
                runtime_path.unlink(missing_ok=True)
        except (OSError, ValueError, KeyError):
            pass
        signal.signal(signal.SIGTERM, previous_signal)
