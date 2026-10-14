import io
import json
from pathlib import Path
import struct
import tempfile
import unittest
from unittest.mock import patch

from t256_helper.desktop import native_host
from t256_helper.install import launcher_text, native_manifest
from t256_helper.resolver import Resolver
from t256_helper.uri import compute_cid


class DesktopTests(unittest.TestCase):
    def test_native_framing_and_error_response(self):
        requests = [{"action": "settings"}, {"action": "unexpected"}]
        incoming = b"".join(struct.pack("=I", len(payload)) + payload for payload in (json.dumps(value).encode() for value in requests))
        output = io.BytesIO()
        with patch('t256_helper.desktop.ensure_gateway', return_value={"settings_url": "http://127.0.0.1:25600/#test-key"}):
            native_host(None, io.BytesIO(incoming), output)
        output.seek(0)
        length = struct.unpack("=I", output.read(4))[0]
        self.assertEqual(json.loads(output.read(length))["url"], "http://127.0.0.1:25600/#test-key")
        length = struct.unpack("=I", output.read(4))[0]
        self.assertIn("error", json.loads(output.read(length)))

    def test_native_oversized_control_message(self):
        with self.assertRaises(ValueError):
            native_host(None, io.BytesIO(struct.pack("=I", 100000)), io.BytesIO())

    def test_native_manifests_scope_extension_access(self):
        manifest = native_manifest("chrome", "a" * 32, "/tmp/native-host")
        self.assertEqual(manifest["allowed_origins"], ["chrome-extension://" + "a" * 32 + "/"])
        self.assertEqual(native_manifest("firefox", "t256@256t.org", "/tmp/host")["allowed_extensions"], ["t256@256t.org"])
        with self.assertRaises(ValueError): native_manifest("chrome", "*", "/tmp/native-host")

    def test_launcher_quoting(self):
        text = launcher_text("/Python executable", "/project's folder/run.py", "/state folder", "native")
        self.assertIn(' "$@"', text)
        self.assertIn("'\"'\"'", text)
        self.assertIn('"C:\\Program Files\\Python\\python.exe"', launcher_text("C:\\Program Files\\Python\\python.exe", "C:\\app\\run.py", "C:\\state", "native", True))
        with self.assertRaises(ValueError): launcher_text("python", "%bad%/run.py", "state", "native", True)

    def test_imported_repository_manifest_and_escape_rejection(self):
        with tempfile.TemporaryDirectory() as directory:
            resolver = Resolver(directory)
            config = dict(resolver.config); config["providers"] = [{"id": "repo", "type": "github", "repo": "owner/repo"}]
            resolver.configure(config)
            cid = compute_cid(b"repository content" * 8)
            self.assertEqual(resolver.import_index("repo", {cid: "articles/example.md"}), 1)
            self.assertEqual(resolver.load_index("repo")[cid], "articles/example.md")
            with self.assertRaises(ValueError): resolver.import_index("repo", {cid: "../secret"})


if __name__ == "__main__": unittest.main()
