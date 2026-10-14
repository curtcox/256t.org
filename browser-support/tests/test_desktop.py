import io
import json
from pathlib import Path
import struct
import tempfile
import sys
import os
import signal
import subprocess
import time
import unittest
from unittest.mock import patch

from t256_helper.desktop import native_host, ENTRY
from t256_helper.install import launcher_text, native_manifest, install, uninstall
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

    def test_cli_accepts_browser_supplied_native_arguments(self):
        from t256_helper.__main__ import main
        with tempfile.TemporaryDirectory() as directory:
            with patch.object(sys, 'argv', ['run.py', '--state', directory, 'native', 'chrome-extension://' + 'a' * 32 + '/', '--parent-window=0']), patch('t256_helper.desktop.native_host') as host:
                self.assertEqual(main(), 0)
                host.assert_called_once()

    def test_native_process_starts_and_reuses_gateway(self):
        with tempfile.TemporaryDirectory() as directory:
            payload = json.dumps({"action": "settings"}).encode()
            packet = struct.pack("=I", len(payload)) + payload
            command = [sys.executable, str(ENTRY), "--state", directory, "native", "chrome-extension://" + "a" * 32 + "/", "--parent-window=0"]
            runtime = None
            try:
                first = subprocess.run(command, input=packet, capture_output=True, timeout=45, check=True)
                length = struct.unpack("=I", first.stdout[:4])[0]
                result = json.loads(first.stdout[4:4 + length])
                if 'error' in result:
                    log = Path(directory) / 'helper.log'
                    self.fail(str(result['error']) + '\n' + (log.read_text() if log.exists() else 'No helper log'))
                self.assertIn("/#", result["url"])
                runtime = json.loads((Path(directory) / "runtime.json").read_text())
                second = subprocess.run(command, input=packet, capture_output=True, timeout=45, check=True)
                self.assertEqual(first.stdout, second.stdout)
                self.assertEqual(runtime["pid"], json.loads((Path(directory) / "runtime.json").read_text())["pid"])
            finally:
                if runtime:
                    os.kill(runtime["pid"], signal.SIGTERM)
                    for _ in range(100):
                        try:
                            (Path(directory) / 'helper.log').unlink(missing_ok=True)
                            break
                        except PermissionError:
                            time.sleep(0.02)
                    if sys.platform != "win32":
                        for _ in range(50):
                            if not (Path(directory) / "runtime.json").exists(): break
                            time.sleep(0.02)

    @unittest.skipUnless(sys.platform == 'darwin', 'AppleScript compilation requires macOS')
    def test_macos_app_compiles_without_changing_os_registration(self):
        import plistlib
        run = subprocess.run
        def without_registration(command, *args, **kwargs):
            if command[0].endswith('/lsregister'):
                return subprocess.CompletedProcess(command, 0)
            return run(command, *args, **kwargs)
        with tempfile.TemporaryDirectory() as directory:
            resolver = Resolver(directory)
            with patch('t256_helper.install.subprocess.run', side_effect=without_registration):
                install(resolver)
                info = Path(directory) / 'launchers/t256 Helper.app/Contents/Info.plist'
                with info.open('rb') as stream: properties = plistlib.load(stream)
                self.assertEqual(properties['CFBundleURLTypes'][0]['CFBundleURLSchemes'], ['t256'])
                uninstall(resolver)
                self.assertFalse(info.exists())

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
