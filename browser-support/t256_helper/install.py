"""Per-user OS registration. No administrator permissions or browser settings edits."""

import json
import os
from pathlib import Path
import plistlib
import re
import shlex
import shutil
import subprocess
import sys

from .desktop import ENTRY
from .resolver import save_json


HOST_NAME = "org.t256.helper"
BROWSER_DIRS = {
    "darwin": {"chrome": "Google/Chrome", "edge": "Microsoft Edge", "brave": "BraveSoftware/Brave-Browser", "opera": "com.operasoftware.Opera", "firefox": "Mozilla"},
    "linux": {"chrome": "google-chrome", "edge": "microsoft-edge", "brave": "BraveSoftware/Brave-Browser", "opera": "opera", "firefox": "mozilla"},
}
REGISTRY = {"chrome": "Google\\Chrome", "edge": "Microsoft\\Edge", "brave": "BraveSoftware\\Brave-Browser", "opera": "Google\\Chrome", "firefox": "Mozilla"}


def launcher_text(python, entry, state, mode, windows=False):
    args = [str(python), str(entry), "--state", str(state), mode]
    if windows:
        # A native host must preserve stdin/stdout, so use python.exe rather than pythonw.exe.
        if any(any(char in value for char in '%!\r\n"') for value in args):
            raise ValueError("Windows launcher paths cannot contain %, !, quotes, or newlines")
        return "@echo off\r\n" + subprocess.list2cmdline(args) + " %*\r\n"
    return "#!/bin/sh\nexec " + " ".join(shlex.quote(value) for value in args) + ' "$@"\n'


def native_manifest(browser, extension_id, launcher):
    if browser == "firefox":
        if not extension_id or len(extension_id) > 128:
            raise ValueError("A Firefox extension ID is required")
        access = {"allowed_extensions": [extension_id]}
    else:
        if not re.fullmatch(r"[a-p]{32}", extension_id or ""):
            raise ValueError("Chromium extension ID must contain 32 letters from a through p")
        access = {"allowed_origins": ["chrome-extension://" + extension_id + "/"]}
    return {"name": HOST_NAME, "description": "t256 desktop resolver", "path": str(launcher), "type": "stdio", **access}


def install(resolver, browser=None, extension_id=None):
    if bool(browser) != bool(extension_id):
        raise ValueError("Supply both --browser and --extension-id to install native messaging")
    if browser:
        native_manifest(browser, extension_id, resolver.directory / "launchers/t256-native")
    destination = resolver.directory / "launchers"
    destination.mkdir(exist_ok=True)
    platform = "win32" if sys.platform == "win32" else "darwin" if sys.platform == "darwin" else "linux"
    suffix = ".cmd" if platform == "win32" else ""
    open_launcher = destination / ("t256-open" + suffix)
    native_launcher = destination / ("t256-native" + suffix)
    for path, mode in ((open_launcher, "open"), (native_launcher, "native")):
        path.write_text(launcher_text(sys.executable, ENTRY, resolver.directory, mode, platform == "win32"), encoding="utf-8")
        if platform != "win32": path.chmod(0o700)
    receipt_path = resolver.directory / "registration.json"
    receipt = json.loads(receipt_path.read_text()) if receipt_path.exists() else {"files": [], "registry": [], "platform": platform}
    if platform == "darwin":
        app = destination / "t256 Helper.app"
        # AppleScript applications receive opaque URLs without interpreting their CID as a host.
        command = " ".join(shlex.quote(value) for value in [sys.executable, str(ENTRY), "--state", str(resolver.directory), "open"])
        escaped = command.replace("\\", "\\\\").replace('"', '\\"')
        source = 'on open location uri\n do shell script "' + escaped + ' " & quoted form of uri\nend open location\non run\n do shell script "' + escaped.rsplit(" open", 1)[0] + ' settings"\nend run\n'
        script = destination / "handler.applescript"
        script.write_text(source)
        subprocess.run(["osacompile", "-o", str(app), str(script)], check=True, capture_output=True)
        info = app / "Contents/Info.plist"
        with info.open("rb") as stream: properties = plistlib.load(stream)
        properties.update(CFBundleIdentifier="org.t256.helper", CFBundleURLTypes=[{"CFBundleURLName": "256t content", "CFBundleURLSchemes": ["t256"]}])
        with info.open("wb") as stream: plistlib.dump(properties, stream)
        subprocess.run(["/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister", "-f", str(app)], check=True, capture_output=True)
        receipt["files"].extend([str(app), str(script)])
    elif platform == "linux":
        desktop = Path.home() / ".local/share/applications/org.t256.helper.desktop"
        desktop.parent.mkdir(parents=True, exist_ok=True)
        # Desktop Exec quoting is not shell quoting; reject field-code injection.
        if any(char in str(open_launcher) for char in '%\r\n'):
            raise ValueError("Unsupported desktop launcher path")
        executable = str(open_launcher).replace("\\", "\\\\").replace('"', '\\"').replace("`", "\\`").replace("$", "\\$")
        desktop.write_text('[Desktop Entry]\nType=Application\nName=t256 Helper\nNoDisplay=true\nExec="' + executable + '" %u\nMimeType=x-scheme-handler/t256;\n')
        previous = subprocess.run(["xdg-mime", "query", "default", "x-scheme-handler/t256"], capture_output=True, text=True, check=True).stdout.strip()
        receipt.setdefault("previous_linux_handler", previous)
        subprocess.run(["xdg-mime", "default", desktop.name, "x-scheme-handler/t256"], check=True)
        receipt["files"].append(str(desktop))
    else:
        import winreg
        key = "Software\\Classes\\t256"
        try:
            with winreg.OpenKey(winreg.HKEY_CURRENT_USER, key):
                if key not in receipt["registry"]:
                    raise ValueError("A per-user t256 handler already exists; select its replacement explicitly in Windows settings")
        except FileNotFoundError:
            pass
        with winreg.CreateKey(winreg.HKEY_CURRENT_USER, key) as handle:
            winreg.SetValueEx(handle, "", 0, winreg.REG_SZ, "URL:t256 content")
            winreg.SetValueEx(handle, "URL Protocol", 0, winreg.REG_SZ, "")
        with winreg.CreateKey(winreg.HKEY_CURRENT_USER, key + "\\shell\\open\\command") as handle:
            # Direct Python invocation avoids cmd.exe interpreting an untrusted URI.
            command = subprocess.list2cmdline([sys.executable, str(ENTRY), "--state", str(resolver.directory), "open"]) + ' "%1"'
            winreg.SetValueEx(handle, "", 0, winreg.REG_SZ, command)
        receipt["registry"].append(key)
    receipt["files"].extend([str(open_launcher), str(native_launcher)])
    save_json(receipt_path, receipt)
    if browser:
        manifest = native_manifest(browser, extension_id, native_launcher)
        if platform == "win32":
            import winreg
            manifest_path = destination / (browser + "-native.json")
            registry = "Software\\" + REGISTRY[browser] + "\\NativeMessagingHosts\\" + HOST_NAME
            with winreg.CreateKey(winreg.HKEY_CURRENT_USER, registry) as handle:
                winreg.SetValueEx(handle, "", 0, winreg.REG_SZ, str(manifest_path))
            receipt["registry"].append(registry)
        else:
            root = Path.home() / ("Library/Application Support" if platform == "darwin" else ".config")
            root = root / BROWSER_DIRS[platform][browser]
            if browser == "firefox":
                root = Path.home() / ("Library/Application Support/Mozilla" if platform == "darwin" else ".mozilla")
            manifest_path = root / ("NativeMessagingHosts" if browser != "firefox" else "NativeMessagingHosts" if platform == "darwin" else "native-messaging-hosts") / (HOST_NAME + ".json")
        if manifest_path.exists() and str(manifest_path) not in receipt["files"]:
            raise ValueError("A native messaging registration already exists at " + str(manifest_path))
        save_json(manifest_path, manifest)
        receipt["files"].append(str(manifest_path))
    receipt["files"] = list(dict.fromkeys(receipt["files"]))
    receipt["registry"] = list(dict.fromkeys(receipt["registry"]))
    save_json(receipt_path, receipt)
    print("Installed per-user t256 registration. Keep this checkout and Python executable at their current paths.")


def uninstall(resolver):
    receipt_path = resolver.directory / "registration.json"
    if not receipt_path.exists():
        print("No registrations were installed by this settings directory")
        return
    receipt = json.loads(receipt_path.read_text())
    if receipt["platform"] == "win32":
        import winreg
        def delete(key):
            try:
                with winreg.OpenKey(winreg.HKEY_CURRENT_USER, key) as handle:
                    children = []
                    for index in range(winreg.QueryInfoKey(handle)[0]): children.append(winreg.EnumKey(handle, index))
                for child in children: delete(key + "\\" + child)
                winreg.DeleteKey(winreg.HKEY_CURRENT_USER, key)
            except FileNotFoundError: pass
        for key in receipt["registry"]: delete(key)
    elif receipt["platform"] == "linux" and receipt.get("previous_linux_handler"):
        current = subprocess.run(["xdg-mime", "query", "default", "x-scheme-handler/t256"], capture_output=True, text=True, check=True).stdout.strip()
        if current == "org.t256.helper.desktop":
            subprocess.run(["xdg-mime", "default", receipt["previous_linux_handler"], "x-scheme-handler/t256"], check=True)
    for value in receipt["files"]:
        path = Path(value)
        if path.suffix == ".app" and path.is_dir():
            subprocess.run(["/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister", "-u", str(path)], capture_output=True)
            shutil.rmtree(path)
        else:
            path.unlink(missing_ok=True)
    receipt_path.unlink()
    print("Removed helper registrations; settings and verified cache were retained")
