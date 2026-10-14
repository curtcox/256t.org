#!/usr/bin/env python3
"""Build unpacked Chromium, Firefox, and Safari Web Extension sources."""

import argparse
import json
from pathlib import Path
import shutil
import zipfile
from build_loader import uri_bundle


def build(destination):
    source = Path(__file__).parent / "extensions/shared"
    for browser in ("chromium", "firefox", "safari"):
        target = Path(destination) / browser
        shutil.copytree(source, target, dirs_exist_ok=True)
        (target / "uri.js").write_text(uri_bundle(source.parents[2]))
        manifest = json.loads((source / "manifest.json").read_text())
        if browser == "firefox":
            manifest["background"] = {"scripts": ["uri.js", "background.js"]}
            manifest["browser_specific_settings"] = {"gecko": {"id": "t256@256t.org", "strict_min_version": "128.0", "data_collection_permissions": {"required": ["none"]}}}
        if browser == "safari":
            manifest["permissions"].remove("nativeMessaging")
            manifest.pop("omnibox")
            # Safari uses manual website capabilities; Python native hosts are
            # not automatically part of its signed containing-app sandbox.
        (target / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")
        with zipfile.ZipFile(Path(destination) / (browser + ".zip"), "w", zipfile.ZIP_DEFLATED) as archive:
            for path in sorted(target.iterdir()): archive.write(path, path.name)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(); parser.add_argument("--output", type=Path, default=Path(__file__).parent / "dist/extensions")
    build(parser.parse_args().output)
