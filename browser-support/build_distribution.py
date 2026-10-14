#!/usr/bin/env python3
"""Create a small source distribution with all helper/browser runtime assets."""
from pathlib import Path
import zipfile


def build():
    root = Path(__file__).resolve().parents[1]
    destination = root / "browser-support/dist/t256-desktop.zip"
    destination.parent.mkdir(parents=True, exist_ok=True)
    files = [root / "resolve.html", root / "browser-support/README.md", root / "browser-support/run.py",
             root / "browser-support/build_extensions.py", root / "browser-support/build_loader.py",
             root / "implementations/ecmascript/cid.js", root / "implementations/ecmascript/uri.mjs"]
    for directory in ("t256_helper", "web", "extensions/shared"):
        files.extend(path for path in (root / "browser-support" / directory).rglob("*")
                     if path.is_file() and "__pycache__" not in path.parts)
    with zipfile.ZipFile(destination, "w", zipfile.ZIP_DEFLATED) as archive:
        for path in sorted(files): archive.write(path, path.relative_to(root))
    print("Created browser-support/dist/t256-desktop.zip")


if __name__ == "__main__": build()
