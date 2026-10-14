"""Create deterministic local providers and browser test pages; no remote services."""

import json
from pathlib import Path
import sys
import struct
from urllib.request import Request, urlopen

from t256_helper.uri import compute_cid


def setup(state):
    state = Path(state)
    runtime = json.loads((state / "runtime.json").read_text())
    directory = state / "fixtures"
    directory.mkdir(exist_ok=True)
    def store(value, suffix):
        content = value.encode() if isinstance(value, str) else value
        cid = compute_cid(content)
        (directory / cid).write_bytes(content)
        return "t256:" + cid + suffix
    image = store('<svg xmlns="http://www.w3.org/2000/svg" width="48" height="48"><rect width="48" height="48" fill="#215cdd"/></svg>', ".svg")
    first = store('window.order = ["first"];', ".js")
    second = store('window.order.push("second"); document.querySelector("#order").textContent = window.order.join(",");', ".js")
    dependency = store('export const answer = 42; // a module dependency verified before the browser receives it', ".mjs")
    module = store('import { answer } from "' + dependency + '"; document.querySelector("#module").textContent = String(answer);', ".mjs")
    imported_css = store('#css { color: rgb(20, 80, 160); }', ".css")
    stylesheet = store('@\\69mport "' + imported_css.replace(":", "\\3a ") + '"; #css { background-image: \\75rl("' + image.replace(":", "\\3a ") + '"); }', ".css")
    page = '<!doctype html><html><head><link rel="stylesheet" href="' + stylesheet + '"><script type="importmap">' + json.dumps({"imports": {dependency: dependency}}) + '</script></head><body><picture><source srcset="' + image + ' 1x"><img id="image" src="' + image + '"></picture><p id="order">waiting</p><p id="module">waiting</p><p id="css">CSS</p><script src="' + first + '"></script><script src="' + second + '"></script><script type="module" src="' + module + '"></script></body></html>'
    root = store(page, ".html")
    samples = bytes(800)
    wave = struct.pack("<4sI4s4sIHHIIHH4sI", b"RIFF", 36 + len(samples), b"WAVE", b"fmt ", 16, 1, 1, 8000, 16000, 2, 16, b"data", len(samples)) + samples
    audio = store(wave, ".wav")
    captions = store("WEBVTT\n\n00:00.000 --> 00:00.050\nVerified caption\n", ".vtt")
    config = json.loads((state / "config.json").read_text())
    config["providers"] = [{"id": "fixtures", "type": "folder", "path": str(directory)}]
    request = Request(runtime["origin"] + "/api/config", data=json.dumps(config).encode(), headers={"Authorization": "Bearer " + runtime["admin"], "Content-Type": "application/json"})
    with urlopen(request) as response: response.read()
    fixture = {"root": root, "image": image, "first": first, "second": second, "module": module, "dependency": dependency,
               "missing": "t256:" + compute_cid(b"missing fixture resource" * 8) + ".png",
               "audio": audio, "captions": captions, "stylesheet": stylesheet}
    (state / "fixture.json").write_text(json.dumps(fixture))
    print("Created browser fixtures")


if __name__ == "__main__": setup(sys.argv[1])
