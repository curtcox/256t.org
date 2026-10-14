"""Start/reuse the helper and respond to browser native messages."""

import json
from pathlib import Path
import struct
import subprocess
import sys
import time
from urllib.parse import urlsplit
from urllib.request import Request, urlopen
import webbrowser

from .uri import parse_uri


ENTRY = Path(__file__).resolve().parents[1] / "run.py"


def running_gateway(resolver):
    runtime = json.loads((resolver.directory / "runtime.json").read_text(encoding="utf-8"))
    parts = urlsplit(runtime["origin"])
    if parts.scheme != "http" or parts.hostname != "127.0.0.1" or parts.path:
        raise ValueError("Invalid helper runtime address")
    # Authenticate the running instance; a stale port or another local service is insufficient.
    request = Request(runtime["origin"] + "/api/config", headers={"Authorization": "Bearer " + runtime["admin"]})
    with urlopen(request, timeout=2) as response:
        if json.load(response).get("version") != 1:
            raise ValueError("Unsupported running helper")
    return runtime


def ensure_gateway(resolver):
    try:
        return running_gateway(resolver)
    except (OSError, ValueError, KeyError):
        pass
    log = resolver.directory / "helper.log"
    with log.open("ab") as output:
        if sys.platform != "win32": log.chmod(0o600)
        options = {"creationflags": subprocess.DETACHED_PROCESS | subprocess.CREATE_NEW_PROCESS_GROUP} if sys.platform == "win32" else {"start_new_session": True}
        process = subprocess.Popen([sys.executable, str(ENTRY), "--state", str(resolver.directory), "serve", "--port", "0"],
                                   stdin=subprocess.DEVNULL, stdout=output, stderr=output, **options)
    deadline = time.monotonic() + 10
    while time.monotonic() < deadline:
        if process.poll() is not None:
            raise RuntimeError("Helper could not start; check helper.log in the settings directory")
        try:
            return running_gateway(resolver)
        except (OSError, ValueError, KeyError):
            time.sleep(0.05)
    raise RuntimeError("Helper did not become ready within ten seconds")


def open_uri(resolver, value):
    uri = parse_uri(value)
    runtime = ensure_gateway(resolver)
    url = runtime["origin"] + "/s/" + runtime["view"] + "/view/" + uri.cid + uri.suffix
    webbrowser.open(url)
    return url


def dispatch(resolver, message):
    if not isinstance(message, dict):
        raise ValueError("Native request must be an object")
    runtime = ensure_gateway(resolver)
    action = message.get("action")
    if action == "settings":
        return {"url": runtime["settings_url"]}
    if action == "open":
        uri = parse_uri(message.get("uri"))
        return {"url": runtime["origin"] + "/s/" + runtime["view"] + "/view/" + uri.cid + uri.suffix}
    if action == "connection":
        origin = message.get("origin")
        if origin not in resolver.config.get("allowed_origins", []):
            raise ValueError("Grant this website access in the helper settings first")
        request = Request(runtime["origin"] + "/api/grant", data=json.dumps({"origin": origin}).encode(),
                          headers={"Authorization": "Bearer " + runtime["admin"], "Content-Type": "application/json"})
        with urlopen(request, timeout=5) as response:
            return json.load(response)
    raise ValueError("Unsupported native action")


def read_exact(stream, length):
    result = bytearray()
    while len(result) < length:
        part = stream.read(length - len(result))
        if not part:
            raise EOFError("Truncated native message")
        result.extend(part)
    return bytes(result)


def native_host(resolver, input_stream=None, output_stream=None):
    input_stream = input_stream or sys.stdin.buffer
    output_stream = output_stream or sys.stdout.buffer
    # Windows text mode must not translate protocol bytes or newline characters.
    if sys.platform == "win32" and input_stream is sys.stdin.buffer:
        import msvcrt, os
        msvcrt.setmode(sys.stdin.fileno(), os.O_BINARY)
        msvcrt.setmode(sys.stdout.fileno(), os.O_BINARY)
    while True:
        header = input_stream.read(4)
        if not header:
            return
        if len(header) < 4:
            header += read_exact(input_stream, 4 - len(header))
        length = struct.unpack("=I", header)[0]
        if not 0 < length <= 64 * 1024:
            raise ValueError("Native request exceeds the control-message limit")
        try:
            message = json.loads(read_exact(input_stream, length))
            result = dispatch(resolver, message)
        except (OSError, ValueError, RuntimeError, KeyError, EOFError) as error:
            result = {"error": str(error)}
        payload = json.dumps(result).encode("utf-8")
        output_stream.write(struct.pack("=I", len(payload)) + payload)
        output_stream.flush()
