"""Strict parsing and verification according to draft-cox-t256-uri-scheme-00."""

import base64
import hashlib
import re
from dataclasses import dataclass


class VerificationError(ValueError):
    pass


def encode(data):
    return base64.urlsafe_b64encode(data).decode("ascii").rstrip("=")


def decode(value):
    data = base64.b64decode(value + "=" * (-len(value) % 4), altchars=b"-_", validate=True)
    if encode(data) != value:
        raise ValueError("Noncanonical base64url encoding")
    return data


def compute_cid(content):
    if len(content) >= 2**48:
        raise ValueError("Content exceeds the 48-bit length limit")
    payload = content if len(content) <= 64 else hashlib.sha512(content).digest()
    return encode(len(content).to_bytes(6, "big")) + encode(payload)


@dataclass(frozen=True)
class URI:
    cid: str
    suffix: str
    length: int
    inline: object

    @property
    def text(self):
        return "t256:" + self.cid + self.suffix

    def verify(self, content):
        if compute_cid(content) != self.cid:
            raise VerificationError("Content does not match its CID")


def parse_uri(value):
    if not isinstance(value, str):
        raise ValueError("Expected a t256 URI")
    match = re.fullmatch(r"(?i:t256):([A-Za-z0-9_-]{8,94})((?:\.[A-Za-z0-9]+)*)", value)
    if not match:
        raise ValueError("Expected t256:<CID>[.suffix], without //, escapes, query, or fragment")
    cid, suffix = match.groups()
    length = int.from_bytes(decode(cid[:8]), "big")
    payload = decode(cid[8:])
    expected = length if length <= 64 else 64
    if len(payload) != expected:
        raise ValueError("CID payload length does not match its length prefix")
    return URI(cid, suffix.lower(), length, payload if length <= 64 else None)


MIME_TYPES = {
    "txt": "text/plain", "html": "text/html", "htm": "text/html",
    "css": "text/css", "js": "text/javascript", "mjs": "text/javascript",
    "json": "application/json", "svg": "image/svg+xml", "png": "image/png",
    "jpg": "image/jpeg", "jpeg": "image/jpeg", "gif": "image/gif",
    "webp": "image/webp", "avif": "image/avif", "ico": "image/x-icon",
    "pdf": "application/pdf", "zip": "application/zip", "gz": "application/gzip",
    "mp3": "audio/mpeg", "wav": "audio/wav", "mp4": "video/mp4",
    "webm": "video/webm", "woff": "font/woff", "woff2": "font/woff2",
    "wasm": "application/wasm",
}


def media_type(uri):
    return MIME_TYPES.get(uri.suffix.rsplit(".", 1)[-1], "application/octet-stream")
