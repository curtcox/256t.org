import base64
import hashlib
from pathlib import Path


BASE_DIR = Path(__file__).resolve().parents[2]
EXAMPLES_DIR = BASE_DIR / "examples"
CIDS_DIR = BASE_DIR / "cids"


def to_base64url(data: bytes) -> str:
    return base64.urlsafe_b64encode(data).decode().rstrip("=")


def encode_length(length: int) -> str:
    return to_base64url(length.to_bytes(6, "big"))


def compute_cid(content: bytes) -> str:
    prefix = encode_length(len(content))
    if len(content) <= 64:
        suffix = to_base64url(content)
    else:
        suffix = to_base64url(hashlib.sha512(content).digest())
    return prefix + suffix


__all__ = [
    "BASE_DIR",
    "EXAMPLES_DIR",
    "CIDS_DIR",
    "compute_cid",
]
