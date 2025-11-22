import base64
import hashlib
from pathlib import Path
from typing import Tuple
from urllib.request import Request, urlopen


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


def download_cid(base_url: str, cid: str) -> Tuple[bytes, str, bool]:
    """
    Download content from a URL and validate its CID.
    
    Args:
        base_url: Base URL to download from (e.g., "https://256t.org")
        cid: Expected content identifier
        
    Returns:
        Tuple of (content, computed_cid, is_valid)
        - content: The downloaded content as bytes
        - computed_cid: The CID computed from the downloaded content
        - is_valid: True if computed_cid matches the expected cid
    """
    url = f"{base_url.rstrip('/')}/{cid}"
    req = Request(url, headers={'User-Agent': 'Mozilla/5.0'})
    with urlopen(req) as response:
        content = response.read()
    computed = compute_cid(content)
    return content, computed, computed == cid


__all__ = [
    "BASE_DIR",
    "EXAMPLES_DIR",
    "CIDS_DIR",
    "compute_cid",
    "download_cid",
]
