"""Provider configuration, provenance, bounded verified cache, and fallback."""

import json
import os
from pathlib import Path
import tempfile
import threading
import subprocess
from urllib.parse import urlsplit
from urllib.error import HTTPError, URLError

from .providers import ProviderError, build_index, retrieve
from .uri import parse_uri


DEFAULT_CONFIG = {
    "version": 1,
    "max_bytes": 64 * 1024 * 1024,
    "cache_bytes": 256 * 1024 * 1024,
    "allowed_origins": [],
    "providers": [{"id": "hashbin", "type": "http", "url": "https://256t.us", "enabled": True}],
}


def save_json(path, value):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    fd, temporary = tempfile.mkstemp(dir=path.parent)
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as stream:
            json.dump(value, stream, indent=2)
            stream.write("\n")
        os.replace(temporary, path)
    finally:
        if os.path.exists(temporary):
            os.unlink(temporary)


def validate_config(config):
    if config.get("version") != 1:
        raise ValueError("Unsupported configuration version")
    for key in ("max_bytes", "cache_bytes"):
        if not isinstance(config.get(key), int) or config[key] <= 0:
            raise ValueError(key + " must be a positive byte count")
    ids = set()
    for provider in config.get("providers", []):
        identifier = provider.get("id", "")
        if not identifier or not identifier.replace("-", "").replace("_", "").isalnum() or identifier in ids:
            raise ValueError("Provider IDs must be unique letters, digits, hyphens, or underscores")
        ids.add(identifier)
        kind = provider.get("type")
        if kind not in ("folder", "http", "github", "git"):
            raise ValueError("Unknown provider type")
        required = "url" if kind == "http" else "repo" if kind == "github" else "path"
        if not isinstance(provider.get(required), str) or not provider[required]:
            raise ValueError("Provider requires " + required)
        if kind == "github" and len(provider["repo"].split("/")) != 2:
            raise ValueError("GitHub repository must be owner/repo")
        if kind in ("http", "github"):
            url = urlsplit(provider["url"] if kind == "http" else provider.get("api_url", "https://api.github.com"))
            if url.scheme not in ("http", "https") or not url.hostname or url.username or url.password or url.query or url.fragment:
                raise ValueError("Provider URL must be an HTTP(S) base without credentials, query, or fragment")
        if not isinstance(provider.get("timeout", 15), (float, int)) or provider.get("timeout", 15) <= 0:
            raise ValueError("Provider timeout must be positive")
        if kind in ("git", "github") and provider.get("ref", "HEAD").startswith("-"):
            raise ValueError("Repository revision cannot start with a hyphen")
    if not isinstance(config.get("allowed_origins", []), list):
        raise ValueError("allowed_origins must be an array")
    for origin in config.get("allowed_origins", []):
        url = urlsplit(origin)
        if url.scheme not in ("http", "https") or not url.hostname or origin != url.scheme + "://" + url.netloc or url.username or url.password:
            raise ValueError("Grant a specific HTTP(S) origin without a path or wildcard")
    return config


class ResolutionError(RuntimeError):
    def __init__(self, failures):
        self.failures = failures
        super().__init__("No selected provider returned matching content")


class Resolver:
    def __init__(self, directory):
        self.directory = Path(directory)
        self.config_path = self.directory / "config.json"
        self.cache = self.directory / "cache"
        self.lock = threading.RLock()
        self.directory.mkdir(parents=True, exist_ok=True)
        if os.name != "nt":
            self.directory.chmod(0o700)
        if not self.config_path.exists():
            save_json(self.config_path, DEFAULT_CONFIG)
        self.config = validate_config(json.loads(self.config_path.read_text(encoding="utf-8")))
        self.cache.mkdir(exist_ok=True)

    def configure(self, config):
        validate_config(config)
        with self.lock:
            save_json(self.config_path, config)
            self.config = config

    def index(self, identifier):
        provider = next(p for p in self.config["providers"] if p["id"] == identifier)
        result = build_index(provider, self.config["max_bytes"])
        save_json(self.directory / "indexes" / (identifier + ".json"), result)
        return len(result)

    def load_index(self, identifier):
        path = self.directory / "indexes" / (identifier + ".json")
        return json.loads(path.read_text(encoding="utf-8")) if path.exists() else {}

    def resolve(self, value):
        uri = parse_uri(value)
        config = self.config
        if uri.length > config["max_bytes"]:
            raise ValueError("Content exceeds the configured size limit")
        if uri.inline is not None:
            return uri, uri.inline, {"provider": "inline", "cache": False, "failures": []}
        path = self.cache / uri.cid
        with self.lock:
            if path.exists():
                # Reverify on every read: a corrupt or replaced cache entry cannot be served.
                if path.stat().st_size == uri.length:
                    content = path.read_bytes()
                    try:
                        uri.verify(content)
                        os.utime(path, None)
                        return uri, content, {"provider": "cache", "cache": True, "failures": []}
                    except ValueError:
                        pass
                path.unlink()
        failures = []
        for provider in config["providers"]:
            if not provider.get("enabled", True):
                continue
            try:
                content = retrieve(provider, uri, self.load_index(provider["id"]))
                uri.verify(content)
                with self.lock:
                    self.store(uri.cid, content, config["cache_bytes"])
                return uri, content, {"provider": provider["id"], "cache": False, "failures": failures}
            except (OSError, ValueError, ProviderError, HTTPError, URLError, TimeoutError, subprocess.SubprocessError) as error:
                # Avoid leaking provider paths, URLs, or credentials through browser diagnostics.
                reason = "HTTP " + str(error.code) if isinstance(error, HTTPError) else type(error).__name__
                failures.append({"provider": provider["id"], "reason": reason})
        raise ResolutionError(failures)

    def store(self, cid, content, budget):
        if len(content) > budget:
            return
        fd, temporary = tempfile.mkstemp(dir=self.cache, prefix=".pending-")
        try:
            with os.fdopen(fd, "wb") as stream:
                stream.write(content)
            os.replace(temporary, self.cache / cid)
        finally:
            if os.path.exists(temporary):
                os.unlink(temporary)
        entries = sorted((p for p in self.cache.iterdir() if not p.name.startswith(".")), key=lambda p: p.stat().st_mtime)
        total = sum(p.stat().st_size for p in entries)
        for entry in entries:
            if total <= budget:
                break
            total -= entry.stat().st_size
            entry.unlink()
