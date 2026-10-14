"""Provider configuration, provenance, bounded verified cache, and fallback."""

import json
import os
from pathlib import Path
from pathlib import PurePosixPath
import tempfile
import threading
import subprocess
import re
import secrets
from concurrent.futures import Future
from copy import deepcopy
from http.client import HTTPException
from urllib.parse import urlsplit
from urllib.error import HTTPError, URLError

from .providers import ProviderError, build_index, retrieve
from .uri import parse_uri, VerificationError


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
    if not isinstance(config, dict) or not isinstance(config.get("providers"), list):
        raise ValueError("Configuration must contain a providers array")
    if type(config.get("version")) is not int or config["version"] != 1:
        raise ValueError("Unsupported configuration version")
    for key in ("max_bytes", "cache_bytes"):
        if type(config.get(key)) is not int or config[key] <= 0:
            raise ValueError(key + " must be a positive byte count")
    ids = set()
    for provider in config.get("providers", []):
        if not isinstance(provider, dict):
            raise ValueError("Each provider must be an object")
        identifier = provider.get("id", "")
        if not isinstance(identifier, str) or not re.fullmatch(r"[A-Za-z0-9_-]+", identifier) or identifier in ids:
            raise ValueError("Provider IDs must be unique letters, digits, hyphens, or underscores")
        ids.add(identifier)
        kind = provider.get("type")
        if kind not in ("folder", "http", "github", "git"):
            raise ValueError("Unknown provider type")
        required = "url" if kind == "http" else "repo" if kind == "github" else "path"
        if not isinstance(provider.get(required), str) or not provider[required] or "\0" in provider[required]:
            raise ValueError("Provider requires " + required)
        if kind == "github" and (len(provider["repo"].split("/")) != 2 or not all(provider["repo"].split("/"))):
            raise ValueError("GitHub repository must be owner/repo")
        for flag in ("enabled", "use_gh"):
            if flag in provider and type(provider[flag]) is not bool:
                raise ValueError(flag + " must be true or false")
        if "token_env" in provider and (not isinstance(provider["token_env"], str) or not re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", provider["token_env"])):
            raise ValueError("Credential environment variable must be a portable variable name")
        if kind in ("http", "github"):
            raw_url = provider["url"] if kind == "http" else provider.get("api_url", "https://api.github.com")
            if not isinstance(raw_url, str):
                raise ValueError("Provider URLs must be strings")
            url = urlsplit(raw_url)
            if url.scheme not in ("http", "https") or not url.hostname or url.username is not None or url.password is not None or url.query or url.fragment or "\0" in raw_url or any(char.isspace() for char in raw_url):
                raise ValueError("Provider URL must be an HTTP(S) base without credentials, query, or fragment")
            url.port  # Validate malformed and out-of-range ports before saving.
            if (provider.get("token_env") or kind == "github" and provider.get("use_gh")) and url.scheme != "https":
                raise ValueError("Authenticated providers require HTTPS")
        timeout = provider.get("timeout", 15)
        if type(timeout) not in (float, int) or not 0 < timeout <= 3600:
            raise ValueError("Provider timeout must be between 0 and 3600 seconds")
        if kind in ("git", "github"):
            ref = provider.get("ref", "HEAD")
            if not isinstance(ref, str) or not ref or ref.startswith("-") or "\0" in ref:
                raise ValueError("Repository revision must be a nonempty string without a leading hyphen")
            directory = provider.get("directory", "cids")
            if not isinstance(directory, str) or PurePosixPath(directory).is_absolute() or ".." in PurePosixPath(directory).parts or any(char in directory for char in ("\\", ":", "\0")):
                raise ValueError("Repository directory must be a relative path")
    if not isinstance(config.get("allowed_origins", []), list):
        raise ValueError("allowed_origins must be an array")
    for origin in config.get("allowed_origins", []):
        if not isinstance(origin, str):
            raise ValueError("Origins must be strings")
        url = urlsplit(origin)
        if url.scheme not in ("http", "https") or not url.hostname or origin != url.scheme + "://" + url.netloc or url.username is not None or url.password is not None or "*" in origin or "\0" in origin or any(char.isspace() for char in origin):
            raise ValueError("Grant a specific HTTP(S) origin without a path or wildcard")
        url.port
    return config


class ResolutionError(RuntimeError):
    def __init__(self, failures):
        self.failures = failures
        super().__init__("No selected provider returned matching content")


def failure_details(error):
    """Only fixed diagnostic text can cross the provider/browser boundary."""
    messages = {
        "provider_error": "Provider retrieval failed",
        "redirect_refused": "Provider redirected outside its allowed origin or supplied URL credentials",
        "size_mismatch": "Provider returned more bytes than the content identifier allows",
        "invalid_provider": "Provider URL is invalid",
        "path_escape": "File index points outside the selected provider",
        "auth_missing": "Selected credential environment variable is not set",
        "auth_unavailable": "Selected GitHub CLI login is unavailable; sign in and retry",
        "auth_requires_https": "Authenticated providers require HTTPS",
        "repository_unavailable": "Git could not read the selected repository or revision",
        "verification_failed": "Content did not match its identifier",
        "not_found": "Content or provider location was not found",
        "access_denied": "Provider access was denied",
        "timeout": "Provider retrieval timed out",
        "network_error": "Provider network connection failed",
        "invalid_data": "Provider data or file index is invalid",
    }
    if isinstance(error, HTTPError) and type(error.code) is int:
        status = error.code
        reason = {401: "Authentication was rejected", 403: "Access was denied or rate limited",
                  404: "Content was not found or repository access was not granted", 429: "Provider rate limit was reached",
                  451: "Content is unavailable for legal reasons"}.get(status, "Provider request failed")
        return {"code": "http_" + str(status), "status": status, "reason": "HTTP " + str(status) + ": " + reason}
    if isinstance(error, VerificationError): code = "verification_failed"
    elif isinstance(error, ProviderError): code = error.code if error.code in messages else "provider_error"
    elif isinstance(error, FileNotFoundError): code = "not_found"
    elif isinstance(error, PermissionError): code = "access_denied"
    elif isinstance(error, (TimeoutError, subprocess.TimeoutExpired)): code = "timeout"
    elif isinstance(error, URLError): code = "timeout" if isinstance(error.reason, TimeoutError) else "network_error"
    elif isinstance(error, HTTPException): code = "network_error"
    elif isinstance(error, ValueError): code = "invalid_data"
    else: code = "provider_error"
    return {"code": code, "reason": messages[code]}


def validate_index(mapping):
    if not isinstance(mapping, dict):
        raise ValueError("Index must map CIDs to relative filenames")
    for cid, path in mapping.items():
        if not isinstance(cid, str) or parse_uri("t256:" + cid).cid != cid:
            raise ValueError("Index keys must be CIDs without a scheme or suffix")
        if not isinstance(path, str) or not path or PurePosixPath(path).is_absolute() or ".." in PurePosixPath(path).parts or "\\" in path or ":" in path:
            raise ValueError("Index paths must be relative and cannot escape the provider")
    return mapping


class Resolver:
    def __init__(self, directory):
        self.directory = Path(directory)
        self.config_path = self.directory / "config.json"
        self.cache = self.directory / "cache"
        self.lock = threading.RLock()
        self.generation = 0
        self.inflight = {}
        self.directory.mkdir(parents=True, exist_ok=True)
        if os.name != "nt":
            self.directory.chmod(0o700)
        if not self.config_path.exists():
            save_json(self.config_path, DEFAULT_CONFIG)
        self.config = validate_config(json.loads(self.config_path.read_text(encoding="utf-8")))
        self.origin_epochs = {origin: secrets.token_urlsafe(16) for origin in self.config.get("allowed_origins", [])}
        self.cache.mkdir(exist_ok=True)

    def configure(self, config):
        validate_config(config)
        with self.lock:
            save_json(self.config_path, config)
            self.origin_epochs = {origin: self.origin_epochs.get(origin, secrets.token_urlsafe(16))
                                  for origin in config.get("allowed_origins", [])}
            self.config = config
            self.generation += 1
            self.trim_cache(config["cache_bytes"])

    def index(self, identifier):
        provider = next(p for p in self.config["providers"] if p["id"] == identifier)
        result = build_index(provider, self.config["max_bytes"])
        with self.lock:
            save_json(self.directory / "indexes" / (identifier + ".json"), result)
            self.generation += 1
        return len(result)

    def load_index(self, identifier):
        path = self.directory / "indexes" / (identifier + ".json")
        return json.loads(path.read_text(encoding="utf-8")) if path.exists() else {}

    def import_index(self, identifier, mapping):
        if not any(p["id"] == identifier for p in self.config["providers"]):
            raise ValueError("Provider not found")
        validate_index(mapping)
        with self.lock:
            save_json(self.directory / "indexes" / (identifier + ".json"), mapping)
            self.generation += 1
        return len(mapping)

    def export_index(self, identifier):
        with self.lock:
            if not any(p["id"] == identifier for p in self.config["providers"]):
                raise ValueError("Provider not found")
            path = self.directory / "indexes" / (identifier + ".json")
            if not path.exists():
                raise ValueError("Index files or import an index before exporting")
            return validate_index(self.load_index(identifier))

    def resolve(self, value):
        uri = parse_uri(value)
        path = self.cache / uri.cid
        with self.lock:
            config = self.config
            if uri.length > config["max_bytes"]:
                raise ValueError("Content exceeds the configured size limit")
            if uri.inline is not None:
                return uri, uri.inline, {"provider": "inline", "cache": False, "failures": []}
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
            key = (self.generation, uri.cid)
            pending = self.inflight.get(key)
            owner = pending is None
            if owner:
                pending = self.inflight[key] = Future()
        if owner:
            try:
                pending.set_result(self.fetch(uri, config))
            except BaseException as error:
                # Every exit releases waiting requests, including interrupted work.
                pending.set_exception(error)
                raise
            finally:
                with self.lock:
                    del self.inflight[key]
        # Never wait while holding the cache/configuration lock. The result is
        # verified original bytes; each caller keeps its own interpretation suffix.
        content, provenance = pending.result()
        return uri, content, deepcopy(provenance)

    def fetch(self, uri, config):
        failures = []
        for provider in config["providers"]:
            if not provider.get("enabled", True):
                continue
            try:
                content = retrieve(provider, uri, self.load_index(provider["id"]))
                uri.verify(content)
                with self.lock:
                    self.store(uri.cid, content, self.config["cache_bytes"])
                return content, {"provider": provider["id"], "cache": False, "failures": failures}
            except (OSError, ValueError, ProviderError, HTTPError, URLError, TimeoutError, subprocess.SubprocessError, HTTPException) as error:
                failures.append({"provider": provider["id"], **failure_details(error)})
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
        self.trim_cache(budget)

    def cache_entries(self):
        return [p for p in self.cache.iterdir() if not p.name.startswith(".") and p.is_file()]

    def cache_stats(self):
        with self.lock:
            entries = self.cache_entries()
            return {"entries": len(entries), "bytes": sum(p.stat().st_size for p in entries),
                    "budget": self.config["cache_bytes"]}

    def clear_cache(self):
        with self.lock:
            entries = self.cache_entries()
            for entry in entries:
                entry.unlink()
            return len(entries)

    def trim_cache(self, budget):
        entries = sorted(self.cache_entries(), key=lambda p: p.stat().st_mtime)
        total = sum(p.stat().st_size for p in entries)
        for entry in entries:
            if total <= budget:
                break
            total -= entry.stat().st_size
            entry.unlink()
