"""Read-only content providers. Credentials never leave the selected origin."""

import json
import os
from pathlib import Path
import subprocess
import threading
from urllib.parse import quote, urlencode, urlsplit
from urllib.request import HTTPRedirectHandler, Request, build_opener


class ProviderError(RuntimeError):
    pass


class SameOriginRedirects(HTTPRedirectHandler):
    def redirect_request(self, request, fp, code, msg, headers, new_url):
        old, new = urlsplit(request.full_url), urlsplit(new_url)
        if (old.scheme, old.hostname, old.port) != (new.scheme, new.hostname, new.port):
            raise ProviderError("Cross-origin redirect refused")
        return super().redirect_request(request, fp, code, msg, headers, new_url)


def read_bounded(stream, limit):
    chunks, total = [], 0
    while True:
        chunk = stream.read(min(65536, limit - total + 1))
        if not chunk:
            return b"".join(chunks)
        total += len(chunk)
        if total > limit:
            raise ProviderError("Provider returned more bytes than allowed")
        chunks.append(chunk)


def get_url(url, limit, timeout, headers=None):
    parts = urlsplit(url)
    if parts.scheme not in ("http", "https") or not parts.hostname or parts.username or parts.password:
        raise ProviderError("Provider must use an HTTP(S) URL without embedded credentials")
    request_headers = {"User-Agent": "t256-helper/0.1", "Accept-Encoding": "identity"}
    request_headers.update(headers or {})
    with build_opener(SameOriginRedirects()).open(Request(url, headers=request_headers), timeout=timeout) as response:
        return read_bounded(response, limit)


def inside(root, relative):
    root = Path(root).expanduser().resolve()
    target = (root / relative).resolve()
    try:
        target.relative_to(root)
    except ValueError:
        raise ProviderError("Path escapes the selected provider directory")
    return target


def github_headers(provider):
    headers = {"Accept": "application/vnd.github.raw+json", "X-GitHub-Api-Version": "2022-11-28"}
    token = os.environ.get(provider.get("token_env", "GH_TOKEN"))
    host = urlsplit(provider.get("api_url", "https://api.github.com")).hostname
    if not token and provider.get("use_gh", False):
        result = subprocess.run(["gh", "auth", "token", "--hostname", "github.com" if host == "api.github.com" else host],
                                capture_output=True, text=True, timeout=15)
        if result.returncode == 0:
            token = result.stdout.strip()
    if token:
        if urlsplit(provider.get("api_url", "https://api.github.com")).scheme != "https":
            raise ProviderError("Authenticated GitHub providers require HTTPS")
        headers["Authorization"] = "Bearer " + token
    return headers


def git_output(provider, args, limit=None):
    command = ["git", "-C", str(Path(provider["path"]).expanduser()), *args]
    with subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL) as process:
        timer = threading.Timer(provider.get("timeout", 20), process.kill)
        timer.start()
        try:
            # stdout is drained before waiting; cap file reads independently of repository size.
            result = read_bounded(process.stdout, limit) if limit is not None else process.stdout.read()
            if process.wait(timeout=provider.get("timeout", 20)):
                raise ProviderError("Git could not read the selected repository revision")
            return result
        except BaseException:
            process.kill()
            process.wait()
            raise
        finally:
            timer.cancel()


def retrieve(provider, uri, index):
    kind = provider["type"]
    timeout = provider.get("timeout", 15)
    if kind == "folder":
        target = inside(provider["path"], index.get(uri.cid, uri.cid))
        with target.open("rb") as stream:
            return read_bounded(stream, uri.length)
    if kind == "http":
        base = provider["url"].rstrip("/")
        # Retrieval uses the CID alone; interpretation of compound suffixes is local.
        headers = {}
        if provider.get("token_env"):
            token = os.environ.get(provider["token_env"])
            if not token:
                raise ProviderError("Provider credential environment variable is not set")
            if urlsplit(base).scheme != "https":
                raise ProviderError("Authenticated providers require HTTPS")
            headers["Authorization"] = "Bearer " + token
        return get_url(base + "/" + uri.cid, uri.length, timeout, headers)
    if kind == "github":
        owner, repo = provider["repo"].split("/")
        relative = index.get(uri.cid, "/".join(filter(None, [provider.get("directory", "cids").strip("/"), uri.cid])))
        api = provider.get("api_url", "https://api.github.com").rstrip("/")
        url = api + "/repos/" + quote(owner, safe="") + "/" + quote(repo, safe="") + "/contents/" + quote(relative, safe="/")
        url += "?" + urlencode({"ref": provider.get("ref", "HEAD")})
        return get_url(url, uri.length, timeout, github_headers(provider))
    if kind == "git":
        relative = index.get(uri.cid, "/".join(filter(None, [provider.get("directory", "cids").strip("/"), uri.cid])))
        return git_output(provider, ["show", provider.get("ref", "HEAD") + ":" + relative], uri.length)
    raise ProviderError("Unknown provider type")


def build_index(provider, max_bytes):
    from .uri import compute_cid
    result = {}
    if provider["type"] == "folder":
        root = Path(provider["path"]).expanduser().resolve()
        for target in sorted(root.rglob("*")):
            if not target.is_file() or target.is_symlink() or target.stat().st_size > max_bytes:
                continue
            relative = str(target.relative_to(root))
            with inside(root, relative).open("rb") as stream:
                result[compute_cid(read_bounded(stream, max_bytes))] = relative
    elif provider["type"] == "git":
        directory = provider.get("directory", "")
        files = git_output(provider, ["ls-tree", "-r", "-z", provider.get("ref", "HEAD"), "--", *([directory] if directory else [])])
        for entry in files.split(b"\0"):
            if not entry:
                continue
            metadata, name = entry.split(b"\t", 1)
            if not metadata.startswith(b"100"):
                continue
            relative = name.decode("utf-8")
            try:
                data = git_output(provider, ["show", provider.get("ref", "HEAD") + ":" + relative], max_bytes)
            except ProviderError:
                continue
            result[compute_cid(data)] = relative
    else:
        raise ProviderError("Use a local clone to index arbitrary repository filenames, or import a CID manifest")
    return result
