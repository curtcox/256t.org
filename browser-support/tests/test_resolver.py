import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch

from t256_helper.resolver import Resolver, ResolutionError
from t256_helper.uri import compute_cid, encode, parse_uri, media_type


class ResolverTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.resolver = Resolver(self.root / "settings")

    def tearDown(self):
        self.temp.cleanup()

    def folders(self, *roots):
        config = dict(self.resolver.config)
        config["providers"] = [{"id": "folder" + str(i), "type": "folder", "path": str(root)} for i, root in enumerate(roots)]
        self.resolver.configure(config)

    def test_existing_corpus(self):
        corpus = Path(__file__).resolve().parents[2] / "cids"
        for path in corpus.iterdir():
            if path.is_file():
                self.assertEqual(compute_cid(path.read_bytes()), path.name)
                parse_uri("t256:" + path.name).verify(path.read_bytes())

    def test_inline_boundaries_and_suffix(self):
        for size in (0, 1, 63, 64, 65):
            data = bytes(range(size))
            uri = parse_uri("T256:" + compute_cid(data) + ".TAR.GZ")
            self.assertEqual(uri.length, size)
            self.assertEqual(uri.inline, data if size <= 64 else None)
            self.assertEqual(media_type(uri), "application/gzip")
            uri.verify(data)
        _, data, result = self.resolver.resolve("t256:" + compute_cid(b"hello"))
        self.assertEqual(data, b"hello")
        self.assertEqual(result["provider"], "inline")

    def test_invalid_and_noncanonical(self):
        valid = "t256:" + compute_cid(b"x")
        values = [valid + "=", valid + "?q=1", valid + "#f", valid.replace(":", "://"),
                  valid.replace("eA", "eB"), "t256:AAAAAAAB", "t256:" + encode((65).to_bytes(6, "big")),
                  valid + ".tar-gz", valid + ".", valid.replace("eA", "%65A"), " " + valid]
        for value in values:
            with self.subTest(value=value), self.assertRaises(ValueError):
                parse_uri(value)

    def test_fallback_cache_corruption_and_disable(self):
        bad, good = self.root / "bad", self.root / "good"
        bad.mkdir(); good.mkdir()
        content = b"verified content" * 8
        cid = compute_cid(content)
        (bad / cid).write_bytes(b"wrong content")
        (good / cid).write_bytes(content)
        self.folders(bad, good)
        _, actual, provenance = self.resolver.resolve("t256:" + cid)
        self.assertEqual(actual, content)
        self.assertEqual(provenance["provider"], "folder1")
        self.assertEqual(len(provenance["failures"]), 1)
        (self.resolver.cache / cid).write_bytes(b"broken cache")
        self.assertEqual(self.resolver.resolve("t256:" + cid)[1], content)
        self.assertTrue(self.resolver.resolve("t256:" + cid)[2]["cache"])
        (self.resolver.cache / cid).unlink()
        config = dict(self.resolver.config)
        config["providers"][1]["enabled"] = False
        self.resolver.configure(config)
        with self.assertRaises(ResolutionError):
            self.resolver.resolve("t256:" + cid)

    def test_index_and_escape(self):
        folder = self.root / "files"; folder.mkdir()
        data = b"ordinary filename" * 8
        (folder / "report.txt").write_bytes(data)
        self.folders(folder)
        self.assertEqual(self.resolver.index("folder0"), 1)
        self.assertEqual(self.resolver.resolve("t256:" + compute_cid(data))[1], data)
        other = b"private file outside root" * 8
        (self.root / "private").write_bytes(other)
        index = self.resolver.directory / "indexes/folder0.json"
        index.write_text(json.dumps({compute_cid(other): "../private"}))
        with self.assertRaises(ResolutionError):
            self.resolver.resolve("t256:" + compute_cid(other))

    def test_git_provider_indexes_committed_files(self):
        repo = self.root / "repo"; repo.mkdir()
        subprocess.run(["git", "init", "-q", str(repo)], check=True)
        data = b"committed private repository content" * 4
        (repo / "article.txt").write_bytes(data)
        subprocess.run(["git", "-C", str(repo), "add", "article.txt"], check=True)
        subprocess.run(["git", "-C", str(repo), "-c", "user.name=Test", "-c", "user.email=test@example.invalid", "commit", "-qm", "fixture"], check=True)
        config = dict(self.resolver.config)
        config["providers"] = [{"id": "git", "type": "git", "path": str(repo), "directory": ""}]
        self.resolver.configure(config)
        self.assertEqual(self.resolver.index("git"), 1)
        self.assertEqual(self.resolver.resolve("t256:" + compute_cid(data))[1], data)

    def test_size_limit(self):
        config = dict(self.resolver.config); config["max_bytes"] = 64
        self.resolver.configure(config)
        with self.assertRaises(ValueError):
            self.resolver.resolve("t256:" + compute_cid(b"x" * 65))

    def test_cache_budget_changes_and_clear_preserve_indexes(self):
        folder = self.root / "cache-fixture"; folder.mkdir()
        self.folders(folder)
        for i in range(3):
            content = bytes([i]) * 100; cid = compute_cid(content)
            (folder / cid).write_bytes(content)
            self.resolver.resolve("t256:" + cid)
            os.utime(self.resolver.cache / cid, (100 + i, 100 + i))
        self.assertEqual(self.resolver.cache_stats()["bytes"], 300)
        config = dict(self.resolver.config); config["cache_bytes"] = 180
        self.resolver.configure(config)
        self.assertEqual(self.resolver.cache_stats(), {"entries": 1, "bytes": 100, "budget": 180})
        self.assertTrue((self.resolver.cache / compute_cid(bytes([2]) * 100)).exists())
        self.resolver.index("folder0")
        self.assertEqual(self.resolver.clear_cache(), 1)
        self.assertEqual(self.resolver.cache_stats()["bytes"], 0)
        self.assertEqual(len(self.resolver.load_index("folder0")), 3)
        self.assertEqual(self.resolver.resolve("t256:" + compute_cid(bytes(100)))[1], bytes(100))

    def test_in_flight_retrieval_uses_current_cache_budget(self):
        content = bytes(100)
        def shrinking_provider(*args):
            config = dict(self.resolver.config); config["cache_bytes"] = 64
            self.resolver.configure(config)
            return content
        with patch("t256_helper.resolver.retrieve", side_effect=shrinking_provider):
            self.assertEqual(self.resolver.resolve("t256:" + compute_cid(content))[1], content)
        self.assertEqual(self.resolver.cache_stats()["entries"], 0)


if __name__ == "__main__":
    unittest.main()
