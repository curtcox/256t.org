import json
from pathlib import Path
import re
import tempfile
import threading
import unittest
from urllib.request import Request, urlopen

from t256_helper.i18n import LANGUAGES, LOCALES, catalog, negotiate, translate_html
from t256_helper.resolver import Resolver
from t256_helper.server import Gateway

WEB = Path(__file__).resolve().parents[1] / "web"
EXTENSION = Path(__file__).resolve().parents[1] / "extensions" / "shared"


class NegotiationTests(unittest.TestCase):
    def test_accept_language_picks_supported_language(self):
        for header, expected in [
            ("zh-CN,zh;q=0.9,en;q=0.8", "zh-Hans"),
            ("zh-Hans-SG", "zh-Hans"),
            ("pt-BR,pt;q=0.9", "pt-BR"),
            ("pt-PT", "pt-BR"),
            ("fr-FR, en;q=0.5", "en"),
            ("en;q=0.5, pt-BR;q=0.9", "pt-BR"),
            ("zh-TW", "en"),
            ("pt;q=0, zh", "zh-Hans"),
            ("", "en"),
            (None, "en"),
        ]:
            self.assertEqual(negotiate(header), expected, header)


class CatalogTests(unittest.TestCase):
    def test_catalogs_share_keys_and_placeholders(self):
        english = set(catalog(LANGUAGES[0]))
        for language in LANGUAGES:
            messages = catalog(language)
            self.assertEqual(set(messages), english, language)
            for key, value in messages.items():
                self.assertEqual(sorted(re.findall(r"\{(\w+)\}", key)), sorted(re.findall(r"\{(\w+)\}", value)), key)

    def test_page_text_is_in_the_catalog(self):
        messages = catalog("zh-Hans")
        for page in ("settings.html", "demo.html"):
            translated = translate_html((WEB / page).read_text(encoding="utf-8"), "zh-Hans")
            body = re.sub(r"<(script|style)\b.*?</\1>", "", translated, flags=re.S)
            for text in re.findall(r">([^<>]+)<", body):
                text = text.strip()
                # Anything left with an English word is untranslated, except
                # product names and examples that are the same in every language.
                if re.search(r"[A-Za-z]{3,}", text) and not re.search(r"[一-鿿]", text):
                    self.assertIn(text, ("t256 URI", "GitHub API URL"), page)
        self.assertIn("Add provider", messages)

    def test_script_strings_are_in_the_catalog(self):
        messages = catalog("pt-BR")
        for script in ("settings.mjs", "demo.js"):
            source = (WEB / script).read_text(encoding="utf-8")
            for key in re.findall(r"\bt\('((?:[^'\\]|\\.)*)'", source):
                key = key.replace("\\n", "\n")
                self.assertIn(key, messages, script)

    def test_extension_locales_cover_popup(self):
        locales = {path.parent.name: json.loads(path.read_text(encoding="utf-8"))
                   for path in (EXTENSION / "_locales").glob("*/messages.json")}
        self.assertEqual(set(locales), {"en", "zh_CN", "pt_BR"})
        keys = set(locales["en"])
        for name, messages in locales.items():
            self.assertEqual(set(messages), keys, name)
        used = set(re.findall(r'data-i18n(?:-placeholder)?="(\w+)"', (EXTENSION / "popup.html").read_text()))
        for script in ("popup.js", "background.js"):
            used |= set(re.findall(r"msg\('(\w+)'", (EXTENSION / script).read_text()))
        used |= set(re.findall(r"__MSG_(\w+)__", (EXTENSION / "manifest.json").read_text()))
        self.assertTrue(used)
        self.assertLessEqual(used, keys)


class TranslateHtmlTests(unittest.TestCase):
    def test_text_attributes_and_language(self):
        page = '<html lang="en"><head><title>Storage limits</title><style>p>b{}</style></head>' \
               '<body><label> Name <input placeholder="Folder path, base URL, or owner/repository"></label>' \
               '<script>const x = "Name";</script><p>Unlisted text</p></body></html>'
        translated = translate_html(page, "pt-BR")
        self.assertIn('<html lang="pt-BR">', translated)
        self.assertIn("<title>Limites de armazenamento</title>", translated)
        self.assertIn("<label> Nome <input", translated)
        self.assertIn('placeholder="Caminho da pasta, URL base ou owner/repository"', translated)
        self.assertIn('const x = "Name";', translated)
        self.assertIn("<style>p>b{}</style>", translated)
        self.assertIn("<p>Unlisted text</p>", translated)
        self.assertEqual(translate_html(page, "en"), page)


class GatewayLanguageTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.server = Gateway(Resolver(Path(self.temp.name) / "state"), 0)
        self.thread = threading.Thread(target=self.server.serve_forever, daemon=True)
        self.thread.start()

    def tearDown(self):
        self.server.shutdown(); self.server.server_close(); self.thread.join()
        self.temp.cleanup()

    def get(self, path, language=None):
        headers = {"Accept-Language": language} if language else {}
        return urlopen(Request(self.server.origin + path, headers=headers), timeout=5)

    def test_settings_page_follows_accept_language(self):
        english = self.get("/")
        self.assertIn("Content providers", english.read().decode())
        self.assertEqual(english.headers["Content-Language"], "en")
        chinese = self.get("/", "zh-CN,zh;q=0.9")
        page = chinese.read().decode()
        self.assertIn('<html lang="zh-Hans">', page)
        self.assertIn("内容提供方", page)
        self.assertEqual(chinese.headers["Content-Language"], "zh-Hans")
        self.assertIn("Accept-Language", chinese.headers.get_all("Vary"))
        self.assertIn("Conteúdo verificado no seu navegador", self.get("/demo.html", "pt-BR").read().decode())

    def test_scripts_and_catalogs_are_served(self):
        self.assertIn("export async function translator", self.get("/i18n.mjs").read().decode())
        for language in LANGUAGES:
            for path in ("/locales/" + language + ".json", "/browser-support/web/locales/" + language + ".json"):
                response = self.get(path)
                self.assertEqual(response.headers["Content-Type"], "application/json; charset=utf-8")
                self.assertEqual(json.load(response), json.loads((LOCALES / (language + ".json")).read_text(encoding="utf-8")))
        self.assertIn("export async function translator", self.get("/browser-support/web/i18n.mjs").read().decode())


if __name__ == "__main__":
    unittest.main()
