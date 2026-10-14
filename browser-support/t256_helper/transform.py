"""Transform verified representations, preserving original bytes in the CID cache.

Only URL-bearing HTML attributes, CSS url()/@import, and import-map values are
rewritten. JavaScript source is never rewritten or evaluated by the helper.
"""

import html
from html.parser import HTMLParser
import json
import re

from .uri import parse_uri


URI_PATTERN = re.compile(r"(?i:t256):[A-Za-z0-9_-]{8,94}(?:\.[A-Za-z0-9]+)*")


def replace_uri(value, make_url, navigation=False):
    try:
        return make_url(parse_uri(value), navigation)
    except ValueError:
        return value


def css_urls(source, make_url):
    # Tokenize comments/strings before matching URL tokens. A quoted string in
    # an ordinary property (e.g. content:) is not a resource reference.
    token = re.compile(r'/\*[\s\S]*?\*/|"(?:\\.|[^"\\])*"|\'(?:\\.|[^\'\\])*\'|url\(\s*(?:"(?:\\.|[^"\\])*"|\'(?:\\.|[^\'\\])*\'|[^)]*)\s*\)|@import\s+(?:"(?:\\.|[^"\\])*"|\'(?:\\.|[^\'\\])*\')', re.I)
    def rewrite(match):
        text = match.group()
        if text.lower().startswith("url("):
            value = text[4:-1].strip()
            if value[:1] in ('"', "'") and value[-1:] == value[:1]:
                value = value[1:-1]
            replacement = replace_uri(value, make_url)
            return 'url("' + replacement + '")' if replacement != value else text
        if text.lower().startswith("@import"):
            offset = text.find('"') if '"' in text else text.find("'")
            value = text[offset + 1:-1]
            return text[:offset] + '"' + replace_uri(value, make_url) + '"'
        return text
    return token.sub(rewrite, source)


def import_map(value, make_url):
    data = json.loads(value)
    for key in ("imports",):
        for specifier, target in data.get(key, {}).items():
            if isinstance(target, str):
                data[key][specifier] = replace_uri(target, make_url)
    for mappings in data.get("scopes", {}).values():
        for specifier, target in mappings.items():
            if isinstance(target, str):
                mappings[specifier] = replace_uri(target, make_url)
    return json.dumps(data).replace("<", "\\u003c")


class PageTransformer(HTMLParser):
    def __init__(self, make_url):
        super().__init__(convert_charrefs=False)
        self.make_url = make_url
        self.output = []
        self.raw_tag = None
        self.importmap = False
        self.buffer = []

    def start(self, tag, attrs, closed):
        attributes = dict(attrs)
        rendered = []
        for key, value in attrs:
            if value is not None:
                if key in ("src", "href", "poster", "data", "action"):
                    value = replace_uri(value, self.make_url, tag in ("a", "iframe", "frame") or key == "action")
                elif key == "srcset":
                    value = URI_PATTERN.sub(lambda match: replace_uri(match.group(), self.make_url), value)
                elif key == "style":
                    value = css_urls(value, self.make_url)
                rendered.append(key + '="' + html.escape(value, quote=True) + '"')
            else:
                rendered.append(key)
        self.output.append("<" + tag + (" " + " ".join(rendered) if rendered else "") + (" />" if closed else ">"))
        if tag in ("script", "style") and not closed:
            self.raw_tag = tag
            self.importmap = tag == "script" and attributes.get("type", "").lower() == "importmap"
            self.buffer = []

    def handle_starttag(self, tag, attrs):
        self.start(tag, attrs, False)

    def handle_startendtag(self, tag, attrs):
        self.start(tag, attrs, True)

    def handle_endtag(self, tag):
        if tag == self.raw_tag:
            value = "".join(self.buffer)
            if tag == "style":
                value = css_urls(value, self.make_url)
            elif self.importmap:
                value = import_map(value, self.make_url)
            self.output.append(value)
            self.raw_tag = None
        self.output.append("</" + tag + ">")

    def append(self, value):
        (self.buffer if self.raw_tag else self.output).append(value)

    def handle_data(self, data):
        self.append(data)

    def handle_entityref(self, name):
        self.append("&" + name + ";")

    def handle_charref(self, name):
        self.append("&#" + name + ";")

    def handle_comment(self, data):
        self.append("<!--" + data + "-->")

    def handle_decl(self, declaration):
        self.output.append("<!" + declaration + ">")


def transform_page(source, make_url):
    parser = PageTransformer(make_url)
    parser.feed(source)
    parser.close()
    if parser.raw_tag:
        parser.output.extend(parser.buffer)
    return "".join(parser.output)
