"""Transform verified representations, preserving original bytes in the CID cache.

Only URL-bearing HTML attributes, CSS url()/@import, and import-map values are
rewritten. JavaScript source is never rewritten or evaluated by the helper.
"""

import html
from html.parser import HTMLParser
import json
import re

from .uri import parse_uri


CSS_WHITESPACE = " \t\n\r\f"
CSS_ESCAPE = r'\\(?:[0-9a-fA-F]{1,6}(?:\r\n|[ \t\n\r\f])?|[^0-9a-fA-F\r\n\f])'
CSS_STRING = r'"(?:\\(?:\r\n|[\s\S])|[^"\\\r\n\f])*"|\'(?:\\(?:\r\n|[\s\S])|[^\'\\\r\n\f])*\''


def css_keyword(word):
    # These ASCII keywords contain no a-f letters, so single-character escapes
    # are unambiguous alongside one-to-six-digit hexadecimal escapes.
    return "".join(r"(?:" + char + r"|\\" + char + r"|\\0{0,4}(?:" + format(ord(char), "x") + "|" + format(ord(char.upper()), "x") + r")(?:\r\n|[ \t\n\r\f])?)" for char in word)


CSS_TOKEN = re.compile(
    r'/\*[\s\S]*?(?:\*/|$)|' + CSS_STRING +
    r'|(?<![\w\\@#-])' + css_keyword("url") + r'\([ \t\r\n\f]*(?P<url_value>' + CSS_STRING +
    r'|(?:' + CSS_ESCAPE + r'|[^()"\'\\\s\x00-\x1f\x7f])*)[ \t\r\n\f]*\)' +
    r'|@' + css_keyword("import") + r'(?:[ \t\r\n\f]|/\*[\s\S]*?\*/)*(?P<import_value>' + CSS_STRING + ')', re.I)


def css_unescape(value, quoted=False):
    def decode(match):
        digits, newline, char = match.groups()
        if digits is not None:
            point = int(digits, 16)
            return chr(point) if 0 < point <= 0x10ffff and not 0xd800 <= point <= 0xdfff else "\ufffd"
        if newline is not None:
            return "" if quoted else match.group()
        return char
    return re.sub(r'\\(?:([0-9a-fA-F]{1,6})(?:\r\n|[ \t\n\r\f])?|(\r\n|[\r\n\f])|([\s\S]))', decode, value)


def replace_uri(value, make_url, navigation=False):
    try:
        return make_url(parse_uri(value), navigation)
    except ValueError:
        return value


def css_urls(source, make_url):
    # Tokenize comments/strings before matching URL tokens. A quoted string in
    # an ordinary property (e.g. content:) is not a resource reference.
    def rewrite(match):
        text = match.group()
        raw = match.group("url_value") if match.group("url_value") is not None else match.group("import_value")
        if raw is None:
            return text
        quoted = raw[:1] in ('"', "'")
        value = css_unescape(raw[1:-1] if quoted else raw, quoted)
        replacement = replace_uri(value, make_url)
        if replacement == value:
            return text
        escaped = replacement.replace("\\", "\\\\").replace('"', '\\"')
        if match.group("url_value") is not None:
            return 'url("' + escaped + '")'
        offset = match.start("import_value") - match.start()
        return text[:offset] + '"' + escaped + '"'
    return CSS_TOKEN.sub(rewrite, source)


def srcset_urls(source, make_url):
    """Rewrite complete candidate URLs, preserving data URLs and descriptors.

    Follow HTML's candidate boundaries: commas can occur inside URL tokens, and
    parentheses protect commas in descriptors. Never replace an identifier prefix.
    """
    output, position = [], 0
    while position < len(source):
        start = position
        while position < len(source) and source[position] in CSS_WHITESPACE + ",":
            position += 1
        output.append(source[start:position])
        start = position
        while position < len(source) and source[position] not in CSS_WHITESPACE:
            position += 1
        end = position
        while end > start and source[end - 1] == ",":
            end -= 1
        output.append(replace_uri(source[start:end], make_url))
        output.append(source[end:position])
        if end != position:
            continue
        start, depth = position, 0
        while position < len(source):
            char = source[position]; position += 1
            if char == "(": depth += 1
            elif char == ")": depth = max(0, depth - 1)
            elif char == "," and depth == 0: break
        output.append(source[start:position])
    return "".join(output)


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
                elif key in ("srcset", "imagesrcset"):
                    value = srcset_urls(value, self.make_url)
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
