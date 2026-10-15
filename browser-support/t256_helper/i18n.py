"""Translated helper pages, chosen from the browser's Accept-Language.

The catalogs in web/locales/<code>.json are shared with web/i18n.mjs:
English text is the key. Pages are translated before they are sent so the
English text never flashes; scripts translate their own messages.
"""

import html
import json
from pathlib import Path
import re

LOCALES = Path(__file__).resolve().parents[1] / "web" / "locales"
LANGUAGES = ("zh-Hans", "pt-BR")


def negotiate(accept_language):
    """The best supported language for an Accept-Language header ("en" if none)."""
    ranked = []
    for position, part in enumerate((accept_language or "").split(",")):
        tag, _, parameters = part.strip().partition(";")
        quality = 1.0
        match = re.search(r"q=([0-9.]+)", parameters)
        if match:
            try:
                quality = float(match.group(1))
            except ValueError:
                quality = 0.0
        if tag and quality > 0:
            ranked.append((-quality, position, tag.lower()))
    for _, _, tag in sorted(ranked):
        if tag in ("zh", "zh-cn", "zh-sg") or tag.startswith("zh-hans"):
            return "zh-Hans"
        if tag == "pt" or tag.startswith("pt-"):
            return "pt-BR"
        if tag == "en" or tag.startswith("en-"):
            return "en"
    return "en"


def catalog(language):
    if language not in LANGUAGES:
        return {}
    try:
        return json.loads((LOCALES / (language + ".json")).read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return {}


_TEXT = re.compile(r"(>)([^<>]+)(<)")
_ATTRIBUTE = re.compile(r'\b(placeholder|title|aria-label|alt)="([^"]*)"')
_RAW = re.compile(r"(<(script|style)\b.*?</\2>)", re.S | re.I)


def translate_html(page, language):
    """Translate text nodes and labelling attributes found in the catalog."""
    messages = catalog(language)
    if not messages:
        return page

    def text(match):
        value = match.group(2)
        key = value.strip()
        if key not in messages:
            return match.group(0)
        start = value[:len(value) - len(value.lstrip())]
        end = value[len(value.rstrip()):]
        return match.group(1) + start + html.escape(messages[key], quote=False) + end + match.group(3)

    def attribute(match):
        key = match.group(2)
        if key not in messages:
            return match.group(0)
        return match.group(1) + '="' + html.escape(messages[key]) + '"'

    pieces = _RAW.split(page)
    out = []
    # re.split with two groups yields: text, whole raw block, tag name, text, ...
    for index in range(0, len(pieces), 3):
        out.append(_ATTRIBUTE.sub(attribute, _TEXT.sub(text, pieces[index])))
        if index + 1 < len(pieces):
            out.append(pieces[index + 1])
    result = "".join(out)
    return re.sub(r'<html lang="en"', '<html lang="' + language + '"', result, count=1)
