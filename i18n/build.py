#!/usr/bin/env python3
"""Generate the translated copies of the 256t.org site.

Sources (edit these):
  i18n/languages.json        languages, their folder, native name and notice
  i18n/pages.json            which English files are translated
  i18n/<lang>/README.md ...  whole-document translations of the Markdown pages
  i18n/<lang>/strings.json   per-page "English snippet" -> "translation" tables
                             for the HTML tool pages and their scripts

Output (generated, committed, deployed as-is):
  <lang>/index.html, <lang>/faq.html, <lang>/hash.html, ...

The English pages are only touched between their
<!-- i18n:languages --> ... <!-- /i18n:languages --> markers, which this
script fills with links to the other languages.

Usage:
  python3 i18n/build.py          regenerate everything
  python3 i18n/build.py --check  exit 1 if anything is stale or missing

Requires the "markdown" package (pip install markdown), the same one the
deploy workflows use for the English Markdown pages.
"""

import argparse
import hashlib
import html
import json
import posixpath
import re
import sys
from html.parser import HTMLParser
from pathlib import Path

import markdown

ROOT = Path(__file__).resolve().parent.parent
I18N = ROOT / 'i18n'

START = '<!-- i18n:languages -->'
END = '<!-- /i18n:languages -->'
BLOCK = re.compile(re.escape(START) + r'.*?' + re.escape(END), re.S)

# First line of a translated document: which English version it translates.
SOURCE_STAMP = re.compile(r'\A<!-- i18n:source sha256:([0-9a-f]+) -->\n+')

MARKDOWN_EXTENSIONS = ['fenced_code', 'tables', 'toc', 'sane_lists']

# A quoted relative path such as "hash.html", './loader.mjs', `cids/${cid}`
# or "browser-support/README.md#plan". Used for HTML attributes and script
# string literals alike, so translated pages keep pointing at shared assets.
REFERENCE = re.compile(
    r"""(?<=["'`])((?:\.{1,2}/)*\.?[A-Za-z0-9_][A-Za-z0-9_.\-]*(?:/[A-Za-z0-9_.\-]+)*/?)(?=["'`#?$])"""
)

# Code samples in documents are shown verbatim, never rewritten.
CODE = re.compile(r'<pre\b.*?</pre>|<code\b.*?</code>', re.S)

HEADING_ID = re.compile(r'(<h[1-6] id=")([^"]*)(")')


def load_json(path):
    return json.loads(path.read_text(encoding='utf-8'))


LANGUAGES = load_json(I18N / 'languages.json')
PAGES = load_json(I18N / 'pages.json')
ENGLISH = LANGUAGES[0]
TRANSLATIONS = LANGUAGES[1:]


def document(source):
    entry = PAGES['documents'].get(source)
    if entry is None:
        return None
    return {'output': entry} if isinstance(entry, str) else entry


def output_name(source):
    """Path of a translated copy inside a language folder (README.md -> index.html)."""
    entry = document(source)
    return entry['output'] if entry else source


def english_path(source):
    """Where the English original is served: a site path or an absolute URL."""
    entry = document(source)
    return entry.get('english', entry['output']) if entry else source


def is_url(path):
    return re.match(r'^[a-z][a-z0-9+.\-]*:', path, re.I) is not None


def all_sources():
    return list(PAGES['documents']) + PAGES['pages'] + PAGES['scripts']


def localized(language, site_path):
    return posixpath.join(language['path'], site_path) if language['path'] else site_path


def relative(target, from_file):
    if is_url(target):
        return target
    return posixpath.relpath(target, posixpath.dirname(from_file) or '.')


def sha256(text):
    return hashlib.sha256(text.encode('utf-8')).hexdigest()[:16]


class Problems(list):
    def add(self, message):
        self.append(message)
        print(f'  ! {message}', file=sys.stderr)


problems = Problems()
warnings = []


def warn(message):
    warnings.append(message)
    print(f'  ~ {message}', file=sys.stderr)


# ---------------------------------------------------------------------------
# Language links

def page_path(source, language):
    if language is ENGLISH:
        return english_path(source)
    return localized(language, output_name(source))


def language_block(source, current, markdown_style=False):
    """The language switcher (and, off English, the translation notice)."""
    here = page_path(source, current)
    links = []
    for language in LANGUAGES:
        if language is current:
            links.append((language, None))
        else:
            links.append((language, relative(page_path(source, language), here)))

    if markdown_style:
        parts = [f'**{language["name"]}**' if href is None else f'[{language["name"]}]({href})'
                 for language, href in links]
        return f'{START}\n\n{" · ".join(parts)}\n\n{END}'

    parts = []
    for language, href in links:
        name = html.escape(language['name'])
        if href is None:
            parts.append(f'<strong lang="{language["code"]}">{name}</strong>')
        else:
            parts.append(f'<a href="{href}" hreflang="{language["code"]}" lang="{language["code"]}">{name}</a>')
    nav = (f'<nav class="i18n-languages" aria-label="{html.escape(current["label"])}" '
           f'style="display: block; font-size: 0.9rem;">{" · ".join(parts)}</nav>')
    notice = ''
    if current is not ENGLISH:
        original = relative(english_path(source), here)
        notice = ('\n<p class="i18n-notice" style="font-size: 0.9rem; opacity: 0.8; margin: 0.25rem 0 0;">'
                  + current['notice'].replace('{original}', original) + '</p>')
    return f'{START}\n{nav}{notice}\n{END}'


def update_english(source, write):
    entry = document(source)
    if entry and entry.get('markers') is False:
        return
    path = ROOT / source
    text = path.read_text(encoding='utf-8')
    if not BLOCK.search(text):
        problems.add(f'{source}: missing {START} ... {END} markers')
        return
    block = language_block(source, ENGLISH, markdown_style=source.endswith('.md'))
    write(path, BLOCK.sub(lambda _: block, text, count=1))


# ---------------------------------------------------------------------------
# Relative references

def exists_for_site(site_path, is_directory):
    if site_path.startswith('.github/'):
        return False  # not deployed
    for generated in PAGES['deploy_generated']:
        if site_path == generated.rstrip('/') or site_path.startswith(generated):
            return True
    target = ROOT / site_path
    if is_directory:
        return target.is_dir()
    # A bare word like 'examples' is far more likely to be ordinary text
    # than a link, so only files with an extension count.
    return target.is_file() and ('/' in site_path or '.' in site_path)


def translated_paths(language):
    """English source paths and site paths -> path of the translated copy."""
    paths = {}
    for source in all_sources():
        translated = localized(language, output_name(source))
        paths[source] = translated
        paths[output_name(source)] = translated
        if not is_url(english_path(source)):
            paths[english_path(source)] = translated
    return paths


def rewrite_references(text, source, language, skip_code=False):
    """Point relative paths at the translated page when there is one,
    otherwise back at the shared English asset."""
    out = localized(language, output_name(source))
    translated = translated_paths(language)

    def replace(match):
        reference = match.group(1)
        is_directory = reference.endswith('/')
        site_path = posixpath.normpath(posixpath.join(posixpath.dirname(source), reference))
        if site_path.startswith('..') or site_path == '.':
            return reference
        if site_path in translated:
            target = translated[site_path]
        elif exists_for_site(site_path, is_directory):
            target = site_path
        else:
            return reference
        result = relative(target, out) + ('/' if is_directory else '')
        if reference.startswith('./') and not result.startswith('.'):
            result = './' + result
        return result

    if not skip_code:
        return REFERENCE.sub(replace, text)
    pieces, last = [], 0
    for code in CODE.finditer(text):
        pieces.append(REFERENCE.sub(replace, text[last:code.start()]))
        pieces.append(code.group(0))
        last = code.end()
    pieces.append(REFERENCE.sub(replace, text[last:]))
    return ''.join(pieces)


class LinkCollector(HTMLParser):
    def __init__(self):
        super().__init__()
        self.links = []

    def handle_starttag(self, tag, attrs):
        for name, value in attrs:
            if name in ('href', 'src') and value:
                self.links.append(value)


def check_links(text, out):
    collector = LinkCollector()
    collector.feed(text)
    translated_outputs = {localized(language, output_name(s))
                          for language in TRANSLATIONS for s in all_sources()}
    translated_outputs |= {english_path(s) for s in all_sources()}
    for link in collector.links:
        if is_url(link) or re.match(r'^#|^//|\$\{', link):
            continue
        path = link.split('#')[0].split('?')[0]
        if not path:
            continue
        site_path = posixpath.normpath(posixpath.join(posixpath.dirname(out), path))
        if site_path in translated_outputs or exists_for_site(site_path, path.endswith('/')):
            continue
        problems.add(f'{out}: link to missing {link}')


# ---------------------------------------------------------------------------
# Generation

def page_template(language, title, body):
    """Mirrors generate_html() in .github/workflows/deploy-pages.yml."""
    return f'''<!DOCTYPE html>
<!-- Generated by i18n/build.py. Edit the sources in i18n/ instead. -->
<html lang="{language['code']}"{direction(language)}>
<head>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <title>{html.escape(title)}</title>
  <style>
    :root {{
      color-scheme: light dark;
      font-family: system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
      line-height: 1.6;
    }}
    body {{
      margin: 0 auto;
      padding: 2rem clamp(2rem, 5vw, 8rem) 4rem;
      max-width: 140ch;
    }}
    pre {{
      padding: 1rem;
      overflow-x: auto;
      border-radius: 0.5rem;
      background: rgba(0, 0, 0, 0.05);
    }}
    code {{
      font-family: "JetBrains Mono", "Fira Code", "SFMono-Regular", Consolas, "Liberation Mono", Menlo, monospace;
    }}
    table {{
      border-collapse: collapse;
      width: 100%;
      overflow: hidden;
    }}
    th, td {{
      border: 1px solid rgba(125, 125, 125, 0.4);
      padding: 0.5rem;
      text-align: left;
    }}
    thead {{
      background: rgba(0, 0, 0, 0.08);
    }}
    a {{
      color: inherit;
    }}
    a:hover, a:focus {{
      text-decoration: underline;
    }}
    img {{
      max-width: 100%;
      height: auto;
    }}
    p[align="center"] {{
      text-align: center;
    }}
  </style>
</head>
<body>
{START}
{END}
{body}
</body>
</html>
'''


def direction(language):
    return f' dir="{language["direction"]}"' if language.get('direction') else ''


def document_title(source_markdown):
    match = re.search(r'^#\s+(.+)$', source_markdown, re.M)
    heading = re.sub(r'\[([^\]]*)\]\([^)]*\)', r'\1', match.group(1)).strip() if match else '256t.org'
    heading = heading.replace('`', '')
    return heading if heading == '256t.org' else f'{heading} - 256t.org'


def english_markdown(source):
    return BLOCK.sub('', (ROOT / source).read_text(encoding='utf-8'))


def build_document(source, language):
    translation = I18N / language['path'] / source
    if not translation.exists():
        problems.add(f'{translation.relative_to(ROOT)}: missing translation')
        return None
    text = translation.read_text(encoding='utf-8')
    english = english_markdown(source)

    stamp = SOURCE_STAMP.match(text)
    current = sha256(english)
    if not stamp:
        problems.add(f'{translation.relative_to(ROOT)}: first line must be <!-- i18n:source sha256:{current} -->')
    elif stamp.group(1) != current:
        warn(f'{translation.relative_to(ROOT)}: {source} changed since it was translated; '
             f'update the translation, then its stamp to sha256:{current}')
    text = SOURCE_STAMP.sub('', text, count=1)

    body = markdown.markdown(text, extensions=MARKDOWN_EXTENSIONS)
    # Keep the English anchors so links like README.md#plan work in every language.
    english_ids = HEADING_ID.findall(markdown.markdown(english, extensions=MARKDOWN_EXTENSIONS))
    translated_ids = HEADING_ID.findall(body)
    if len(english_ids) == len(translated_ids):
        ids = iter(match[1] for match in english_ids)
        body = HEADING_ID.sub(lambda m: m.group(1) + next(ids) + m.group(3), body)
    else:
        problems.add(f'{translation.relative_to(ROOT)}: {len(translated_ids)} headings, '
                     f'English {source} has {len(english_ids)}')

    page = page_template(language, document_title(text), body)
    return rewrite_references(page, source, language, skip_code=True)


def apply_strings(text, source, table):
    for english in sorted(table, key=len, reverse=True):
        if english not in text:
            problems.add(f'{source}: string no longer in the English page: {english!r}')
            continue
        text = text.replace(english, table[english])
    return text


def build_page(source, language, strings):
    text = (ROOT / source).read_text(encoding='utf-8')
    is_html = source.endswith('.html')
    if is_html and not BLOCK.search(text):
        problems.add(f'{source}: missing {START} ... {END} markers')
        return None
    # Keep the English language links out of the string replacement.
    text = BLOCK.sub(START + END, text, count=1)
    table = strings.get(source)
    if table is None:
        problems.add(f'i18n/{language["path"]}/strings.json: no entry for {source}')
        table = {}
    text = apply_strings(text, source, table)
    text = rewrite_references(text, source, language)
    if is_html:
        text = re.sub(r'<html lang="en"', f'<html lang="{language["code"]}"{direction(language)}', text, count=1)
        text = re.sub(r'(<!doctype html>)', r'\1\n<!-- Generated by i18n/build.py. Edit the sources in i18n/ instead. -->',
                      text, count=1, flags=re.I)
    else:
        text = '// Generated by i18n/build.py. Edit the sources in i18n/ instead.\n' + text
    return text


def finish(text, source, language):
    if text is None:
        return None
    block = language_block(source, language)
    text = BLOCK.sub(lambda _: block, text, count=1)
    out = localized(language, output_name(source))
    if out.endswith('.html'):
        check_links(text, out)
    return text


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--check', action='store_true', help='report stale output instead of writing it')
    args = parser.parse_args()

    stale = []

    def write(path, text):
        current = path.read_text(encoding='utf-8') if path.exists() else None
        if current == text:
            return
        stale.append(path.relative_to(ROOT))
        if not args.check:
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(text, encoding='utf-8')

    for source in list(PAGES['documents']) + PAGES['pages']:
        update_english(source, write)

    for language in TRANSLATIONS:
        print(f'{language["code"]} -> {language["path"]}/')
        strings_path = I18N / language['path'] / 'strings.json'
        strings = load_json(strings_path) if strings_path.exists() else {}
        for source in PAGES['documents']:
            text = finish(build_document(source, language), source, language)
            if text is not None:
                write(ROOT / localized(language, output_name(source)), text)
        for source in PAGES['pages'] + PAGES['scripts']:
            text = finish(build_page(source, language, strings), source, language)
            if text is not None:
                write(ROOT / localized(language, output_name(source)), text)

    verb = 'stale' if args.check else 'updated'
    for path in stale:
        print(f'  {verb}: {path}')
    if warnings:
        print(f'{len(warnings)} translation(s) may be out of date', file=sys.stderr)
    if problems:
        print(f'{len(problems)} problem(s) found', file=sys.stderr)
    if problems or (args.check and stale):
        sys.exit(1)


if __name__ == '__main__':
    main()
