# Translations

The deployed site has translated copies of its pages, one folder per language:

| Language | Folder | URL |
|----------|--------|-----|
| 简体中文 (Simplified Chinese) | [`zh-hans/`](../zh-hans/) | `/zh-hans/` |
| Português (Brasil) | [`pt-br/`](../pt-br/) | `/pt-br/` |

Those folders are **generated** by [`build.py`](build.py) and committed, so the
existing deploy workflows publish them unchanged. Edit the sources here instead.

## Layout

```
i18n/
  languages.json            languages: code, folder, native name, notice text
  pages.json                which English files are translated
  build.py                  generator
  <folder>/<path>.md        whole-document translations, at the English file's
                            path (README.md, browser-support/README.md, ...)
  <folder>/strings.json     string tables for the HTML tools and their scripts
```

### Documents

The Markdown pages listed under `documents` in `pages.json` (the home page, FAQ,
Publishing, the browser-support guide, the RFC draft and the R2 scripts guide)
are translated as whole files and rendered with the same template the deploy
workflow uses. Keep links exactly as they are in the English file (`hash.html`,
`examples/256t.png`, `../resolve.html`): the build points them at the translated
page when one exists, and at the shared English asset otherwise. Code samples
are left untouched. Headings get the English anchor ids, so links such as
`browser-support/README.md#active-implementation-plan` work in every language;
keep the same headings, in the same order, as the English file.

The first line of each translation records which English version it translates:

```
<!-- i18n:source sha256:8fa34a8ffffd587a -->
```

When the English file changes, the build prints a warning naming the stale
translation and the new stamp. Update the translation, then the stamp.

A `documents` entry is either the translated page's path (`"FAQ.md": "faq.html"`)
or an object with `output`, plus `english` when the English original is served
somewhere else (a raw `.md` path or a URL) and `"markers": false` when the
English file is not on the site and gets no language links.

Avoid hard line breaks inside Chinese (and other CJK) paragraphs: browsers
render them as stray spaces.

### Tool pages

The HTML tools (`hash.html`, `check.html`, …) are not copied by hand. The build
takes the current English page and applies `strings.json`, which maps exact
English snippets to their translation, per page:

```json
{
  "hash.html": {
    "<h1>256t Text Hash</h1>": "<h1>256t 文本哈希</h1>",
    "`${length} byte${length === 1 ? '' : 's'}`": "`${length} 字节`"
  }
}
```

Snippets include enough surrounding markup or code to be unambiguous. Strings in
scripts are translated the same way, keeping their quotes and `${…}`
placeholders. An English string with no entry simply stays English.

Modules listed under `scripts` get translated copies too (the resolver page and
the library modules whose error messages it shows). A translated module imports
the translated copies of other listed modules and the shared English files for
everything else.

If an English page changes so that a snippet no longer appears, the build fails
and names it — update the key (and translation) in every `strings.json`.

## Language links

Every English page has a marker block:

```
<!-- i18n:languages -->
<!-- /i18n:languages -->
```

The build fills it with links to the other languages; the translated pages get
the same links plus a short notice that the page is machine-translated and the
English original is authoritative. Nothing else in the English pages is touched.

## Building

```bash
pip install markdown
python3 i18n/build.py
```

Run it after editing a translation **or any translated English page**, and
commit the result. `python3 i18n/build.py --check` exits non-zero if anything
is stale; the `i18n` workflow runs it on every push and pull request.

## Adding a language

1. Add an entry to `languages.json` — `code` is a BCP 47 tag (`es`, `ja`,
   `zh-Hant`, …), `path` is its folder (lowercase), and `notice` is the
   translated notice, with `{original}` where the link to the English page goes.
   Add `"direction": "rtl"` for right-to-left languages.
2. Create `i18n/<path>/` with translated `README.md`, `FAQ.md`, `PUBLISHING.md`
   and a `strings.json` (copy an existing one and translate the values).
3. Run `python3 i18n/build.py` and commit `i18n/<path>/`, the new `<path>/`
   folder, and the English pages whose language links changed.

## Adding a page

Add the English file to `pages.json` (`documents` for Markdown, `pages` for
HTML, `scripts` for a page-specific module whose strings need translating), add
the marker block to the English page, add its entries to each `strings.json` (or
a translated Markdown file to each language folder), and rebuild.

## Desktop helper and browser extension

The helper's settings and demo pages and the browser extension are not part of
the generated site; they translate themselves at runtime from the browser's
language preferences (English when nothing matches).

```
browser-support/web/locales/<code>.json      catalog: English text -> translation
browser-support/web/i18n.mjs                 translator used by the helper pages,
                                             the extension popup and the resolver
browser-support/t256_helper/i18n.py          Accept-Language negotiation and
                                             server-side translation of the pages
browser-support/extensions/shared/_locales/  the extension's own text (standard
                                             WebExtension messages.json files)
```

- **Catalog keys are the English text.** Variable parts use `{name}`
  placeholders, e.g. `"Edit {provider}": "编辑 {provider}"`. The same keys
  translate messages the helper sends back (configuration errors, provider
  diagnostics), so the helper's API and command line stay English.
- **Pages:** the helper translates `settings.html` and `demo.html` before
  sending them, so English never flashes; scripts call `t('…')` for text they
  create and `t.message(…)` for messages from the helper or the page loader.
- **Resolver:** the site resolver uses the catalog only for messages relayed
  from the desktop helper; its own text comes from `strings.json` above.
- **Extension:** popup and background text comes from `_locales/<locale>/messages.json`
  (`en`, `zh_CN`, `pt_BR`); `languageTag` in each file selects the catalog the
  popup uses for helper messages. `build_extensions.py` packages `i18n.mjs` and
  the catalogs with each extension.

To add a language, add `locales/<code>.json` with every key from an existing
catalog, add the code to `languages` in `i18n.mjs` and `LANGUAGES` in
`t256_helper/i18n.py` (plus its tags in both `negotiate` functions), and add an
extension `_locales/<locale>/messages.json`. `tests/test_i18n.py` checks that
catalogs and extension locales have the same keys and cover every string the
pages and scripts use.
