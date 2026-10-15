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
  <folder>/README.md        whole-document translations of README.md,
  <folder>/FAQ.md             FAQ.md and PUBLISHING.md
  <folder>/PUBLISHING.md
  <folder>/strings.json     string tables for the HTML tools and their scripts
```

### Documents

`README.md`, `FAQ.md` and `PUBLISHING.md` are translated as whole Markdown files
and rendered with the same template the deploy workflow uses. Keep links exactly
as they are in the English file (`hash.html`, `examples/256t.png`, `cids/`):
the build points them at the translated page when one exists, and at the shared
English asset otherwise.

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
