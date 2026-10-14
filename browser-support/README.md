# t256 desktop and browser support

The helper resolves `t256:<CID>[.suffix]` using user-selected content providers.
It verifies original bytes before serving them, and keeps settings separate from
content access. Python 3.9 or later is required; the helper has no third-party
runtime dependencies. Node.js and Playwright are needed only for browser tests.

## Start

Download `browser-support/dist/t256-desktop.zip` from the published URI resolver
and extract it, or clone this repository. Preserve the directory layout.

From the repository root:

```sh
python3 browser-support/run.py serve --open
```

The provider settings page lets you add, reorder, disable, remove, and index
providers. Hashbin's public content origin, `https://256t.us`, is selected initially.
Use `python` in place of `python3` on Windows if that is your Python command.
Keep this checkout in a stable location if you install desktop registration.

To open a URI, starting the helper automatically:

```sh
python3 browser-support/run.py open t256:AAAAAAAFaGVsbG8.txt
python3 browser-support/run.py settings
python3 browser-support/run.py stop
```

Settings live in `~/Library/Application Support/t256` on macOS,
`%LOCALAPPDATA%/t256` on Windows, or `$XDG_CONFIG_HOME/t256` (normally
`~/.config/t256`) on Linux. Override this with `--state DIRECTORY` **before** the
subcommand, or `T256_HOME`. Restart after editing `config.json` manually.

## Content providers

```sh
# Folder or mounted network drive; files named with their CID need no index.
python3 browser-support/run.py add documents folder /path/to/documents
python3 browser-support/run.py index documents

# HTTP(S) base URL: retrieval appends /<CID>, without a suffix.
python3 browser-support/run.py add public http https://example.org/cids

# GitHub: CID-named files in a directory, using the existing GitHub CLI login.
python3 browser-support/run.py add github github owner/repository --directory cids --ref main --use-gh

# Alternatively specify the NAME of a token environment variable, not its value.
python3 browser-support/run.py add private github owner/private-repository --token-env GH_TOKEN

# A clone uses committed content at the selected revision, including ordinary filenames.
# Clone/fetch using your existing Git/SSH credentials, then point the helper at it.
python3 browser-support/run.py add clone git /path/to/clone --directory '' --ref HEAD
python3 browser-support/run.py index clone

python3 browser-support/run.py disable public
python3 browser-support/run.py enable public
python3 browser-support/run.py resolve t256:AAAAAAAFaGVsbG8.txt --output hello.txt
```

Public, private, and organization repositories use the access granted to the user's
credentials. `--api-url https://github.example/api/v3` supports GitHub Enterprise.
API providers default to `cids/<CID>`. To use ordinary filenames remotely, import
a JSON object mapping full CIDs to relative paths:

```sh
python3 browser-support/run.py import-index github cid-paths.json
```

Indexes never establish trust: the returned bytes still have to match the CID.
Reindex after changing files or repository revisions. Local clones are not
automatically fetched. Mounted shares use the user's existing mount credentials.
Symlinks escaping selected roots are refused. Git indexing excludes symlinks and
submodules. HTTP redirects stay on the configured origin; cross-origin redirects
are refused, including for authenticated providers.

The default per-content limit is 64 MiB; the disk cache budget is 256 MiB.
These limits are editable in settings. Original objects are currently buffered in
memory; this version is intended for bounded desktop resources rather than
multi-gigabyte media. The cache is verified on every read and evicted by last use.
The gateway supports single HTTP byte ranges for original resources, allowing
audio/video seeking and partial downloads. The entire object is retrieved and
verified before any range is sent. Transformed HTML/CSS, malformed or multipart
ranges, and conditional ranges without a matching validator receive full responses.
Disabling a provider retains previously verified cached content. Clear the
settings directory's `cache` folder if you want to remove those copies.

## Browser integration choices

| Browser | Page loader / gateway | Optional extension | Desktop navigation |
|---|---|---|---|
| Chrome / Edge | Implemented; Chromium engine checks | Chromium package; native messaging | OS handler |
| Brave / Opera | Implemented; branded-browser validation pending | Chromium package; validation pending | OS handler |
| Firefox | Implemented; Firefox engine checks in CI | Firefox package; native messaging | OS handler |
| Safari on macOS | Implemented; WebKit engine checks | Safari Web Extension source; manual connection URL | macOS handler |

Engine tests do not establish installed-browser protocol prompts, address-bar
behavior, extension-store approval, or Safari signing. These require the manual
checks below. A browser extension does not register a general `t256:` network
scheme. Website/PWA registration cannot register bare `t256:` or `web+t256:`;
an optional letters-only alias remains a future compatibility choice.

### Cooperating pages

Grant the page's **exact origin** in helper settings, then copy its connection URL.
For example, a local development site might use `http://127.0.0.1:8080`.
Connections rotate on helper restart; revoke an origin in settings to reject
subsequent requests. A connection is a bearer capability: anyone holding it can
request known CIDs from your selected providers. Do not embed private connection
URLs in publicly distributed source or share links.

```html
<img data-t256-src="t256:YOUR_IMAGE_CID.png" alt="Content image">
<link rel="stylesheet" data-t256-href="t256:YOUR_STYLESHEET_CID.css">
<script type="application/t256" data-t256-src="t256:YOUR_FIRST_SCRIPT_CID.js"></script>
<script type="application/t256" data-t256-src="t256:YOUR_SECOND_SCRIPT_CID.js"></script>
<script type="module">
  import { T256Client } from './browser-support/web/loader.mjs';
  const client = new T256Client(connectionUrlSuppliedByTheUser);
  await client.hydrate(document);
  const observer = client.observe(document, { onError: console.error });
  // On teardown: observer.disconnect(); client.dispose();
</script>
```

Inert declarations avoid races with the browser's parser. Classic scripts run
sequentially in document order; they are verified again in the browser and loaded
as blobs, without `eval`. Repeated hydration does not rerun unchanged declarations.
Dynamic opt-in declarations can be handled with `observe()`.

For modules, supply the whole dependency graph before loading any modules:

Use `<script src="./browser-support/web/loader.js"></script>` for this path, then
access `window.T256Client`. Firefox requires its import map before the first module
load, including the loader itself. The ES module entry remains suitable for pages
with a static import map, classic scripts, images, or self-contained modules.

```js
await client.imports({
  't256:DEPENDENCY_CID.mjs': 't256:DEPENDENCY_CID.mjs',
  'named-dependency': 't256:ANOTHER_DEPENDENCY_CID.mjs',
});
await client.script('t256:ENTRY_CID.mjs', { module: true });
const bytes = await client.bytes('t256:DATA_CID.json', { signal });
const worker = await client.worker('t256:WORKER_CID.js');
```

Modules are served as original source through the verified gateway. Import maps
are explicit; JavaScript source and computed imports are not rewritten. Classic,
self-contained workers are supported; module-worker dependency graphs are not.
The opaque t256 syntax has no relative-reference semantics, so dependencies must
use complete identifiers or explicit import-map names.

Normal page CSP still applies: permit the loader's source, `blob:` for classic
scripts/images/workers, and the helper origin for module and stylesheet requests.
A CSP nonce can be supplied to loader methods. Browser local-network permission
and CORS checks are preserved. The helper's `/demo.html` is a runnable example.

### Gateway for existing pages

Open a `.html` URI with the helper or settings page. The gateway verifies the
original HTML, then transforms URL-bearing attributes, inline CSS, and import-map
values before the browser parses it. External CSS `url()` and quoted `@import`
references are also transformed. Nested HTML frames and t256 navigation links use
the gateway. Original JavaScript is not changed.

Transformed HTML and CSS must be UTF-8 and are served explicitly as UTF-8.

An existing page can use standard import maps with t256 targets:

```html
<script type="importmap">
{"imports":{"named-dependency":"t256:DEPENDENCY_CID.mjs"}}
</script>
<script type="module" src="t256:ENTRY_CID.mjs"></script>
```

Content documents have a sandboxed, opaque origin. They can run their selected
scripts but cannot read helper management settings or authenticate to providers.
Their CSP limits resource and connection destinations to the gateway. Verified
source bytes remain in the CID cache; generated HTML/CSS are labeled with
`X-T256-Representation: transformed` and `X-T256-Source-CID`, and are not claimed
to hash to that source CID.

### Extensions

```sh
python3 browser-support/build_extensions.py
```

Packages are created under `browser-support/dist/extensions`:

- Chrome, Edge, Brave, Opera: load the `chromium` directory as an unpacked extension.
- Firefox: load `firefox/manifest.json` temporarily for development. Persistent
  installations require Mozilla signing or an appropriate managed deployment.
- Safari: convert the `safari` directory with
  `xcrun safari-web-extension-converter`, then build/sign the containing app in
  Xcode. This package uses copied website connection URLs, not Python native
  messaging. The page loader/gateway also work without a Safari extension.

The extension uses `activeTab`; it does not automatically read every website.
“Adapt images and links” handles current image, poster, stylesheet, and navigation
references after an explicit click. It deliberately leaves existing script tags
alone; use the page loader or gateway for execution order and module support.

Chrome/Firefox native launch integration uses this per-user registration:

```sh
python3 browser-support/run.py install --browser chrome --extension-id YOUR_32_CHARACTER_EXTENSION_ID
python3 browser-support/run.py install --browser firefox --extension-id t256@256t.org
```

Use `edge`, `brave`, or `opera` for other native-host locations. The installed
native manifests allow only the specified extension. Context menus and the
`t256` address-bar keyword open content through the gateway. The keyword requires
the browser's keyword activation gesture; it does not make bare address-bar URIs
universally recognized.

### OS registration

```sh
python3 browser-support/run.py install
python3 browser-support/run.py uninstall
```

Registration uses an AppleScript `.app` on macOS, per-user URL protocol registry
keys on Windows, and a desktop entry plus `xdg-mime` on Linux. It does not require
administrator access. Python and this checkout must remain at their installed
paths. Uninstallation retains provider settings and cached bytes.

The macOS wrapper is locally ad-hoc signed. It is generated on installation and
does not require a developer signing identity. Safari extension distribution is a
separate signed-containing-app process.

## Public HTTPS fallback

The site's [URI resolver](../resolve.html) extracts inline content and verifies
bytes from public CORS-enabled providers without installation. Its optional
connection field uses the desktop resolver for selected private/local providers.
HTML and JavaScript previews are text, and shared links contain only the URI in
their fragment, never a helper capability or provider credential.

## Hashbin integration

Hashbin's local documentation specifies `https://256t.us/<CID>[.<ext>]` for public,
cookie-free bytes; `https://hashbin.org/api/content/<CID>` returns metadata.
The helper uses `https://256t.us/<CID>` and applies the suffix locally, including
compound suffixes. HTTP failures, including 404/451/429, participate in provider
fallback. Publishing/authentication APIs are outside this read-only resolver.

## Validation

These commands apply to the source checkout; the small downloadable runtime
package omits development dependencies and test fixtures.

```sh
PYTHONPATH=browser-support python3 -m unittest discover -s browser-support/tests -v
node --test browser-support/tests/loader.test.mjs
npm --prefix browser-support ci
browser-support/node_modules/.bin/playwright install chromium firefox webkit
npm --prefix browser-support run test:browsers
```

The dedicated workflow tests resolver/gateway/native framing on macOS, Windows,
and Linux and browser resources on Chromium, Firefox, and WebKit. Tests use local
providers, not live private repositories or credentials.

Manual release checks: clicked and pasted URIs; browser launch prompts; private
windows; provider grants/revocation; disconnected shares; GitHub login/SSO and
credential renewal; signed Safari extension; branded Brave/Opera; installation
and removal on each OS. Native transparent `fetch('t256:…')`, arbitrary dynamic
imports, CSS escape forms, SVG-internal resources, media ranges, and upstream
browser-engine implementation remain future work.
