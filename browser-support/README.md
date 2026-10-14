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

The provider settings page lets you add, edit, reorder, disable, remove, and index
providers, import/export file indexes, and view or clear cached content.
Hashbin's public content origin, `https://256t.us`, is selected initially.
**Check providers** verifies a pasted URI without opening it and lists provider
failures encountered before a match, or all failures if none matched. Diagnostics
use fixed descriptions and status codes; provider paths, credential values, raw
network errors, and GitHub CLI output are never included. The page loader exposes
the same structured diagnostics on `error.failures`, and the public resolver shows
helper failures and successful fallback details.
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
An explicitly selected credential variable or GitHub CLI login must be available;
missing credentials fail that provider and allow the next selected provider to run.
Leaving both options unset allows public access, or the default `GH_TOKEN` if set.
API providers default to `cids/<CID>`. To use ordinary filenames remotely, import
a JSON object mapping full CIDs to relative paths:

```sh
python3 browser-support/run.py import-index github cid-paths.json
```

Generate that mapping from a local clone or folder without reading helper state
files manually, then import it into a GitHub provider for the same file layout:

```sh
python3 browser-support/run.py export-index clone --output cid-paths.json
```

Use **Import index** beside a folder, local clone, or GitHub provider in settings
to select the same JSON file. Keys are bare CIDs; paths are relative to the folder
or repository root (include repository directories in imported paths). The settings
upload is limited to 1000 KiB; use the command above for larger manifests. **Export
index** downloads the current mapping. Folder indexes use forward slashes on all
operating systems so exported manifests can be reused on other desktop platforms.
Older Windows indexes should be rebuilt before export.

**Edit** preserves a provider's name, type, order, enabled state, index, and any
unshown options while updating its location, revision, authentication choices, or
timeout. Reindex after changes that affect filenames or revisions; use **Cancel
edit** to discard a draft. HTTP timeouts currently apply to individual socket
operations, rather than an overall download deadline.
Timeouts must be positive and at most 3600 seconds. Credential variable names use
letters, digits, and underscores, beginning with a letter or underscore. Providers
with explicitly selected authentication require HTTPS. Invalid settings are rejected
before they replace the saved configuration.

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
Simultaneous requests for the same CID share one provider retrieval and verification,
including when the object exceeds the disk cache budget. Requests for different
CIDs can resolve in parallel. Provider or file-index changes start a new retrieval
generation, so new requests do not join work that began under older settings.
The gateway supports single HTTP byte ranges for original resources, allowing
audio/video seeking and partial downloads. The entire object is retrieved and
verified before any range is sent. Transformed HTML/CSS, malformed or multipart
ranges, and conditional ranges without a matching validator receive full responses.
Disabling a provider retains previously verified cached content. Clear the
cache using **Clear cached content** in settings if you want to remove those copies.
Cache usage is available there too; reducing the budget evicts the oldest copies
immediately. Provider configuration and file indexes survive clearing the cache.

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
the active web-handler plan below uses an optional letters-only alias.

### Active implementation plan

Updated 2026-10-14. All three mechanisms are active workstreams. Checked items
record existing implementation or completed investigation; unchecked items are
remaining work. A mechanism is release-ready only after its acceptance checks
pass in installed browsers. Engine tests and generated extension packages alone
do not establish that result.

| Mechanism | Status and existing work | Strengths | Limitations | Next deliverable |
|---|---|---|---|---|
| Content script rewrites `t256:` links to a viewer/gateway; optional address-bar keyword | **In progress: implementation and browser validation.** Explicit-click adaptation, helper-free viewer navigation, Chromium/Firefox packages, Safari conversion source, native context menus and validated CID/URI keyword input exist. | Preserves the RFC URI; can adapt existing links and selected page resources; can use the helper's private/local providers. | Requires permission to access the page; current adaptation is a single pass. Extension-store/signing and installed Firefox/Safari checks remain. A keyword is a separate address-bar gesture. | Add opt-in dynamic adaptation and validate each installed extension package. |
| Web protocol handler, including the requested `web+t256:` spelling | **In progress: implementation and browser validation.** Strict alias conversion and explicit website registration/removal controls exist; engine checks cover transport and rejection. | A valid alias can open a browser viewer without an extension; website, Firefox extension, and installed Chromium PWA registration are separate options. | `web+t256:` fails the standard's scheme rules; an alias differs from the RFC. Registration does not enable custom-scheme subresources. Safari needs another route. | Validate installed-browser consent/dispatch; add saved-helper choices, Firefox manifest handling and optional PWA support. |
| OS registration of `t256:` with a desktop helper | **In progress: implementation and distribution validation.** macOS app wrapper, Windows per-user registry registration, Linux desktop entry, startup, native messaging and uninstall exist. | Preserves the exact scheme; opens content with user-selected folders, mounted shares, HTTP bases and authenticated repositories; offers Safari an external navigation route. | Requires installation and browser approval where prompted. Typed/pasted address-bar dispatch varies. Current runtime needs Python at a stable path. Does not turn `t256:` into a browser resource protocol. | Validate real browser dispatch and clean installation/removal on all three OSes; build convenient platform packages. |

The [HTML custom-handler rules](https://html.spec.whatwg.org/multipage/system-state.html#custom-handlers)
allow `web+` followed by lowercase ASCII letters, so neither bare `t256` nor
`web+t256` can be registered through the standard website API. Keep `t256:` as
the canonical RFC scheme. Use `web+tcontent:` as the working compatibility alias;
document the convention before publishing alias links. Exact `web+t256:` support
requires a browser-specific exception or an upstream change and is an
investigation item, not a promised shipping capability.

Desktop Firefox and Chromium support
[`navigator.registerProtocolHandler()`](https://developer.mozilla.org/en-US/docs/Web/API/Navigator/registerProtocolHandler)
for eligible schemes; do not describe the Chromium website API as generally
flag-only. Chrome also documents
[installed-PWA protocol handlers](https://developer.chrome.com/docs/web-platform/best-practices/url-protocol-handler)
as available from Chrome 96. Test website registration, extension manifest
registration and PWA installation independently, with default browser settings.

Safari can also adapt page links through a
[Safari Web Extension](https://developer.apple.com/documentation/safariservices/safari-web-extensions)
or use the loader/gateway. The OS helper is the primary planned external route
for the exact scheme in Safari; it is not the only practical Safari integration.

#### 1. Extension links, viewer and keyword

- [x] Build Chromium and Firefox extensions and Safari conversion source from
  shared code (`extensions/shared`, `build_extensions.py`).
- [x] Rewrite existing page navigation links to the verified helper gateway
  after an explicit user action; adapt images, stylesheets and media too.
- [x] Provide native-host context menus and keyword event handling on browsers
  with those APIs; exercise page adaptation in the Chromium extension smoke test.
- [x] Make `t256 <CID>[.suffix]` work in keyword mode. Accept bare CIDs and full
  URIs, preserve CID case, validate with the shared RFC parser before native
  launch, and show input guidance. Unit tests exercise the built Chromium and
  Firefox background scripts; installed Firefox checks remain alongside Chrome's
  [keyword API](https://developer.chrome.com/docs/extensions/reference/api/omnibox).
- [x] Add a configurable viewer destination and **Open in browser viewer** /
  **Adapt links without helper** actions. Default to the public HTTPS resolver
  for public/inline content; private/local content retains the helper route.
  Strictly validate URIs and viewer locations, preserve existing link relations
  and leave invalid links/resources unchanged. Chromium tests cover popup opening,
  link navigation, no provider requests during adaptation and verified bytes.
- [ ] **Next: add optional continued adaptation** for new or changed links/resources on
  the active page. Bound observation, preserve page edits made during retrieval,
  release resources on teardown, and avoid repeated media loads. Add optional
  per-site persistence only with explicit site permissions and a disable control.
- [ ] Define frame and open-shadow-root coverage; report inaccessible frames,
  protected browser pages, restrictive CSP and local-network permission failures.
  Link-only adaptation already works without granting local provider access.
- [ ] Validate Firefox installation/signing, Edge/Brave/Opera compatibility and
  a built Safari extension. For Safari, retain the manual connection option and
  plan containing-app messaging; do not assume Python native-host registration
  or an omnibox API works there.
- [ ] Connect page-resource improvements to the existing loader and gateway:
  scripts need ordered loading, module dependency maps and CSP handling, beyond
  link rewriting. Track module workers, computed imports and SVG dependencies
  as separate capability work rather than claiming transparent script support.

Acceptance: unmodified `t256:` anchors navigate after activation; keyword mode
accepts bare CIDs and full URIs where available; dynamic adaptation stops when
disabled; copied/shared links contain no helper capability; unavailable content
has a useful failure state. Validate inline and hashed CIDs, mixed CID case,
compound suffixes, invalid input, missing helper, revoked grants and private
windows. Resource tests must verify bytes before use and script execution order.

#### 2. Web handler and compatibility alias

- [x] Establish the standards restriction on `web+t256:` and identify the
  letters-only `web+tcontent:` route. No change to the canonical RFC is needed.
- [x] Implement the shared alias adapter and handler entry point in
  `web/protocol-handler.mjs` and the public resolver. Convert only the recognized
  alias's scheme, preserve CID case, use the strict RFC parser and decode the
  transport once. Unit tests cover inline/hashed IDs, compound suffixes,
  malformed encodings, unknown aliases and duplicate URI parameters.
- [x] Add explicit website registration and supported removal buttons. Use a
  same-origin `%s` fragment template without existing query parameters or helper
  capabilities; support HTTPS and secure loopback development. Feature detection,
  rejection feedback and manual resolution remain available. Engine tests check
  template forwarding and explicit invocation with simulated browser APIs.
- [x] Reuse verified public/inline and pasted-helper resolution. Explain that
  this page can read the identifier, including inline bytes, and wait for the
  user's provider choice before fetching. Hash changes clear old previews;
  downloads and shared links retain canonical `t256:` identifiers.
- [ ] **Next: validate real consent and dispatch** in installed Firefox/Chrome/
  Edge, including denied prompts, private windows and browser-settings removal.
- [x] Add explicit **Remember connection in this browser** and **Forget
  remembered connection** controls. Restore a validated loopback connection on
  subsequent visits without retrieving anything automatically or including it
  in registration templates/shared links. Browser checks cover save/reload,
  removal and blocked storage; invalid credential-bearing URLs are rejected.
- [ ] Test Firefox's optional
  [`protocol_handlers` extension manifest](https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/manifest.json/protocol_handlers)
  route to an extension viewer. Keep browser-specific manifest fields separate
  from the Chromium/Safari packages and avoid competing registrations by default.
- [ ] Add an optional installable Chromium PWA with `protocol_handlers`, scoped
  handler URLs and clear installation/removal instructions. Test OS integration
  separately from website registration; keep the basic resolver usable without
  PWA installation. Probe Edge, Brave and Opera rather than inheriting Chrome's
  support claim.
- [x] Add negative engine checks for exact `web+t256:` registration (standard
  rejection or API unavailability). Chromium and WebKit checks pass locally;
  Firefox checks run in CI. Valid-registration consent is not bypassed.
- [ ] Investigate any documented browser-specific exception or required flag
  in isolated profiles. Explore an upstream scheme-rule or allowlist change
  only as a long-term route; never silently enable browser flags.
- [x] Publish canonical/alias/HTTPS examples and the conversion convention below.
  Provide removal controls where available and settings guidance otherwise;
  retain extension/OS/manual alternatives for unsupported browsers.

Acceptance: a clicked valid alias arrives as the exact canonical identifier;
invalid aliases never retrieve content; registration requires explicit action;
declining or removing registration leaves the fallback usable. Test the website
flow in installed Firefox/Chrome/Edge and PWA/extension flows only where supported.
Record address-bar results independently from link activation. Do not count
navigation-handler success as `fetch`, image or script protocol support.

#### 3. OS registration, native helper and distribution

- [x] Implement per-user scheme registration for macOS, Windows and Linux in
  `t256_helper/install.py`, plus automatic helper startup and strict URI parsing.
- [x] Launch a verified gateway page; provide Chrome-family/Firefox native-host
  registration restricted to the specified extension ID.
- [x] Implement ownership-aware removal, Linux previous-handler restoration,
  provider configuration, verification and private/local content access.
- [x] Run helper and registration unit tests across all three OSes in CI. These
  do not exercise every installed desktop/browser registration service.
- [ ] **Next: perform fresh-profile installation and dispatch checks.** Exercise
  clicked links, typed and pasted URIs, prompt acceptance/denial, browser closed,
  helper stopped, repeated launches and private windows. Record browser build,
  OS version and default settings; classify address-bar input that becomes a
  search as a limitation and provide keyword/paste-into-viewer alternatives.
- [ ] Package an embedded runtime or standalone helper for each platform, with
  installation, update and removal UI. Test paths containing spaces/non-ASCII
  characters, moved installs, existing handler ownership and uninstall after
  the default handler changes. Keep the existing Python source package usable.
- [ ] For macOS, validate LaunchServices delivery and create a distributable
  signed/notarized app; include the Safari extension/containing-app connection
  path. Treat the current generated ad-hoc wrapper as a development installation.
- [ ] For Windows, validate per-user registry delivery, safe argument forwarding,
  browser/native-host discovery and a signed installer. Avoid administrator
  requirements and avoid replacing another handler without a deliberate choice.
- [ ] For Linux, validate desktop entry and `xdg-mime` behavior on GNOME/KDE,
  desktop portals and packaged browsers such as Flatpak/Snap. Document dependency
  and sandbox access limits for mounted shares, launchers and native hosts.
- [ ] Improve launch feedback and destination selection: distinguish invalid
  URI, missing provider/authentication, blocked browser launch and expired helper
  connection. Support opening in the requesting extension where available, with
  the default browser/gateway as the OS-handler fallback.

Acceptance: installation routes the unchanged URI to one running helper and a
verified viewer without a launch loop; denying a browser prompt does not open
content; uninstall leaves user settings intact and respects subsequent handler
changes. Verify inline/hashed content and selected filesystem, mounted share,
HTTP, Hashbin and authorized GitHub providers. Keep OS navigation tests separate
from page-resource tests: resource loading uses the loader or transformed gateway.

#### Browser coverage and delivery order

| Target | Extension plan | Web-handler plan | OS-handler release gate |
|---|---|---|---|
| Chrome / Edge on macOS, Windows, Linux | Shared package, native host, keyword; test each installed browser | HTTPS website alias; optional PWA, tested separately | Actual click/typed/pasted dispatch and install/removal on each OS |
| Firefox on macOS, Windows, Linux | Signed package, native host, keyword; actual extension tests | HTTPS website alias; optional extension manifest handler | Actual dispatch, prompts and native-host discovery on each OS |
| Brave / Opera on macOS, Windows, Linux | Shared Chromium source; branded-browser permission/store tests | Probe website/PWA support under default settings | Actual dispatch and browser-specific native-host discovery |
| Safari on macOS | Built containing app, link adaptation and manual/native connection choices; no current keyword route | Feature detection and HTTPS fallback; no website API support promised | LaunchServices, Safari prompts, signing/notarization and real Safari tests |

Delivery order: keyword input, explicit alias registration and extension viewer
fallback are implemented; next validate installed
browser/OS combinations and package the helper/Safari app. Dynamic adaptation,
Firefox manifest handling and PWA installation can follow as separate increments.
Update this checklist with each increment and link its test or manual-check
evidence. Every supported combination needs a recorded pass or a documented
limitation before being advertised as release-ready.

### Cooperating pages

Grant the page's **exact origin** in helper settings, then copy its connection URL.
For example, a local development site might use `http://127.0.0.1:8080`.
Connections rotate on helper restart; revoke an origin in settings to reject
subsequent requests. Granting that origin again creates a new connection URL;
previously revoked URLs remain invalid. Unrelated provider or storage edits preserve
connections for origins that stayed allowed. A connection is a bearer capability: anyone holding it can
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
as blobs, without `eval`. Repeated or overlapping hydration does not rerun unchanged
declarations. Stylesheets finish loading before declared scripts run. Dynamic opt-in
declarations can be handled with `observe()`. Load events are bounded by the client
timeout, and `dispose()` cancels pending retrieval/loading, disconnects observers,
and releases owned blob URLs. Script execution already started cannot be undone;
workers returned to the page must be terminated by the caller.

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
values before the browser parses it. Responsive image candidates in `srcset` and
`imagesrcset` are transformed as complete URLs, preserving data URLs and descriptors.
External CSS `url()` and quoted `@import` references are also transformed, including
CSS character escapes, escaped keywords, and quoted line continuations. Comments
and ordinary CSS strings remain unchanged. These use the standard
[CSS escape syntax](https://www.w3.org/TR/css-syntax-3/#escaping).
Nested HTML frames and t256 navigation links use
the gateway. Original JavaScript is not changed.

Transformed HTML and CSS must be UTF-8 and are served explicitly as UTF-8.

An existing page can use standard import maps with t256 targets:

```html
<script type="importmap">
{"imports":{"named-dependency":"t256:DEPENDENCY_CID.mjs"}}
</script>
<script type="module" src="t256:ENTRY_CID.mjs"></script>
```

HTML and SVG content documents have a sandboxed, opaque origin, including direct
navigation to original/raw representations. They can run their selected
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
**Open in browser viewer** sends a validated URI to the selected viewer (the
public HTTPS resolver by default) without invoking a native host.
**Adapt links without helper** rewrites only current `t256:` anchors to that
viewer. It does not retrieve content, rewrite page resources, or require a helper
connection/provider grant. Invalid URIs remain unchanged and report an error;
existing link relations are retained. Only HTTPS or loopback HTTP viewer URLs
without credentials, queries or fragments are accepted. The URI is carried in
the viewer's fragment, and the viewer waits for a resolution action. The selected
viewer can read the identifier and inline bytes after navigation. These actions
are included in Safari conversion source; signed Safari installation remains a
release check.

“Adapt images, media, and links” handles current images (including `srcset` and
picture sources), audio/video sources, posters, caption tracks, stylesheets, and
navigation references after an explicit click. Source changes reload the affected
media element once, so resources can be selected again. Identical URIs share one
metadata request per adaptation. Attributes changed by the page during retrieval
are left alone, and unavailable content reports the helper's provider diagnostics.
It deliberately leaves existing script tags
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
universally recognized. Enter either a CID (for example,
`t256 AAAAAAAFaGVsbG8.txt`) or a full URI after activating the keyword. Invalid
identifiers show input guidance and do not launch the native host.

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
The resolver also accepts the compatibility alias `web+tcontent:<CID>[.suffix]`.
It replaces that scheme with `t256:` before parsing or retrieving content; CID
case is preserved and suffixes follow the canonical parser's normalization.
For example, `web+tcontent:AAAAAAAFaGVsbG8.txt` maps to
`t256:AAAAAAAFaGVsbG8.txt`. No other alias is accepted, and the RFC parser/helper
continue to require `t256:`.

Select **Enable browser links** on the HTTPS resolver (or a secure local helper
page) to request website registration, then approve it in the browser. The
template uses `resolve.html#uri=%s` on that same origin, with no existing query,
connection or credentials. A successful API call reports a request sent rather
than claiming approval; the browser controls confirmation and defaults. Incoming
links populate the canonical URI field and wait for **Resolve and verify** before
accessing providers. Existing providers and a pasted helper connection can be
selected for that visit. **Remember connection in this browser** explicitly
saves the validated connection in this site's browser storage and restores it
for subsequent visits, including alias navigation. Connections are not saved
automatically, and restoration does not fetch content. Replace a remembered URL
after restarting the helper; stale/revoked grants still fail at retrieval.
**Forget remembered connection** removes the saved URL and clears the field;
revoking provider access requires removing the site's grant in helper settings.
If browser storage is blocked, manual connection entry and resolution still work.

Use **Remove browser link handler** where the browser exposes removal, or change
the handler in browser settings. Unsupported/blocked registration leaves manual
resolution usable. PWA and Firefox extension-manifest registration are separate
pending plan items. Engine tests simulate valid-registration UI outcomes and
exercise real invalid-scheme rejection without registering a handler in the
user's browser; installed-browser consent and external dispatch remain release
checks.

HTML, JavaScript, and SVG previews are text. Downloads use a binary media type,
preserve the original verified bytes, and cannot create an HTML/SVG document under
the resolver page's origin. Bitmap previews use separate temporary URLs.
Shared links contain only the URI in
their fragment, never a helper capability or provider credential.
Use **Cancel retrieval** to stop a pending provider or helper request and retry.
Cancellation clears stale previews/download links, and leaving the page releases
its preview objects. Public providers that time out while streaming content fall
back to the next selected provider; caller cancellation stops the search entirely.
The reader cancels stalled streams and releases their locks. A cancelled browser
request to the helper can leave an already-started provider retrieval running for
other requests that share it.

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
npm --prefix browser-support test
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
imports, SVG-internal resources, multipart media ranges, and upstream
browser-engine implementation remain future work.
