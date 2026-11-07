# t256 — command line tool for 256t content

`t256` saves content and gets back its [256t](https://256t.org) content identifier (CID). Later it loads that content by CID from any provider you use: a folder on this machine, a network drive, [256t.org](https://256t.org), [hashbin.org](https://hashbin.org), or any other 256t-compatible server. It checks everything it loads against the CID, so a provider can't hand you the wrong bytes without the tool noticing.

It is a single static binary for macOS, Linux and Windows (x86-64 and ARM64), with no runtime dependencies. It is built for scripts: references go to standard output and notices go to standard error, it has JSON output and documented exit codes, and it never asks questions interactively.

```console
$ echo 'Hello from 256t, a content-addressed store where the name is the content.' > hello.txt
$ t256 save hello.txt
AAAAAABKPqCjyp8bJmYowowe4WO1_9XgOx9X8nzfS7iApBrgGj5xmOTSkEYdAaISFcvUFFm7fR6z20MB3jADKRM68FuVgQ
$ t256 load AAAAAABKPqCjyp8bJmYowowe4WO1_9XgOx9X8nzfS7iApBrgGj5xmOTSkEYdAaISFcvUFFm7fR6z20MB3jADKRM68FuVgQ
Hello from 256t, a content-addressed store where the name is the content.
$ t256 save report.pdf -f t256 --ext auto
t256:AAAAkijj0AqKH3tO-J3GF7hKYylmVo2E1qqGeLAhWEk5BUrLvYxa3hJpbqXlmRAK77ncy6OuCB2jM9zT_Jiur6PlZCwqSw.pdf
```

- [Install](#install)
- [References: 256t and t256](#references-256t-and-t256)
- [Commands](#commands)
- [Providers](#providers)
- [Configuration](#configuration)
- [Scripting](#scripting)
- [Security notes](#security-notes)
- [Building and releasing](#building-and-releasing)

## Install

**macOS and Linux**

```bash
curl -fsSL https://raw.githubusercontent.com/curtcox/256t.org/main/tools/t256/install.sh | sh
```

The script installs to `/usr/local/bin` if it can write there and to `~/.local/bin` if not. Set `T256_INSTALL_DIR` to pick another directory, or `T256_VERSION=t256-v0.1.0` to pin a release.

**Windows (PowerShell)**

```powershell
irm https://raw.githubusercontent.com/curtcox/256t.org/main/tools/t256/install.ps1 | iex
```

The script installs `t256.exe` to `%LOCALAPPDATA%\Programs\t256` and adds that folder to your user `PATH`. Download the script and run it with `-NoPath` to leave `PATH` alone, or with `-InstallDir` or `-Version` to change the location or release.

Both installers download from the [GitHub releases](https://github.com/curtcox/256t.org/releases) and check the binary against the release's `SHA256SUMS` before installing it.

**From source** (Go 1.22 or later)

```bash
go install github.com/curtcox/256t.org/tools/t256@latest
```

You can also download a binary such as `t256-linux-arm64` or `t256-windows-amd64.exe` from the releases page yourself and put it on your `PATH`.

## References: 256t and t256

Wherever a command takes a `REF`, any of these forms works:

| Form | Example |
|------|---------|
| Bare CID (the 256t form) | `AAAAAAANSGVsbG8sIFdvcmxkIQ` |
| Bare CID with a media type suffix | `AAAAAAANSGVsbG8sIFdvcmxkIQ.txt` |
| `t256:` URI ([draft RFC](../../rfc/draft-cox-t256-uri-scheme-00.md)) | `t256:AAAAAAANSGVsbG8sIFdvcmxkIQ.txt` |
| `256t:` prefix (accepted leniently, never produced) | `256t:AAAAAAANSGVsbG8sIFdvcmxkIQ` |
| URL ending in a CID | `https://256t.org/AAAAAAANSGVsbG8sIFdvcmxkIQ` |

The suffix (`.txt`, `.png`, `.tar.gz`) states a media type. It does not change which content the CID names. When you load a URL, the tool tries that URL first and then falls back to your configured sources.

To choose how references are printed, use `-f`/`--format`:

| `--format` | Prints |
|------------|--------|
| `cid` (default) or `256t` | `AAAAAAANSGVsbG8sIFdvcmxkIQ` |
| `t256` | `t256:AAAAAAANSGVsbG8sIFdvcmxkIQ` |
| `url` | where the content is stored: a URL or a file path |
| `json` | one JSON object per item (same as `--json`) |

`--ext png` adds a suffix to the printed reference. `--ext auto` takes the suffix from the input file's extension.

Content of 64 bytes or less is carried inside its CID. Loading such a CID never touches the network, and `save` doesn't store such content unless you pass `--store-inline`.

## Commands

```
t256 save   [FILE...]        store content at the destination, print its reference
t256 load   REF... [-o FILE] fetch content from the sources, verify it, write it
t256 cid    [FILE...]        compute references without storing anything
t256 verify REF [FILE]       check that content matches a reference
t256 has    REF...           check which sources hold content
t256 info   REF...           describe a reference (length, inline, media type)
t256 providers               list providers, sources and the destination
t256 config [path|show|init] manage the config file
t256 version
```

A `FILE` of `-`, or no `FILE` at all, means standard input. Flags can go before or after the arguments, and `--` ends the flags. `get` and `put` work as aliases for `load` and `save`. `t256 help COMMAND` lists a command's options.

These options work with every command:

| Option | Meaning |
|--------|---------|
| `--config FILE` | use this config file |
| `-q`, `--quiet` | no notices on stderr (errors are still reported) |
| `-v`, `--verbose` | log each provider attempt on stderr |
| `--timeout 30s` | limit on connecting and on waiting for a response (`0` = none, default `60s`). A large transfer is never cut off once it has started. |

### save

```bash
t256 save photo.jpg                       # store at the default destination
t256 save --to hashbin --retention-months 12 --max-cost-cents 50 site.tar.gz
t256 save --to /Volumes/Shared/256t *.pdf # several files: "REF  FILE" per line
tar cz src | t256 save -f t256 --ext tar.gz
t256 save -n big.iso -f url               # dry run: show the reference and location only
```

| Option | Meaning |
|--------|---------|
| `--to DEST` | destination instead of the configured one: a provider name, a URL (uses `PUT`), or a directory |
| `--type TYPE` | media type to store with the content (default: from the file name or `--ext`) |
| `--retention-months N` | hashbin: whole months to keep the content |
| `--max-cost-cents N` | hashbin: refuse an upload that would cost more than this |
| `--skip-existing` | don't upload content the destination already reports having |
| `--store-inline` | store content of 64 bytes or less as well |
| `-n`, `--dry-run` | compute and show only; store nothing |
| `-f`, `--ext`, `--json` | output, as described above |

When you save several files and some of them fail, the rest are still saved. Each failure is reported, and the exit code is 1.

### load

```bash
t256 load AAAAAAKANHMwYm...Q > report.pdf
t256 load t256:AAAAAAKANHMwYm...Q.pdf -o report.pdf
t256 load --from nas,256t.org REF -o out.bin
t256 load -O downloads/ REF1 REF2 REF3     # writes downloads/CID.suffix and prints each path
t256 load --cache REF -o out.bin          # also keep a verified copy at the destination
```

`load` tries each source in order. The first one that returns content matching the CID wins. A source that returns the wrong bytes is reported and skipped. Content is checked before anything is written: a file given with `-o` is replaced in a single step, and nothing reaches standard output until the content has been verified.

| Option | Meaning |
|--------|---------|
| `--from SRC` | sources to use instead of the configured ones; repeat the option or separate them with commas |
| `-o FILE` | write to FILE (an existing directory means `DIR/CID.suffix`) |
| `-O DIR` | write each REF to `DIR/CID.suffix` and print the paths; allows several REFs |
| `--cache` | save verified content to the destination (the local store by default) |
| `--json` | print metadata (which provider served the content, its location, verified) (needs `-o` or `-O`) |

### cid, verify, has, info

```bash
t256 cid file.bin                    # same CID that `save` would print
find . -type f -exec t256 cid {} +   # "REF  FILE" per line, like shasum
t256 verify REF downloaded.bin       # prints OK; exit 4 on mismatch
curl -s https://example.com/CID | t256 verify -q CID && echo good
t256 has REF                         # prints the first location that has it
t256 has --all REF                   # one line per source: REF, provider, yes/no, location
t256 info t256:CID.tar.gz            # length, inline or hashed, suffix, media type
```

### providers, config

```console
$ t256 providers
local        dir      read-write source 1, destination  /Users/me/Library/Application Support/t256/store
256t.org     http     read-only  source 2               https://256t.org
hashbin      hashbin  read-write source 3               https://256t.us (save via https://hashbin.org/api)
```

- `t256 config path` prints the config file location.
- `t256 config show` prints the configuration in effect, including the built-in providers.
- `t256 config init` writes an example config file to edit.

## Providers

A **provider** is a place content can be loaded from and, if it allows it, saved to. You can use several **sources** at once; they are tried in order. Saving goes to one **destination**.

Three providers are built in:

| Name | Type | Load | Save |
|------|------|------|------|
| `local` | dir | per-user store (see below) | yes |
| `256t.org` | http | `https://256t.org/<cid>` | no (read-only) |
| `hashbin` (alias `hashbin.org`) | hashbin | `https://256t.us/<cid>` | with an API key |

The default sources are `local`, then `256t.org`, then `hashbin`. The default destination is `local`, so nothing is uploaded and nothing costs money until you choose a different destination.

The `local` store lives at:

- macOS: `~/Library/Application Support/t256/store`
- Linux: `$XDG_DATA_HOME/t256/store` (default `~/.local/share/t256/store`)
- Windows: `%LOCALAPPDATA%\t256\store`

### Filesystem directories and network drives (`dir`)

A `dir` provider stores each item as a file named by its CID in one flat directory. Any path works, including mounted network drives:

| Platform | Network path example |
|----------|---------------------|
| macOS | `/Volumes/Shared/256t` (SMB/AFP/NFS mount) |
| Linux | `/mnt/nas/256t` |
| Windows | `\\server\share\256t` or `Z:\256t` |

Writes go to a temporary file in the same directory and are then renamed into place. A half-written file never shows up under a CID, and several machines can safely share one directory. Only the built-in `local` store is created automatically. Any other directory must already exist (set `"create": true` to change that), so an unmounted drive gives an error instead of quietly filling an empty mount point.

A `dir` store uses the same layout a 256t server serves. You can publish one by serving the directory over HTTP, and other machines can then use it as an `http` source.

### HTTP servers (`http`)

Loading is `GET <url>/<cid>`, which every 256t-compatible server supports. If the URL contains `{cid}`, the CID is substituted there instead of appended (for example `https://example.com/raw/{cid}?download=1`).

To make an HTTP provider writable, set `"save"`:

- `"put"` sends `PUT <save_url or url>/<cid>` with the content as the body.
- `"post"` sends `POST <save_url>` (substituting `{cid}` if it appears).

Any 2xx response counts as success, and a `Location` header, if present, is reported as where the content went. `headers` are sent with every request, and `${VAR}` in them is replaced from the environment, so tokens can stay out of the file.

### hashbin.org (`hashbin`)

Anyone can load content from hashbin. To save, you need a hashbin account with funds and an API key that has the `content:write` scope. Create one at <https://hashbin.org/api-keys.html>. Hashbin lets you limit a key (a monthly spending cap, the largest upload, allowed content types), and a key used only by this tool should be limited.

```bash
export T256_HASHBIN_API_KEY=hbk_...
t256 save --to hashbin --retention-months 12 --max-cost-cents 100 site.tar.gz -f url
```

Uploads go to `POST https://hashbin.org/api/content`, with `Authorization: ApiKey …`, the content type, `retention_months` and `max_cost_cents`. The tool checks that hashbin's reported CID matches the one it computed. Saving content hashbin already has buys more retention and is charged. Use `--skip-existing` to skip it instead. Hashbin accepts at most 90 MB per upload.

### Ad-hoc providers

`--from` and `--to` (and entries in `sources`) accept a URL or a path directly, without any config:

```bash
t256 load --from https://mirror.example.org/cids,/mnt/backup/256t REF
t256 save --to ./build-cache artifact.zip       # existing directory
t256 save --to https://cas.example.com/256t f   # PUT to the server
```

Anything containing `/` or `\`, starting with `.` or `~`, starting with a drive letter, or naming an existing directory is treated as a path. Write `dir:NAME` to force a path.

## Configuration

The config file is JSON. It is found at:

1. the path given with `--config`
2. otherwise `$T256_CONFIG`
3. otherwise the per-user file: `~/Library/Application Support/t256/config.json` (macOS), `~/.config/t256/config.json` (Linux), or `%APPDATA%\t256\config.json` (Windows)

You don't need a config file: without one, the built-in defaults above apply. Here is an example (`t256 config init` writes a similar one):

```json
{
  "sources": ["local", "nas", "256t.org", "hashbin"],
  "destination": "nas",
  "timeout": "60s",
  "providers": {
    "nas": {
      "type": "dir",
      "path": "//nas/share/256t"
    },
    "team": {
      "type": "http",
      "url": "https://cas.example.com/256t",
      "save": "put",
      "headers": { "Authorization": "Bearer ${TEAM_CAS_TOKEN}" }
    },
    "hashbin": {
      "type": "hashbin",
      "api_key_env": "T256_HASHBIN_API_KEY",
      "retention_months": 12,
      "max_cost_cents": 100
    }
  }
}
```

A provider you define with the same name as a built-in one (`local`, `256t.org`, `hashbin`) replaces it.

| Field | Types | Meaning |
|-------|-------|---------|
| `type` | all | `dir`, `http` or `hashbin` |
| `path` | dir | directory; `~` and `${VAR}` are expanded |
| `create` | dir | create the directory on the first save (default false, except for `local`) |
| `read_only` | dir | never save here |
| `url` | http | base URL for loading (or a template containing `{cid}`) |
| `save` | http | `put` or `post`; leave it out for read-only |
| `save_url` | http | URL for saving if it differs from `url` |
| `headers` | http | extra request headers; `${VAR}` is expanded |
| `content_url` | hashbin | where content is loaded from (default `https://256t.us`) |
| `api_url` | hashbin | API base (default `https://hashbin.org/api`) |
| `api_key_env` | hashbin | environment variable that holds the key (default `T256_HASHBIN_API_KEY`) |
| `api_key` | hashbin | the key itself (prefer `api_key_env`; if you use this field, keep the file private) |
| `retention_months` | hashbin | default retention for saves |
| `max_cost_cents` | hashbin | default per-upload cost ceiling |

### Environment variables

| Variable | Meaning |
|----------|---------|
| `T256_CONFIG` | config file path |
| `T256_SOURCES` | comma-separated sources; overrides the config file |
| `T256_DESTINATION` | destination; overrides the config file |
| `T256_TIMEOUT` | network timeout, e.g. `30s` |
| `T256_HASHBIN_API_KEY` | hashbin API key (or whatever variable `api_key_env` names) |

The order of precedence is: command line flags, then environment variables, then the config file, then the built-in defaults.

## Scripting

**Exit codes**

| Code | Meaning |
|------|---------|
| 0 | success |
| 1 | error (I/O, network, provider refused, partial failure) |
| 2 | usage error (bad flag, malformed reference, unknown provider) |
| 3 | not found in any source |
| 4 | verification failed: content did not match the CID |

Only the result goes to standard output: the reference, the content, or a path. Notices and errors go to standard error. `--json` prints one object per line, with fields such as `cid`, `uri`, `length`, `inline`, `suffix`, `media_type`, `file`, `provider`, `location`, `stored`, `found`, `verified` and `error`.

```bash
# Save a build artifact and record its URI
uri=$(t256 save -q -f t256 --ext auto dist/app.zip) || exit 1
echo "artifact: $uri" >> RELEASE_NOTES

# Fetch, failing loudly if missing or tampered with
if ! t256 load "$uri" -o app.zip; then
  case $? in 3) echo "not available anywhere";; 4) echo "corrupt copies only";; esac
  exit 1
fi

# Mirror a list of CIDs from the internet into a shared drive
t256 load --cache --to /mnt/nas/256t -O /tmp/mirror $(cat cids.txt)

# Use with jq
t256 save --json *.png | jq -r '"\(.file)\t\(.uri)"'

# Content-addressed cache in CI: skip the work if the output already exists
t256 has -q --from ./cache "$EXPECTED" || make build
```

```powershell
# PowerShell
$ref = t256 save -q -f t256 --ext auto .\report.pdf
if ($LASTEXITCODE -ne 0) { throw "save failed" }
t256 load $ref -o .\copy.pdf
t256 save --json .\*.txt | ConvertFrom-Json | Select-Object file, uri
```

## Security notes

- Everything loaded from a provider is checked against the CID before you get it. `has` only asks whether a provider has the content and checks nothing. `load` checks everything.
- The suffix (`.html`, `.png`) is not covered by the CID. Anyone who can change a reference can change its suffix. Treat the media type it implies the way you would treat an untrusted `Content-Type` header.
- For content of 64 bytes or less, the CID *is* the content, in base64url. Keep such CIDs as private as the content itself.
- A CID shows the exact length of its content.
- Keep API keys in environment variables or a secrets manager, not in shared config files. Give hashbin keys only `content:write` and a spending limit.

## Building and releasing

```bash
cd tools/t256
go test ./...
go build -o t256 .                                     # this platform
GOOS=windows GOARCH=amd64 go build -o t256.exe .       # cross-compile
```

The [t256 CLI workflow](../../.github/workflows/t256-cli.yml) runs the tests and both installers on Linux, macOS and Windows for every change under `tools/t256`. To publish a release, push a tag such as `t256-v0.1.0`. The workflow then builds `t256-{darwin,linux,windows}-{amd64,arm64}`, writes `SHA256SUMS`, and attaches everything to a GitHub release, which the install scripts then download.
