#!/bin/sh
# Install the t256 command line tool on macOS or Linux.
#
#   curl -fsSL https://raw.githubusercontent.com/curtcox/256t.org/main/tools/t256/install.sh | sh
#
# Environment:
#   T256_VERSION      release tag to install, e.g. t256-v0.1.0 (default: latest)
#   T256_INSTALL_DIR  where to put the binary (default: /usr/local/bin if
#                     writable, otherwise ~/.local/bin)
#   T256_BASE_URL     download from this URL instead of GitHub releases
set -eu

repo="curtcox/256t.org"

case "$(uname -s)" in
  Darwin) os=darwin ;;
  Linux) os=linux ;;
  *) echo "t256 install: unsupported OS $(uname -s); on Windows use install.ps1" >&2; exit 1 ;;
esac
case "$(uname -m)" in
  x86_64 | amd64) arch=amd64 ;;
  arm64 | aarch64) arch=arm64 ;;
  *) echo "t256 install: unsupported CPU $(uname -m)" >&2; exit 1 ;;
esac

name="t256-${os}-${arch}"
if [ -n "${T256_BASE_URL:-}" ]; then
  base="${T256_BASE_URL%/}"
elif [ -n "${T256_VERSION:-}" ]; then
  base="https://github.com/${repo}/releases/download/${T256_VERSION}"
else
  base="https://github.com/${repo}/releases/latest/download"
fi

dir="${T256_INSTALL_DIR:-}"
if [ -z "$dir" ]; then
  if [ -w /usr/local/bin ]; then dir=/usr/local/bin; else dir="$HOME/.local/bin"; fi
fi
mkdir -p "$dir"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT INT TERM

echo "Downloading ${base}/${name}"
curl -fsSL -o "$tmp/$name" "$base/$name"
curl -fsSL -o "$tmp/SHA256SUMS" "$base/SHA256SUMS"

expected="$(awk -v n="$name" '$2 == n || $2 == "*"n {print $1}' "$tmp/SHA256SUMS")"
if command -v sha256sum >/dev/null 2>&1; then
  actual="$(sha256sum "$tmp/$name" | awk '{print $1}')"
else
  actual="$(shasum -a 256 "$tmp/$name" | awk '{print $1}')"
fi
if [ -z "$expected" ] || [ "$expected" != "$actual" ]; then
  echo "t256 install: checksum mismatch for $name" >&2
  exit 1
fi

chmod +x "$tmp/$name"
mv "$tmp/$name" "$dir/t256"
echo "Installed $("$dir/t256" version) to $dir/t256"

case ":$PATH:" in
  *":$dir:"*) ;;
  *) echo "Add $dir to your PATH to run t256 from anywhere." ;;
esac
