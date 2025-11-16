#!/usr/bin/env bash
set -euo pipefail

source "$(dirname -- "${BASH_SOURCE[0]}")/cid.sh"

main() {
  mkdir -p "$CIDS_DIR"
  local example cid destination
  while IFS= read -r -d '' example; do
    cid=$(compute_cid "$example")
    destination="$CIDS_DIR/$cid"
    cp "$example" "$destination"
    echo "Wrote $(basename -- "$destination") from $(basename -- "$example")"
  done < <(find "$EXAMPLES_DIR" -maxdepth 1 -type f -print0 | sort -z) || true
}

main "$@"
