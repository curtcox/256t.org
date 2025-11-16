#!/usr/bin/env bash
set -euo pipefail

source "$(dirname -- "${BASH_SOURCE[0]}")/cid.sh"

main() {
  local mismatches=()
  local count=0
  while IFS= read -r -d '' path; do
    ((count++))
    actual=$(basename -- "$path")
    expected=$(compute_cid "$path")
    if [[ "$actual" != "$expected" ]]; then
      mismatches+=("$path:$expected")
    fi
  done < <(find "$CIDS_DIR" -maxdepth 1 -type f -print0 | sort -z) || true

  if (( ${#mismatches[@]} > 0 )); then
    echo "Found CID mismatches:"
    for mismatch in "${mismatches[@]}"; do
      IFS=":" read -r path expected <<<"$mismatch"
      echo "- $(basename -- "$path") should be $expected"
    done
    return 1
  fi

  echo "All $count CID files match their contents."
}

main "$@"
