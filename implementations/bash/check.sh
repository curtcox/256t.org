#!/usr/bin/env bash
set -euo pipefail

source "$(dirname -- "${BASH_SOURCE[0]}")/cid.sh"

main() {
  local mismatches=()
  local download_failures=()
  local count=0
  local base_url="https://256t.org"
  
  while IFS= read -r -d '' path; do
    ((count++))
    local cid
    cid=$(basename -- "$path")
    local expected
    expected=$(compute_cid "$path")
    
    # Check local CID file
    if [[ "$cid" != "$expected" ]]; then
      mismatches+=("$cid:$expected")
    fi
    
    # Check downloaded content
    local download_result
    if download_result=$(download_cid "$base_url" "$cid" 2>&1); then
      IFS='|' read -r content computed is_valid <<<"$download_result"
      if [[ "$is_valid" != "true" ]]; then
        download_failures+=("$cid:$computed")
      else
        local local_content
        local_content=$(<"$path")
        if [[ "$content" != "$local_content" ]]; then
          download_failures+=("$cid:content mismatch with local file")
        fi
      fi
    else
      download_failures+=("$cid:$download_result")
    fi
  done < <(find "$CIDS_DIR" -maxdepth 1 -type f -print0 | sort -z) || true

  local has_errors=0

  if (( ${#mismatches[@]} > 0 )); then
    echo "Found CID mismatches:"
    for mismatch in "${mismatches[@]}"; do
      IFS=":" read -r cid expected <<<"$mismatch"
      echo "- $cid should be $expected"
    done
    has_errors=1
  fi

  if (( ${#download_failures[@]} > 0 )); then
    echo "Found download validation failures:" >&2
    for failure in "${download_failures[@]}"; do
      IFS=":" read -r cid error <<<"$failure"
      echo "- $cid: $error" >&2
    done
    has_errors=1
  fi

  if (( has_errors > 0 )); then
    return 1
  fi

  echo "All $count CID files match their contents."
  echo "All $count downloaded CIDs are valid."
}

main "$@"
