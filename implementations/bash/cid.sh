#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
BASE_DIR=$(cd -- "$SCRIPT_DIR/../.." && pwd)
EXAMPLES_DIR="$BASE_DIR/examples"
CIDS_DIR="$BASE_DIR/cids"

encode_length() {
  local length=$1
  printf '%012x' "$length" \
    | perl -ne 'chomp; print pack("H*", $_)' \
    | base64 \
    | tr '+/' '-_' \
    | tr -d '=\n'
}

compute_cid() {
  local file=$1
  local length suffix
  length=$(wc -c <"$file")
  if (( length <= 64 )); then
    suffix=$(base64 "$file" | tr '+/' '-_' | tr -d '=\n')
  else
    suffix=$(sha512sum "$file" \
      | awk '{print $1}' \
      | perl -ne 'chomp; print pack("H*", $_)' \
      | base64 \
      | tr '+/' '-_' \
      | tr -d '=\n')
  fi
  printf '%s%s\n' "$(encode_length "$length")" "$suffix"
}

export BASE_DIR
export EXAMPLES_DIR
export CIDS_DIR
export -f encode_length
export -f compute_cid
