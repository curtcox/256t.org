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

compute_cid_from_content() {
  local content=$1
  local length suffix
  length=${#content}
  if (( length <= 64 )); then
    suffix=$(printf '%s' "$content" | base64 | tr '+/' '-_' | tr -d '=\n')
  else
    suffix=$(printf '%s' "$content" | sha512sum \
      | awk '{print $1}' \
      | perl -ne 'chomp; print pack("H*", $_)' \
      | base64 \
      | tr '+/' '-_' \
      | tr -d '=\n')
  fi
  printf '%s%s\n' "$(encode_length "$length")" "$suffix"
}

download_cid() {
  local base_url=$1
  local cid=$2
  local url="${base_url%/}/$cid"
  local content
  
  if ! content=$(curl -sS -f "$url" 2>&1); then
    echo "error:$content" >&2
    return 1
  fi
  
  local computed
  computed=$(compute_cid_from_content "$content")
  local is_valid
  if [[ "$computed" == "$cid" ]]; then
    is_valid="true"
  else
    is_valid="false"
  fi
  
  echo "$content|$computed|$is_valid"
}

export BASE_DIR
export EXAMPLES_DIR
export CIDS_DIR
export -f encode_length
export -f compute_cid
export -f compute_cid_from_content
export -f download_cid
