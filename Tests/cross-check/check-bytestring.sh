#!/usr/bin/env bash
set -uo pipefail
cd "$(dirname "$0")/../.."
echo "Cross-check: ByteString"
LEAN=$(lake exe hale 2>/dev/null)
pass=0; fail=0

check() {
  local label="$1" pattern="$2"
  if echo "$LEAN" | grep -qF "$pattern"; then
    echo "  PASS: $label"
    pass=$((pass+1))
  else
    echo "  FAIL: $label"
    fail=$((fail+1))
  fi
}

check "length" "bs length = 5"
check "take 3" "bs take 3 = [72, 101, 108]"
check "drop 2" "bs drop 2 = [108, 108, 111]"
check "reverse" "bs reverse = [111, 108, 108, 101, 72]"
check "elem" "bs elem 108 = true"
check "count" "bs count 108 = 2"
check "isPrefixOf" "bs isPrefixOf = true"

echo "  $pass passed, $fail failed"
exit $fail
