#!/usr/bin/env bash
# Build the C FFI object files for Hale.
# Run this before `lake build` if FFI sources have changed.
#
# Usage: bash ffi/build.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
LEAN_INCLUDE="$(lean --print-prefix)/include"

cc -c -O2 -I"$LEAN_INCLUDE" "$SCRIPT_DIR/network.c" -o "$SCRIPT_DIR/network.o"

echo "Built ffi/network.o"
