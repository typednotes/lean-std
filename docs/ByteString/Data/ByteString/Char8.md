# Data.ByteString.Char8

**Lean:** `Hale.ByteString.Data.ByteString.Char8` | **Haskell:** `Data.ByteString.Char8`

## Overview

Character-oriented operations on strict `ByteString` using the Latin-1 (ISO 8859-1) encoding. Each `Char` is truncated to its low 8 bits via `Char.toUInt8`.

This module re-exports the core `ByteString` type and provides `Char`-based wrappers around the byte-level API.

## Key API Mapping

| Lean | Haskell | Notes |
|------|---------|-------|
| `Char8.pack` | `pack` | `String → ByteString` (Latin-1) |
| `Char8.unpack` | `unpack` | `ByteString → String` |
| `Char8.singleton` | `singleton` | `Char → ByteString` |
| `Char8.head` | `head` | Returns `Char` with proof |
| `Char8.last` | `last` | Returns `Char` with proof |
| `Char8.map` | `map` | `(Char → Char) → ByteString → ByteString` |
| `Char8.filter` | `filter` | `(Char → Bool) → ByteString → ByteString` |
| `Char8.lines` | `lines` | Split on `'\n'` (byte 10) |
| `Char8.words` | `words` | Split on ASCII whitespace |
| `Char8.unlines` | `unlines` | Join with `'\n'` |
| `Char8.unwords` | `unwords` | Join with `' '` |
| `Char8.isSpace` | (helper) | ASCII whitespace check |

## Design Notes

- **Latin-1 only:** Characters above U+00FF are truncated. This matches Haskell's `Data.ByteString.Char8` semantics exactly.
- **No Unicode support:** For UTF-8 encoded byte strings, use a dedicated text library instead.
- **Shared representation:** `Char8` functions operate on the same `ByteString` type -- there is no separate `Char8ByteString`.

## Instances

No additional instances beyond those on `ByteString`. The `Char8` module provides functions, not a new type.
