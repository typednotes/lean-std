# Data.ByteString.Builder

**Lean:** `Hale.ByteString.Data.ByteString.Builder` | **Haskell:** `Data.ByteString.Builder`

## Overview

Continuation-based builder for efficient incremental construction of byte strings. The builder type wraps a difference-list style function, achieving O(1) concatenation.

## Core Type

```lean
structure Builder where
  run : LazyByteString → LazyByteString
```

A `Builder` is a function that prepends its content onto a continuation `LazyByteString`. Two builders compose by function composition, giving O(1) `append`.

## Key API Mapping

| Lean | Haskell | Notes |
|------|---------|-------|
| `Builder.empty` | `mempty` | Identity builder |
| `Builder.append` | `<>` / `mappend` | O(1) composition |
| `Builder.singleton` | `word8` | Single byte |
| `Builder.byteString` | `byteString` | From strict `ByteString` |
| `Builder.lazyByteString` | `lazyByteString` | From lazy `ByteString` |
| `Builder.toLazyByteString` | `toLazyByteString` | Execute the builder |
| `Builder.word16BE` | `word16BE` | Big-endian 16-bit |
| `Builder.word16LE` | `word16LE` | Little-endian 16-bit |
| `Builder.word32BE` | `word32BE` | Big-endian 32-bit |
| `Builder.word32LE` | `word32LE` | Little-endian 32-bit |
| `Builder.word64BE` | `word64BE` | Big-endian 64-bit |
| `Builder.word64LE` | `word64LE` | Little-endian 64-bit |
| `Builder.intDec` | `intDec` | Decimal integer encoding |

## Monoid Law Proofs

The module proves that `Builder` forms a lawful monoid:

- **`empty_append`:** `Builder.empty ++ b = b`
- **`append_empty`:** `b ++ Builder.empty = b`
- **`append_assoc`:** `(a ++ b) ++ c = a ++ (b ++ c)`

These follow directly from function composition laws.

## Usage Pattern

```lean
let b := Builder.byteString header
     ++ Builder.word32BE payloadLen
     ++ Builder.byteString payload
let result := Builder.toLazyByteString b
```

## Performance

| Operation | Complexity | Notes |
|-----------|-----------|-------|
| `append` | O(1) | Function composition |
| `singleton`, `byteString` | O(1) | Wraps input |
| `toLazyByteString` | O(n) | Executes all builders |
