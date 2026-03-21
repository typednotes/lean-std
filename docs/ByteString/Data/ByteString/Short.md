# Data.ByteString.Short

**Lean:** `Hale.ByteString.Data.ByteString.Short` | **Haskell:** `Data.ByteString.Short`

## Overview

Short byte strings: a thin newtype over `ByteArray` providing a `ByteString`-compatible API without the slice indirection. Useful when you need compact storage and do not benefit from O(1) slicing.

## Core Type

```lean
structure ShortByteString where
  data : ByteArray
```

Unlike `ByteString`, there is no offset/length -- the entire `ByteArray` is the content.

## Key API Mapping

| Lean | Haskell | Notes |
|------|---------|-------|
| `ShortByteString.empty` | `empty` | |
| `ShortByteString.pack` | `pack` | `List UInt8 → ShortByteString` |
| `ShortByteString.unpack` | `unpack` | `ShortByteString → List UInt8` |
| `ShortByteString.length` | `length` | O(1), direct `ByteArray.size` |
| `ShortByteString.index` | `index` | Bounds-checked with proof |
| `ShortByteString.toByteString` | `fromShort` | Wraps as full-offset slice |
| `ShortByteString.fromByteString` | `toShort` | Copies slice to fresh array |

## Instances

- `BEq ShortByteString`
- `Ord ShortByteString`
- `Inhabited ShortByteString` -- empty
- `ToString ShortByteString`

## When to Use

- **Use `ShortByteString`** for small, long-lived keys (e.g., hash map keys) where GC overhead of the slice representation matters.
- **Use `ByteString`** for I/O buffers and large data where O(1) slicing is valuable.

## Performance

| Operation | Complexity | Notes |
|-----------|-----------|-------|
| `length` | O(1) | Direct array size |
| `index` | O(1) | Direct array access |
| `toByteString` | O(1) | Wraps with off=0 |
| `fromByteString` | O(n) | Copies slice data |
| `pack`, `unpack` | O(n) | |
