# Data.ByteString

**Lean:** `Hale.ByteString.Data.ByteString` | **Haskell:** `Data.ByteString`

## Overview

Strict byte strings with O(1) slicing. The core type is a slice into a `ByteArray`:

```lean
structure ByteString where
  data : ByteArray
  off : Nat
  len : Nat
  valid : off + len ≤ data.size
```

## Key API Mapping

| Lean | Haskell | Notes |
|------|---------|-------|
| `ByteString.empty` | `empty` | O(1) |
| `ByteString.singleton` | `singleton` | |
| `ByteString.pack` | `pack` | `List UInt8 → ByteString` |
| `ByteString.unpack` | `unpack` | `ByteString → List UInt8` |
| `ByteString.take` | `take` | **O(1)** slice |
| `ByteString.drop` | `drop` | **O(1)** slice |
| `ByteString.splitAt` | `splitAt` | **O(1)** slice |
| `ByteString.head h` | `head` | Total with proof `h : len > 0` |
| `ByteString.tail h` | `tail` | O(1) slice with proof |
| `ByteString.last h` | `last` | Total with proof |
| `ByteString.init h` | `init` | O(1) slice with proof |
| `ByteString.index i h` | `index` | Bounds-checked with proof `h : i < len` |
| `ByteString.foldl` | `foldl'` | Strict left fold |
| `ByteString.foldr` | `foldr` | Right fold |
| `ByteString.map` | `map` | O(n) |
| `ByteString.reverse` | `reverse` | O(n) |
| `ByteString.filter` | `filter` | O(n) |
| `ByteString.isPrefixOf` | `isPrefixOf` | |
| `ByteString.isSuffixOf` | `isSuffixOf` | |
| `ByteString.isInfixOf` | `isInfixOf` | |
| `ByteString.readFile` | `readFile` | Wraps `IO.FS.readBinFile` |
| `ByteString.writeFile` | `writeFile` | Wraps `IO.FS.writeBinFile` |

## Instances

- `BEq ByteString` -- byte-by-byte comparison
- `Ord ByteString` -- lexicographic ordering
- `Append ByteString` -- concatenation (O(m+n))
- `ToString ByteString` -- `[w1, w2, ...]` format
- `Hashable ByteString`
- `Inhabited ByteString` -- empty

## Proofs

- `take_valid` / `drop_valid` -- slice operations preserve bounds
- `null_iff_length_zero` -- `null <-> length = 0`

## Performance

| Operation | Complexity | Notes |
|-----------|-----------|-------|
| `take`, `drop`, `splitAt` | O(1) | Zero-copy slice |
| `head`, `last`, `index` | O(1) | Direct array access |
| `tail`, `init` | O(1) | Slice adjustment |
| `cons`, `snoc`, `append` | O(n) | Copies data |
| `map`, `reverse`, `filter` | O(n) | |
| `copy` | O(n) | Materialises fresh array |
