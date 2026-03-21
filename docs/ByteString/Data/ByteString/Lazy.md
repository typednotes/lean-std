# Data.ByteString.Lazy

**Lean:** `Hale.ByteString.Data.ByteString.Lazy` | **Haskell:** `Data.ByteString.Lazy`

## Overview

Lazy byte strings built from a spine of strict `ByteString` chunks connected via `Thunk`. This emulates Haskell's lazy evaluation in Lean's strict runtime.

## Core Type

```lean
inductive LazyByteString where
  | empty : LazyByteString
  | chunk : (c : ByteString) → (h : c.len > 0) → Thunk LazyByteString → LazyByteString
```

The `h : c.len > 0` invariant guarantees that every chunk in the spine is non-empty. This rules out degenerate representations and ensures that operations like `null` and `length` behave correctly.

## Key API Mapping

| Lean | Haskell | Notes |
|------|---------|-------|
| `LazyByteString.empty` | `empty` | |
| `LazyByteString.fromStrict` | `fromStrict` | Single-chunk lazy |
| `LazyByteString.toStrict` | `toStrict` | Materialises all chunks |
| `LazyByteString.toChunks` | `toChunks` | `List ByteString` |
| `LazyByteString.fromChunks` | `fromChunks` | Filters empty chunks |
| `LazyByteString.length` | `length` | Traverses spine |
| `LazyByteString.take` | `take` | Lazy, may split a chunk |
| `LazyByteString.drop` | `drop` | Lazy |
| `LazyByteString.append` | `append` | O(1) thunk link |
| `LazyByteString.map` | `map` | Chunk-wise |
| `LazyByteString.foldlChunks` | `foldlChunks` | Fold over chunks |

## Instances

- `BEq LazyByteString`
- `Append LazyByteString` -- O(1) via thunk linking
- `Inhabited LazyByteString` -- empty

## Design Notes

- **Non-empty chunk invariant:** The proof `h : c.len > 0` on every `chunk` constructor prevents empty chunks from appearing in the spine. This simplifies reasoning about `null`, `head`, and `uncons`.
- **Thunk-based laziness:** Each tail is wrapped in `Thunk` so chunks are only forced on demand, matching Haskell's lazy list spine.
- **Chunk size:** `fromChunks` silently drops empty byte strings. Consumers should use `defaultChunkSize` (typically 32 KiB) when building lazy byte strings incrementally.

## Performance

| Operation | Complexity | Notes |
|-----------|-----------|-------|
| `append` | O(1) | Thunk link, no copying |
| `cons` | O(1) | Prepends single-byte chunk |
| `length` | O(chunks) | Must traverse spine |
| `toStrict` | O(n) | Copies all data into one array |
| `take`, `drop` | O(chunks affected) | May split one chunk |
