/-
  Hale.ByteString.Data.ByteString — Strict byte strings

  Re-exports `Data.ByteString.Internal` as the public API for strict ByteStrings.

  ## Haskell equivalent
  `Data.ByteString` (https://hackage.haskell.org/package/bytestring/docs/Data-ByteString.html)

  ## Design
  The `ByteString` type is a slice into a `ByteArray` with offset and length,
  enabling O(1) `take`/`drop`/`splitAt`. This is the main value-add over Lean's
  built-in `ByteArray` which copies on `extract`.

  ## Lean stdlib reuse
  Uses `ByteArray`, `UInt8`, `IO.FS.readBinFile`/`writeBinFile` from Lean's stdlib.
-/
import Hale.ByteString.Data.ByteString.Internal
