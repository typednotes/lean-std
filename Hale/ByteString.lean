/-
  Hale.ByteString — Haskell `bytestring` for Lean 4

  Re-exports all ByteString sub-modules. Inspired by Haskell's `bytestring` package,
  with a maximalist approach to typing: types encode bounds proofs, non-emptiness
  invariants, and algebraic guarantees.

  ## Lean stdlib reuse
  Uses `ByteArray`, `UInt8`, `IO.FS` from Lean's stdlib. Adds slice-based O(1)
  take/drop, lazy chunked ByteStrings with Thunk, and a Builder monoid.
-/

-- Strict ByteString (slice-based, O(1) take/drop)
import Hale.ByteString.Data.ByteString
import Hale.ByteString.Data.ByteString.Short

-- Lazy ByteString (chunked, Thunk-based)
import Hale.ByteString.Data.ByteString.Lazy

-- Builder (O(1) concatenation via continuations)
import Hale.ByteString.Data.ByteString.Builder

-- Char8 (Latin-1 character-oriented wrappers)
import Hale.ByteString.Data.ByteString.Char8
import Hale.ByteString.Data.ByteString.Lazy.Char8
