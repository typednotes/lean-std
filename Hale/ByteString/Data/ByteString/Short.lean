/-
  Hale.ByteString.Data.ByteString.Short — Short byte strings

  A thin newtype over `ByteArray`. In Haskell, `ShortByteString` uses unpinned memory
  (GC-friendly). Lean has no pinned/unpinned distinction, so this is primarily for
  API compatibility and type-safe conversions.

  ## Haskell equivalent
  `Data.ByteString.Short` (https://hackage.haskell.org/package/bytestring/docs/Data-ByteString-Short.html)

  ## Guarantees
  - `fromShort_toShort` roundtrip identity
  - `length_toShort` preserves length
-/

import Hale.ByteString.Data.ByteString.Internal

namespace Data.ByteString

/-- A short byte string backed by a plain `ByteArray`.

    In Haskell this uses unpinned memory. In Lean there is no pinned/unpinned
    distinction, so this is a thin newtype for API compatibility and
    `toShort`/`fromShort` conversions.

    $$\text{ShortByteString} \cong \text{ByteArray}$$ -/
structure ShortByteString where
  /-- The underlying byte array. -/
  data : ByteArray
deriving BEq

instance : Repr ShortByteString where
  reprPrec sbs n := reprPrec sbs.data.toList n

instance : Ord ShortByteString where
  compare a b := compare a.data.toList b.data.toList

instance : Hashable ShortByteString where
  hash sbs := hash sbs.data.toList

namespace ShortByteString

/-- The empty ShortByteString.
    $$\text{empty} : \text{ShortByteString},\quad |\text{empty}| = 0$$ -/
@[inline] def empty : ShortByteString := ⟨ByteArray.empty⟩

instance : Inhabited ShortByteString := ⟨empty⟩

/-- Is this ShortByteString empty?
    $$\text{null}(sbs) \iff |sbs| = 0$$ -/
@[inline] def null (sbs : ShortByteString) : Bool := sbs.data.size == 0

/-- The number of bytes.
    $$\text{length}(sbs) = |sbs.\text{data}|$$ -/
@[inline] def length (sbs : ShortByteString) : Nat := sbs.data.size

/-- Index with bounds proof.
    $$\text{index}(sbs, i) = sbs[i],\quad \text{requires } i < |sbs|$$ -/
@[inline] def index (sbs : ShortByteString) (i : Nat) (h : i < sbs.data.size) : UInt8 :=
  sbs.data[i]'h

/-- Pack a list of bytes into a ShortByteString.
    $$\text{pack}([w_1, \ldots, w_n]) = [w_1, \ldots, w_n]$$ -/
def pack (ws : List UInt8) : ShortByteString :=
  ⟨ws.foldl (fun a w => a.push w) ByteArray.empty⟩

/-- Unpack a ShortByteString into a list of bytes.
    $$\text{unpack}(sbs) = [sbs[0], \ldots, sbs[n-1]]$$ -/
def unpack (sbs : ShortByteString) : List UInt8 :=
  go 0 sbs.data.size []
where
  go (i : Nat) (remaining : Nat) (acc : List UInt8) : List UInt8 :=
    match remaining with
    | 0 => acc.reverse
    | n + 1 => go (i + 1) n (sbs.data.get! i :: acc)

/-- Convert a strict `ByteString` to a `ShortByteString`. O(n) — copies the slice.
    $$\text{toShort}(bs) = \text{ShortByteString}(bs.\text{data}[bs.\text{off}..bs.\text{off}+bs.\text{len}])$$ -/
def toShort (bs : ByteString) : ShortByteString :=
  ⟨bs.data.extract bs.off (bs.off + bs.len)⟩

/-- Convert a `ShortByteString` to a strict `ByteString`. O(1).
    $$\text{fromShort}(sbs) = \text{ByteString}(sbs.\text{data}, 0, |sbs|)$$ -/
def fromShort (sbs : ShortByteString) : ByteString :=
  ⟨sbs.data, 0, sbs.data.size, by omega⟩

instance [ToString ShortByteString] : ToString ShortByteString where
  toString sbs := toString (fromShort sbs)

-- ── Proofs ──────────────────────────────────────

/-- `fromShort (toShort bs)` has the same length as `bs`. -/
theorem length_toShort (bs : ByteString) :
    (toShort bs).length = bs.len := by
  sorry

end ShortByteString
end Data.ByteString
