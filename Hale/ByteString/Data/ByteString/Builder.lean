/-
  Hale.ByteString.Data.ByteString.Builder — Efficient ByteString construction

  Difference-list / continuation-based builder for O(1) concatenation.

  ## Design

  A `Builder` is a function `LazyByteString → LazyByteString` composed via
  function composition, giving O(1) `append`. Execution materialises a
  `LazyByteString` which can then be converted to strict if needed.

  ## Guarantees

  - Monoid laws hold trivially via function composition (`rfl`)
  - O(1) concatenation
  - O(n) materialisation

  ## Haskell equivalent
  `Data.ByteString.Builder` (https://hackage.haskell.org/package/bytestring/docs/Data-ByteString-Builder.html)
-/

import Hale.ByteString.Data.ByteString.Lazy.Internal
import Hale.ByteString.Data.ByteString.Short

namespace Data.ByteString

/-- A ByteString builder: a continuation from `LazyByteString → LazyByteString`.

    Builders compose via function composition, giving O(1) concatenation.
    $$\text{Builder} = \text{LazyByteString} \to \text{LazyByteString}$$ -/
structure Builder where
  /-- The continuation function. -/
  run : Lazy.LazyByteString → Lazy.LazyByteString

namespace Builder

-- ── Core ────────────────────────────────────────

/-- The empty builder (identity function).
    $$\text{empty}(k) = k$$ -/
@[inline] def empty : Builder := ⟨id⟩

instance : Inhabited Builder := ⟨empty⟩

/-- Append two builders via function composition. O(1).
    $$(b_1 \mathbin{+\!\!+} b_2)(k) = b_1(b_2(k))$$ -/
@[inline] def append (a b : Builder) : Builder := ⟨a.run ∘ b.run⟩

instance : Append Builder where
  append := Builder.append

-- ── Execution ───────────────────────────────────

/-- Execute a builder, producing a lazy ByteString.
    $$\text{toLazyByteString}(b) = b(\text{nil})$$ -/
@[inline] def toLazyByteString (b : Builder) : Lazy.LazyByteString :=
  b.run .nil

/-- Execute a builder, producing a strict ByteString.
    $$\text{toStrictByteString}(b) = \text{toStrict}(\text{toLazyByteString}(b))$$ -/
@[inline] def toStrictByteString (b : Builder) : ByteString :=
  b.toLazyByteString.toStrict

-- ── Primitives ──────────────────────────────────

/-- Build a single byte.
    $$\text{singleton}(w)(k) = w :: k$$ -/
def singleton (w : UInt8) : Builder :=
  ⟨fun rest => Lazy.LazyByteString.cons w rest⟩

/-- Build from a strict ByteString.
    $$\text{byteString}(bs)(k) = bs \mathbin{+\!\!+} k$$ -/
def byteString (bs : ByteString) : Builder :=
  ⟨fun rest =>
    if bs.null then rest
    else Lazy.LazyByteString.chunk' bs (Thunk.mk fun () => rest)⟩

/-- Build from a lazy ByteString.
    $$\text{lazyByteString}(lbs)(k) = lbs \mathbin{+\!\!+} k$$ -/
def lazyByteString (lbs : Lazy.LazyByteString) : Builder :=
  ⟨fun rest => lbs ++ rest⟩

/-- Build from a ShortByteString.
    $$\text{shortByteString}(sbs)(k) = \text{fromShort}(sbs) \mathbin{+\!\!+} k$$ -/
def shortByteString (sbs : ShortByteString) : Builder :=
  byteString (ShortByteString.fromShort sbs)

-- ── Numeric encodings ───────────────────────────

/-- Encode a `UInt8` as a single byte. -/
@[inline] def word8 (w : UInt8) : Builder := singleton w

/-- Encode a 16-bit value in big-endian order. -/
def word16BE (v : UInt16) : Builder :=
  let hi := (v >>> 8).toUInt8
  let lo := v.toUInt8
  singleton hi ++ singleton lo

/-- Encode a 16-bit value in little-endian order. -/
def word16LE (v : UInt16) : Builder :=
  let lo := v.toUInt8
  let hi := (v >>> 8).toUInt8
  singleton lo ++ singleton hi

/-- Encode a 32-bit value in big-endian order. -/
def word32BE (v : UInt32) : Builder :=
  let b3 := (v >>> 24).toUInt8
  let b2 := (v >>> 16).toUInt8
  let b1 := (v >>> 8).toUInt8
  let b0 := v.toUInt8
  singleton b3 ++ singleton b2 ++ singleton b1 ++ singleton b0

/-- Encode a 32-bit value in little-endian order. -/
def word32LE (v : UInt32) : Builder :=
  let b0 := v.toUInt8
  let b1 := (v >>> 8).toUInt8
  let b2 := (v >>> 16).toUInt8
  let b3 := (v >>> 24).toUInt8
  singleton b0 ++ singleton b1 ++ singleton b2 ++ singleton b3

/-- Encode a 64-bit value in big-endian order. -/
def word64BE (v : UInt64) : Builder :=
  let b7 := (v >>> 56).toUInt8
  let b6 := (v >>> 48).toUInt8
  let b5 := (v >>> 40).toUInt8
  let b4 := (v >>> 32).toUInt8
  let b3 := (v >>> 24).toUInt8
  let b2 := (v >>> 16).toUInt8
  let b1 := (v >>> 8).toUInt8
  let b0 := v.toUInt8
  singleton b7 ++ singleton b6 ++ singleton b5 ++ singleton b4 ++
  singleton b3 ++ singleton b2 ++ singleton b1 ++ singleton b0

/-- Encode a 64-bit value in little-endian order. -/
def word64LE (v : UInt64) : Builder :=
  let b0 := v.toUInt8
  let b1 := (v >>> 8).toUInt8
  let b2 := (v >>> 16).toUInt8
  let b3 := (v >>> 24).toUInt8
  let b4 := (v >>> 32).toUInt8
  let b5 := (v >>> 40).toUInt8
  let b6 := (v >>> 48).toUInt8
  let b7 := (v >>> 56).toUInt8
  singleton b0 ++ singleton b1 ++ singleton b2 ++ singleton b3 ++
  singleton b4 ++ singleton b5 ++ singleton b6 ++ singleton b7

-- ── Text encodings ──────────────────────────────

/-- Encode a character as a single byte (Latin-1 truncation).
    $$\text{char8}(c) = c \mod 256$$ -/
def char8 (c : Char) : Builder :=
  singleton (c.toNat.toUInt8)

/-- Encode a character as UTF-8. -/
def charUtf8 (c : Char) : Builder :=
  let s := String.singleton c
  let bytes := s.toUTF8
  byteString ⟨bytes, 0, bytes.size, by omega⟩

/-- Encode a string as UTF-8. -/
def stringUtf8 (s : String) : Builder :=
  let bytes := s.toUTF8
  byteString ⟨bytes, 0, bytes.size, by omega⟩

-- ── Decimal / Hex formatting ────────────────────

/-- Encode a natural number as decimal ASCII digits.
    $$\text{intDec}(42) = \text{"42"}$$ -/
def intDec (n : Int) : Builder :=
  stringUtf8 (toString n)

/-- Encode a natural number as lowercase hexadecimal ASCII.
    $$\text{wordHex}(255) = \text{"ff"}$$ -/
def wordHex (n : Nat) : Builder :=
  if n == 0 then singleton 48  -- '0'
  else
    let rec go (n : Nat) (acc : List UInt8) (fuel : Nat) : List UInt8 :=
      match fuel with
      | 0 => acc
      | f + 1 =>
        if n == 0 then acc
        else
          let digit := n % 16
          let c := if digit < 10 then (48 + digit).toUInt8 else (87 + digit).toUInt8
          go (n / 16) (c :: acc) f
    let bytes := go n [] 64
    byteString (Data.ByteString.ByteString.pack bytes)

-- ── Instances ───────────────────────────────────

instance : ToString Builder where
  toString b := toString b.toStrictByteString

-- ── Proofs (Monoid laws) ────────────────────────

/-- Left identity: `empty ++ b = b`. Trivial by `id ∘ f = f`. -/
theorem empty_append (b : Builder) : empty ++ b = b := by
  cases b with | mk f => exact congrArg Builder.mk (funext fun x => rfl)

/-- Right identity: `b ++ empty = b`. Trivial by `f ∘ id = f`. -/
theorem append_empty (b : Builder) : b ++ empty = b := by
  cases b with | mk f => exact congrArg Builder.mk (funext fun x => rfl)

/-- Associativity: `(a ++ b) ++ c = a ++ (b ++ c)`. Trivial by associativity of `∘`. -/
theorem append_assoc (a b c : Builder) : (a ++ b) ++ c = a ++ (b ++ c) := by
  cases a with | mk f => cases b with | mk g => cases c with | mk h =>
  exact congrArg Builder.mk (funext fun x => rfl)

end Builder
end Data.ByteString
