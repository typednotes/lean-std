/-
  Hale.ByteString.Data.ByteString.Char8 — Character-oriented strict ByteString operations

  Thin wrapper providing `Char`-oriented API over `ByteString`. Each byte is treated
  as a Latin-1 character (truncated to 8 bits via `Char.toNat % 256`).

  ## Haskell equivalent
  `Data.ByteString.Char8` (https://hackage.haskell.org/package/bytestring/docs/Data-ByteString-Char8.html)

  ## Design
  No new types — all functions operate on `Data.ByteString.ByteString` with
  `Char ↔ UInt8` conversions (Latin-1 truncation).
-/

import Hale.ByteString.Data.ByteString.Internal

namespace Data.ByteString.Char8

open Data.ByteString

/-- Convert a `Char` to a byte (Latin-1 truncation).
    $$\text{c2w}(c) = c \bmod 256$$ -/
@[inline] private def c2w (c : Char) : UInt8 := c.toNat.toUInt8

/-- Convert a byte to a `Char` (Latin-1 interpretation).
    $$\text{w2c}(w) = \text{Char.ofNat}(w)$$ -/
@[inline] private def w2c (w : UInt8) : Char := Char.ofNat w.toNat

/-- Pack a `String` into a `ByteString` (Latin-1 truncation).
    $$\text{pack}(s) = [\text{c2w}(c) \mid c \in s]$$ -/
def pack (s : String) : ByteString :=
  ByteString.pack (s.toList.map c2w)

/-- Unpack a `ByteString` into a `String` (Latin-1 interpretation).
    $$\text{unpack}(bs) = [\text{w2c}(w) \mid w \in bs]$$ -/
def unpack (bs : ByteString) : String :=
  String.ofList (bs.unpack.map w2c)

/-- Cons a character to the front.
    $$\text{cons}(c, bs) = \text{c2w}(c) :: bs$$ -/
def cons (c : Char) (bs : ByteString) : ByteString :=
  ByteString.cons (c2w c) bs

/-- Snoc a character to the end.
    $$\text{snoc}(bs, c) = bs :: \text{c2w}(c)$$ -/
def snoc (bs : ByteString) (c : Char) : ByteString :=
  ByteString.snoc bs (c2w c)

/-- The first character, with proof of non-emptiness. -/
def head (bs : ByteString) (h : bs.len > 0) : Char :=
  w2c (bs.head h)

/-- The first character, or `none` if empty. -/
def head? (bs : ByteString) : Option Char :=
  bs.head?.map w2c

/-- The last character, with proof of non-emptiness. -/
def last (bs : ByteString) (h : bs.len > 0) : Char :=
  w2c (bs.last h)

/-- Map a character function over every byte.
    $$\text{map}(f, bs) = [\text{c2w}(f(\text{w2c}(w))) \mid w \in bs]$$ -/
def map (f : Char → Char) (bs : ByteString) : ByteString :=
  ByteString.map (fun w => c2w (f (w2c w))) bs

/-- Filter bytes whose corresponding character satisfies the predicate. -/
def filter (p : Char → Bool) (bs : ByteString) : ByteString :=
  ByteString.filter (fun w => p (w2c w)) bs

/-- Left fold with characters. -/
def foldl (f : β → Char → β) (init : β) (bs : ByteString) : β :=
  ByteString.foldl (fun acc w => f acc (w2c w)) init bs

/-- Right fold with characters. -/
def foldr (f : Char → β → β) (init : β) (bs : ByteString) : β :=
  ByteString.foldr (fun w acc => f (w2c w) acc) init bs

/-- Take characters while predicate holds. -/
def takeWhile (p : Char → Bool) (bs : ByteString) : ByteString :=
  ByteString.takeWhile (fun w => p (w2c w)) bs

/-- Drop characters while predicate holds. -/
def dropWhile (p : Char → Bool) (bs : ByteString) : ByteString :=
  ByteString.dropWhile (fun w => p (w2c w)) bs

/-- Split where character predicate first holds. -/
def «break» (p : Char → Bool) (bs : ByteString) : ByteString × ByteString :=
  ByteString.break (fun w => p (w2c w)) bs

/-- Split where character predicate first fails. -/
def span (p : Char → Bool) (bs : ByteString) : ByteString × ByteString :=
  ByteString.span (fun w => p (w2c w)) bs

/-- Does a character occur in the ByteString? -/
def elem (c : Char) (bs : ByteString) : Bool :=
  ByteString.elem (c2w c) bs

/-- Find the first character satisfying a predicate. -/
def find (p : Char → Bool) (bs : ByteString) : Option Char :=
  (ByteString.find (fun w => p (w2c w)) bs).map w2c

/-- Split a ByteString into lines (splitting on newline `\n`). -/
def lines (bs : ByteString) : List ByteString :=
  go bs bs.len []
where
  go (bs : ByteString) (fuel : Nat) (acc : List ByteString) : List ByteString :=
    match fuel with
    | 0 => acc.reverse
    | f + 1 =>
      if bs.null then acc.reverse
      else
        let (line, rest) := ByteString.«break» (· == 10) bs  -- 10 = '\n'
        match rest.uncons with
        | none => (line :: acc).reverse
        | some (_, rest') => go rest' f (line :: acc)

/-- Split a ByteString into words (splitting on whitespace). -/
def words (bs : ByteString) : List ByteString :=
  go bs bs.len []
where
  isSpace (w : UInt8) : Bool := w == 32 || w == 9 || w == 10 || w == 13
  go (bs : ByteString) (fuel : Nat) (acc : List ByteString) : List ByteString :=
    match fuel with
    | 0 => acc.reverse
    | f + 1 =>
      let bs' := ByteString.dropWhile isSpace bs
      if bs'.null then acc.reverse
      else
        let (word, rest) := ByteString.break isSpace bs'
        go rest f (word :: acc)

/-- Join lines with newline separators. -/
def unlines (bss : List ByteString) : ByteString :=
  ByteString.concat (bss.map (fun bs => ByteString.snoc bs 10))

/-- Join words with space separators. -/
def unwords (bss : List ByteString) : ByteString :=
  ByteString.intercalate (ByteString.singleton 32) bss

end Data.ByteString.Char8
