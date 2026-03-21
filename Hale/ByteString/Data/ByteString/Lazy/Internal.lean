/-
  Hale.ByteString.Data.ByteString.Lazy.Internal — Lazy byte strings

  Chunked, lazily-evaluated byte strings using `Thunk` for deferred evaluation.

  ## Design

  A lazy ByteString is a list of strict `ByteString` chunks, where each chunk
  is guaranteed non-empty (enforced in the `chunk` constructor). The `rest` field
  uses `Thunk` to defer evaluation, emulating Haskell's lazy evaluation.

  ## Guarantees

  - Non-empty chunk invariant: `chunk` constructor requires `bs.len > 0`
  - Laziness via `Thunk`: chunks are only materialised on demand
  - `toStrict`/`fromStrict` roundtrip identity

  ## Haskell equivalent
  `Data.ByteString.Lazy.Internal`
-/

import Hale.ByteString.Data.ByteString.Internal

namespace Data.ByteString.Lazy

/-- A lazy ByteString: a linked list of non-empty strict `ByteString` chunks
    with deferred evaluation of the tail via `Thunk`.

    $$\text{LazyByteString} = \text{nil} \mid \text{chunk}(bs, \text{rest})$$
    where $|bs| > 0$ is enforced by the constructor.

    Uses `Thunk` to emulate Haskell's lazy evaluation in Lean's strict setting. -/
inductive LazyByteString where
  /-- The empty lazy ByteString. -/
  | nil : LazyByteString
  /-- A non-empty chunk followed by a lazily-evaluated tail.
      The proof `h : bs.len > 0` ensures no empty chunks exist. -/
  | chunk (bs : Data.ByteString.ByteString) (h : bs.len > 0) (rest : Thunk LazyByteString) : LazyByteString

namespace LazyByteString

/-- The empty lazy ByteString.
    $$\text{empty} : \text{LazyByteString}$$ -/
@[inline] def empty : LazyByteString := .nil

instance : Inhabited LazyByteString := ⟨empty⟩

/-- Is this lazy ByteString empty?
    $$\text{null}(lbs) \iff lbs = \text{nil}$$ -/
@[inline] def null : LazyByteString → Bool
  | .nil => true
  | .chunk .. => false

/-- Smart constructor that drops empty chunks.
    $$\text{chunk'}(bs, rest) = \begin{cases} rest & |bs| = 0 \\ \text{chunk}(bs, rest) & |bs| > 0 \end{cases}$$ -/
def chunk' (bs : Data.ByteString.ByteString) (rest : Thunk LazyByteString) : LazyByteString :=
  if h : bs.len > 0 then .chunk bs h rest
  else rest.get

/-- Total length across all chunks. Forces all thunks. O(chunks).
    $$\text{length}(lbs) = \sum_{c \in \text{chunks}} |c|$$ -/
def length : LazyByteString → Nat
  | .nil => 0
  | .chunk bs _ rest => bs.len + rest.get.length

/-- Convert a list of strict ByteStrings to a lazy ByteString.
    Empty chunks are filtered out.
    $$\text{fromChunks}([c_1, \ldots, c_k]) = c_1 \cdot c_2 \cdot \ldots \cdot c_k$$ -/
def fromChunks : List Data.ByteString.ByteString → LazyByteString
  | [] => .nil
  | c :: cs => chunk' c (Thunk.mk fun () => fromChunks cs)

/-- Materialise all chunks into a list. Forces all thunks.
    $$\text{toChunks}(lbs) = [c_1, \ldots, c_k]$$ -/
def toChunks : LazyByteString → List Data.ByteString.ByteString
  | .nil => []
  | .chunk bs _ rest => bs :: rest.get.toChunks

/-- Convert a strict ByteString to a single-chunk lazy ByteString. O(1).
    $$\text{fromStrict}(bs) = \text{chunk}(bs, \text{nil})$$ if non-empty. -/
def fromStrict (bs : Data.ByteString.ByteString) : LazyByteString :=
  chunk' bs (Thunk.mk fun () => .nil)

/-- Force all chunks into a single strict ByteString. O(n).
    $$\text{toStrict}(lbs) = \text{concat}(\text{toChunks}(lbs))$$ -/
def toStrict (lbs : LazyByteString) : Data.ByteString.ByteString :=
  Data.ByteString.ByteString.concat (lbs.toChunks)

/-- Append two lazy ByteStrings. O(1) at the point of call (lazy tail).
    $$\text{append}(xs, ys)$$ -/
def append : LazyByteString → LazyByteString → LazyByteString
  | .nil, ys => ys
  | .chunk bs h rest, ys => .chunk bs h (Thunk.mk fun () => rest.get.append ys)

instance : Append LazyByteString where
  append := LazyByteString.append

/-- Cons a byte to the front. -/
def cons (w : UInt8) (lbs : LazyByteString) : LazyByteString :=
  let bs := Data.ByteString.ByteString.singleton w
  chunk' bs (Thunk.mk fun () => lbs)

/-- Snoc a byte to the end. -/
def snoc (lbs : LazyByteString) (w : UInt8) : LazyByteString :=
  lbs.append (fromStrict (Data.ByteString.ByteString.singleton w))

/-- The first byte, with proof of non-emptiness.
    $$\text{head}(lbs),\quad \text{requires } \neg\text{null}(lbs)$$ -/
def head : (lbs : LazyByteString) → lbs ≠ .nil → UInt8
  | .chunk bs h _, _ => bs.head h

/-- The first byte, or `none` if empty. -/
def head? : LazyByteString → Option UInt8
  | .nil => none
  | .chunk bs h _ => some (bs.head h)

/-- All bytes except the first. -/
def tail : (lbs : LazyByteString) → lbs ≠ .nil → LazyByteString
  | .chunk bs h rest, _ =>
    if _h2 : bs.len > 1 then
      let t := bs.tail h
      chunk' t rest
    else
      rest.get

/-- Uncons: decompose into head and tail. -/
def uncons : LazyByteString → Option (UInt8 × LazyByteString)
  | .nil => none
  | .chunk bs h rest =>
    let w := bs.head h
    let t := if _h2 : bs.len > 1 then chunk' (bs.tail h) rest else rest.get
    some (w, t)

/-- Left fold over all bytes, across all chunks. Forces all thunks.
    $$\text{foldl}(f, z, lbs) = f(\ldots f(f(z, w_1), w_2) \ldots, w_n)$$ -/
def foldl (f : β → UInt8 → β) (init : β) : LazyByteString → β
  | .nil => init
  | .chunk bs _ rest => rest.get.foldl f (bs.foldl f init)

/-- Right fold over all bytes, across all chunks. Forces all thunks. -/
def foldr (f : UInt8 → β → β) (init : β) : LazyByteString → β
  | .nil => init
  | .chunk bs _ rest => bs.foldr f (rest.get.foldr f init)

/-- Fold over chunks (left). -/
def foldlChunks (f : β → Data.ByteString.ByteString → β) (init : β) : LazyByteString → β
  | .nil => init
  | .chunk bs _ rest => rest.get.foldlChunks f (f init bs)

/-- Fold over chunks (right). -/
def foldrChunks (f : Data.ByteString.ByteString → β → β) (init : β) : LazyByteString → β
  | .nil => init
  | .chunk bs _ rest => f bs (rest.get.foldrChunks f init)

/-- Map a function over all bytes. -/
def map (f : UInt8 → UInt8) : LazyByteString → LazyByteString
  | .nil => .nil
  | .chunk bs _h rest =>
    let bs' := bs.map f
    -- map preserves length, so h still holds
    chunk' bs' (Thunk.mk fun () => rest.get.map f)

/-- Filter bytes satisfying a predicate. -/
def filter (p : UInt8 → Bool) : LazyByteString → LazyByteString
  | .nil => .nil
  | .chunk bs _ rest =>
    let bs' := bs.filter p
    chunk' bs' (Thunk.mk fun () => rest.get.filter p)

/-- Take the first `n` bytes. -/
def take (n : Nat) : LazyByteString → LazyByteString
  | .nil => .nil
  | _lbs@(.chunk bs h rest) =>
    if n == 0 then .nil
    else if n >= bs.len then
      .chunk bs h (Thunk.mk fun () => rest.get.take (n - bs.len))
    else
      fromStrict (bs.take n)

/-- Drop the first `n` bytes. -/
def drop (n : Nat) : LazyByteString → LazyByteString
  | .nil => .nil
  | .chunk bs _h rest =>
    if n >= bs.len then rest.get.drop (n - bs.len)
    else
      let bs' := bs.drop n
      chunk' bs' rest

/-- Split at byte position `n`. -/
def splitAt (n : Nat) (lbs : LazyByteString) : LazyByteString × LazyByteString :=
  (lbs.take n, lbs.drop n)

/-- Reverse the lazy ByteString. Forces all thunks. -/
def reverse (lbs : LazyByteString) : LazyByteString :=
  fromStrict (lbs.toStrict.reverse)

/-- Pack a list of bytes into a lazy ByteString. -/
def pack (ws : List UInt8) : LazyByteString :=
  fromStrict (Data.ByteString.ByteString.pack ws)

/-- Unpack into a list of bytes. Forces all thunks. -/
def unpack (lbs : LazyByteString) : List UInt8 :=
  lbs.toStrict.unpack

/-- True if any byte satisfies the predicate. -/
def any (p : UInt8 → Bool) : LazyByteString → Bool
  | .nil => false
  | .chunk bs _ rest => bs.any p || rest.get.any p

/-- True if all bytes satisfy the predicate. -/
def all (p : UInt8 → Bool) : LazyByteString → Bool
  | .nil => true
  | .chunk bs _ rest => bs.all p && rest.get.all p

/-- Does a byte occur in the lazy ByteString? -/
def elem (w : UInt8) (lbs : LazyByteString) : Bool := lbs.any (· == w)

/-- Concatenate a list of lazy ByteStrings. -/
def concat (lbss : List LazyByteString) : LazyByteString :=
  lbss.foldl (· ++ ·) empty

-- ── Instances ───────────────────────────────────

instance : BEq LazyByteString where
  beq a b := a.toStrict == b.toStrict

instance : Ord LazyByteString where
  compare a b := compare a.toStrict b.toStrict

instance : ToString LazyByteString where
  toString lbs := toString lbs.toStrict

instance : Repr LazyByteString where
  reprPrec lbs _ := Std.Format.text (toString lbs)

instance : Hashable LazyByteString where
  hash lbs := hash lbs.toStrict

end LazyByteString
end Data.ByteString.Lazy
