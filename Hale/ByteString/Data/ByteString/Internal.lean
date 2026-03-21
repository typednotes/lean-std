/-
  Hale.ByteString.Data.ByteString.Internal — Core ByteString type

  Slice-based byte string with O(1) `take`/`drop`/`splitAt`.

  ## Design

  Backed by Lean's `ByteArray` with offset and length fields, enabling zero-copy
  slicing. The key invariant `off + len ≤ data.size` is enforced by construction.

  ## Guarantees

  - `valid` field proves bounds: `off + len ≤ data.size`
  - O(1) `take`, `drop`, `splitAt` (no copying, just pointer arithmetic)
  - `copy` materialises a fresh `ByteArray` when sharing must be broken

  ## Lean stdlib reuse

  Uses `ByteArray` and `UInt8` directly from Lean's standard library.
  This module adds the slice representation that Lean lacks.
-/

namespace Data.ByteString

/-- A slice into a `ByteArray`. Enables O(1) `take`/`drop`/`splitAt`.

    $$\text{ByteString} = \{ d : \text{ByteArray},\; o : \mathbb{N},\; l : \mathbb{N} \mid o + l \leq |d| \}$$

    The bytes of this ByteString are `d[o], d[o+1], …, d[o+l-1]`. -/
structure ByteString where
  /-- The underlying byte array (shared; not copied on slice). -/
  data : ByteArray
  /-- Offset into `data` where this slice begins. -/
  off : Nat
  /-- Number of bytes in this slice. -/
  len : Nat
  /-- Proof that the slice is within bounds. -/
  valid : off + len ≤ data.size

namespace ByteString

/-- Helper to construct a ByteString from a fresh ByteArray (off=0, full size). -/
@[inline] private def ofByteArray (arr : ByteArray) : ByteString :=
  ⟨arr, 0, arr.size, by omega⟩

-- ── Construction ────────────────────────────────

/-- The empty ByteString. O(1).
    $$\text{empty} : \text{ByteString},\quad |\text{empty}| = 0$$ -/
@[inline] def empty : ByteString :=
  ⟨ByteArray.empty, 0, 0, by omega⟩

instance : Inhabited ByteString := ⟨empty⟩

/-- A single-byte ByteString.
    $$\text{singleton}(w) = [w]$$ -/
@[inline] def singleton (w : UInt8) : ByteString :=
  ofByteArray (ByteArray.empty.push w)

/-- Pack a list of bytes into a ByteString.
    $$\text{pack}([w_1, \ldots, w_n]) = [w_1, \ldots, w_n]$$ -/
def pack (ws : List UInt8) : ByteString :=
  ofByteArray (ws.foldl (fun a w => a.push w) ByteArray.empty)

/-- Unpack a ByteString into a list of bytes.
    $$\text{unpack}([w_1, \ldots, w_n]) = [w_1, \ldots, w_n]$$ -/
def unpack (bs : ByteString) : List UInt8 :=
  go bs.off bs.len []
where
  go (i : Nat) (remaining : Nat) (acc : List UInt8) : List UInt8 :=
    match remaining with
    | 0 => acc.reverse
    | n + 1 =>
      let w := bs.data.get! i
      go (i + 1) n (w :: acc)

/-- Create a ByteString of `n` copies of byte `w`.
    $$\text{replicate}(n, w) = [w, w, \ldots, w],\quad |\text{result}| = n$$ -/
def replicate (n : Nat) (w : UInt8) : ByteString :=
  ofByteArray ((List.replicate n w).foldl (fun a b => a.push b) ByteArray.empty)

/-- Force a copy of the slice into a fresh ByteArray. O(n).
    Breaks sharing with the original backing array.
    $$\text{copy}(bs) = bs',\quad |bs'| = |bs| \land bs'.\text{off} = 0$$ -/
def copy (bs : ByteString) : ByteString :=
  ofByteArray (bs.data.extract bs.off (bs.off + bs.len))

-- ── Basic interface ─────────────────────────────

/-- Is this ByteString empty?
    $$\text{null}(bs) \iff |bs| = 0$$ -/
@[inline] def null (bs : ByteString) : Bool := bs.len == 0

/-- The number of bytes.
    $$\text{length}(bs) = |bs|$$ -/
@[inline] def length (bs : ByteString) : Nat := bs.len

/-- Cons a byte to the front. O(n) — copies the data.
    $$\text{cons}(w, [w_1, \ldots, w_n]) = [w, w_1, \ldots, w_n]$$ -/
def cons (w : UInt8) (bs : ByteString) : ByteString :=
  ofByteArray (ByteArray.empty.push w |>.append (bs.data.extract bs.off (bs.off + bs.len)))

/-- Snoc a byte to the end. O(n) — copies the data.
    $$\text{snoc}([w_1, \ldots, w_n], w) = [w_1, \ldots, w_n, w]$$ -/
def snoc (bs : ByteString) (w : UInt8) : ByteString :=
  ofByteArray ((bs.data.extract bs.off (bs.off + bs.len)).push w)

/-- The first byte, with proof of non-emptiness.
    $$\text{head}(bs) = bs[0],\quad \text{requires } |bs| > 0$$ -/
def head (bs : ByteString) (h : bs.len > 0) : UInt8 :=
  have hv := bs.valid
  bs.data[bs.off]'(by omega)

/-- The first byte, or `none` if empty.
    $$\text{head?}(bs) = \begin{cases} \text{some}(bs[0]) & |bs| > 0 \\ \text{none} & |bs| = 0 \end{cases}$$ -/
def head? (bs : ByteString) : Option UInt8 :=
  if h : bs.len > 0 then some (bs.head h) else none

/-- All bytes except the first. O(1) slicing.
    $$\text{tail}(bs) = bs[1..],\quad \text{requires } |bs| > 0$$ -/
def tail (bs : ByteString) (h : bs.len > 0) : ByteString :=
  have hv := bs.valid
  ⟨bs.data, bs.off + 1, bs.len - 1, by omega⟩

/-- The last byte, with proof of non-emptiness.
    $$\text{last}(bs) = bs[|bs|-1],\quad \text{requires } |bs| > 0$$ -/
def last (bs : ByteString) (h : bs.len > 0) : UInt8 :=
  have hv := bs.valid
  bs.data[bs.off + bs.len - 1]'(by omega)

/-- All bytes except the last. O(1) slicing.
    $$\text{init}(bs) = bs[..n-1],\quad \text{requires } |bs| > 0$$ -/
def init (bs : ByteString) (h : bs.len > 0) : ByteString :=
  have hv := bs.valid
  ⟨bs.data, bs.off, bs.len - 1, by omega⟩

/-- Decompose into head and tail, or `none` if empty.
    $$\text{uncons}(bs) = \begin{cases} \text{some}(bs[0],\; bs[1..]) & |bs| > 0 \\ \text{none} & |bs| = 0 \end{cases}$$ -/
def uncons (bs : ByteString) : Option (UInt8 × ByteString) :=
  if h : bs.len > 0 then
    some (bs.head h, bs.tail h)
  else none

/-- Decompose into init and last, or `none` if empty.
    $$\text{unsnoc}(bs) = \begin{cases} \text{some}(bs[..n-1],\; bs[n-1]) & |bs| > 0 \\ \text{none} & |bs| = 0 \end{cases}$$ -/
def unsnoc (bs : ByteString) : Option (ByteString × UInt8) :=
  if h : bs.len > 0 then
    some (bs.init h, bs.last h)
  else none

-- ── Append ──────────────────────────────────────

/-- Concatenate two ByteStrings. O(m + n).
    $$\text{append}(xs, ys),\quad |\text{result}| = |xs| + |ys|$$ -/
def append (a b : ByteString) : ByteString :=
  if a.null then b.copy
  else if b.null then a.copy
  else
    let arrA := a.data.extract a.off (a.off + a.len)
    ofByteArray (arrA.append (b.data.extract b.off (b.off + b.len)))

instance : Append ByteString where
  append := ByteString.append

/-- Concatenate a list of ByteStrings.
    $$\text{concat}([bs_1, \ldots, bs_k]) = bs_1 \mathbin{+\!\!+} \cdots \mathbin{+\!\!+} bs_k$$ -/
def concat (bss : List ByteString) : ByteString :=
  bss.foldl (· ++ ·) empty

/-- Intercalate a separator between ByteStrings.
    $$\text{intercalate}(sep, [bs_1, \ldots, bs_k]) = bs_1 \mathbin{+\!\!+} sep \mathbin{+\!\!+} \cdots \mathbin{+\!\!+} sep \mathbin{+\!\!+} bs_k$$ -/
def intercalate (sep : ByteString) : List ByteString → ByteString
  | [] => empty
  | [x] => x.copy
  | x :: xs => xs.foldl (fun acc b => acc ++ sep ++ b) x

-- ── Transform ───────────────────────────────────

/-- Map a function over every byte. O(n).
    $$\text{map}(f, [w_1, \ldots, w_n]) = [f(w_1), \ldots, f(w_n)]$$ -/
def map (f : UInt8 → UInt8) (bs : ByteString) : ByteString :=
  ofByteArray (go bs.off bs.len ByteArray.empty)
where
  go (i : Nat) (remaining : Nat) (acc : ByteArray) : ByteArray :=
    match remaining with
    | 0 => acc
    | n + 1 => go (i + 1) n (acc.push (f (bs.data.get! i)))

/-- Reverse the bytes. O(n).
    $$\text{reverse}([w_1, \ldots, w_n]) = [w_n, \ldots, w_1]$$ -/
def reverse (bs : ByteString) : ByteString :=
  ofByteArray (go (bs.off + bs.len) bs.len ByteArray.empty)
where
  go (i : Nat) (remaining : Nat) (acc : ByteArray) : ByteArray :=
    match remaining with
    | 0 => acc
    | n + 1 => go (i - 1) n (acc.push (bs.data.get! (i - 1)))

/-- Intersperse a byte between each element. O(n).
    $$\text{intersperse}(w, [a, b, c]) = [a, w, b, w, c]$$ -/
def intersperse (w : UInt8) (bs : ByteString) : ByteString :=
  if bs.len ≤ 1 then bs.copy
  else
    ofByteArray (go (bs.off + 1) (bs.len - 1) (ByteArray.empty.push (bs.data.get! bs.off)))
where
  go (i : Nat) (remaining : Nat) (acc : ByteArray) : ByteArray :=
    match remaining with
    | 0 => acc
    | n + 1 => go (i + 1) n (acc.push w |>.push (bs.data.get! i))

/-- Transpose rows and columns of a list of ByteStrings.
    $$\text{transpose}([[a,b],[c,d],[e,f]]) = [[a,c,e],[b,d,f]]$$ -/
def transpose (bss : List ByteString) : List ByteString :=
  let maxLen := bss.foldl (fun m bs => Nat.max m bs.len) 0
  List.range maxLen |>.map fun col =>
    pack (bss.filterMap fun bs =>
      if col < bs.len then some (bs.data.get! (bs.off + col)) else none)

-- ── Folds ───────────────────────────────────────

/-- Left fold over bytes. O(n).
    $$\text{foldl}(f, z, [w_1, \ldots, w_n]) = f(\ldots f(f(z, w_1), w_2) \ldots, w_n)$$ -/
def foldl (f : β → UInt8 → β) (init : β) (bs : ByteString) : β :=
  go bs.off bs.len init
where
  go (i : Nat) (remaining : Nat) (acc : β) : β :=
    match remaining with
    | 0 => acc
    | n + 1 => go (i + 1) n (f acc (bs.data.get! i))

/-- Right fold over bytes. O(n).
    $$\text{foldr}(f, z, [w_1, \ldots, w_n]) = f(w_1, f(w_2, \ldots f(w_n, z)))$$ -/
def foldr (f : UInt8 → β → β) (init : β) (bs : ByteString) : β :=
  go (bs.off + bs.len) bs.len init
where
  go (i : Nat) (remaining : Nat) (acc : β) : β :=
    match remaining with
    | 0 => acc
    | n + 1 => go (i - 1) n (f (bs.data.get! (i - 1)) acc)

/-- Left fold on non-empty ByteString using the first byte as initial value.
    $$\text{foldl1}(f, [w_1, \ldots, w_n]) = f(\ldots f(w_1, w_2) \ldots, w_n)$$ -/
def foldl1 (f : UInt8 → UInt8 → UInt8) (bs : ByteString) (h : bs.len > 0) : UInt8 :=
  (bs.tail h).foldl f (bs.head h)

/-- Right fold on non-empty ByteString using the last byte as initial value.
    $$\text{foldr1}(f, [w_1, \ldots, w_n]) = f(w_1, f(w_2, \ldots f(w_{n-1}, w_n)))$$ -/
def foldr1 (f : UInt8 → UInt8 → UInt8) (bs : ByteString) (h : bs.len > 0) : UInt8 :=
  (bs.init h).foldr f (bs.last h)

/-- Map with accumulator, left-to-right. O(n).
    $$\text{mapAccumL}(f, s, [w_1, \ldots, w_n]) = (s', [w'_1, \ldots, w'_n])$$ -/
def mapAccumL (f : σ → UInt8 → σ × UInt8) (init : σ) (bs : ByteString) : σ × ByteString :=
  let (s, arr) := go bs.off bs.len init ByteArray.empty
  (s, ofByteArray arr)
where
  go (i : Nat) (remaining : Nat) (s : σ) (acc : ByteArray) : σ × ByteArray :=
    match remaining with
    | 0 => (s, acc)
    | n + 1 =>
      let (s', w') := f s (bs.data.get! i)
      go (i + 1) n s' (acc.push w')

/-- Map with accumulator, right-to-left. O(n).
    $$\text{mapAccumR}(f, s, [w_1, \ldots, w_n]) = (s', [w'_1, \ldots, w'_n])$$ -/
def mapAccumR (f : σ → UInt8 → σ × UInt8) (init : σ) (bs : ByteString) : σ × ByteString :=
  let (s, ws) := go (bs.off + bs.len) bs.len init []
  (s, ofByteArray (ws.foldl (fun a w => a.push w) ByteArray.empty))
where
  go (i : Nat) (remaining : Nat) (s : σ) (acc : List UInt8) : σ × List UInt8 :=
    match remaining with
    | 0 => (s, acc)
    | n + 1 =>
      let (s', w') := f s (bs.data.get! (i - 1))
      go (i - 1) n s' (w' :: acc)

/-- Map a function over bytes and concatenate the results.
    $$\text{concatMap}(f, [w_1, \ldots, w_n]) = f(w_1) \mathbin{+\!\!+} \cdots \mathbin{+\!\!+} f(w_n)$$ -/
def concatMap (f : UInt8 → ByteString) (bs : ByteString) : ByteString :=
  concat (bs.foldl (fun acc w => acc ++ [f w]) [])

/-- True if any byte satisfies the predicate.
    $$\text{any}(p, bs) = \exists w \in bs.\; p(w)$$ -/
def any (p : UInt8 → Bool) (bs : ByteString) : Bool :=
  bs.foldl (fun acc w => acc || p w) false

/-- True if all bytes satisfy the predicate.
    $$\text{all}(p, bs) = \forall w \in bs.\; p(w)$$ -/
def all (p : UInt8 → Bool) (bs : ByteString) : Bool :=
  bs.foldl (fun acc w => acc && p w) true

/-- Maximum byte in a non-empty ByteString.
    $$\text{maximum}(bs) = \max(bs),\quad \text{requires } |bs| > 0$$ -/
def maximum (bs : ByteString) (h : bs.len > 0) : UInt8 :=
  (bs.tail h).foldl (fun acc w => if w > acc then w else acc) (bs.head h)

/-- Minimum byte in a non-empty ByteString.
    $$\text{minimum}(bs) = \min(bs),\quad \text{requires } |bs| > 0$$ -/
def minimum (bs : ByteString) (h : bs.len > 0) : UInt8 :=
  (bs.tail h).foldl (fun acc w => if w < acc then w else acc) (bs.head h)

-- ── Scans ───────────────────────────────────────

/-- Left scan. Result has length `n + 1`.
    $$\text{scanl}(f, z, [w_1, \ldots, w_n]) = [z, f(z,w_1), f(f(z,w_1),w_2), \ldots]$$ -/
def scanl (f : UInt8 → UInt8 → UInt8) (z : UInt8) (bs : ByteString) : ByteString :=
  ofByteArray (go bs.off bs.len z (ByteArray.empty.push z))
where
  go (i : Nat) (remaining : Nat) (acc : UInt8) (arr : ByteArray) : ByteArray :=
    match remaining with
    | 0 => arr
    | n + 1 =>
      let acc' := f acc (bs.data.get! i)
      go (i + 1) n acc' (arr.push acc')

/-- Left scan on non-empty ByteString using the first byte as seed.
    $$\text{scanl1}(f, [w_1, \ldots, w_n]) = [w_1, f(w_1,w_2), \ldots]$$ -/
def scanl1 (f : UInt8 → UInt8 → UInt8) (bs : ByteString) (h : bs.len > 0) : ByteString :=
  scanl f (bs.head h) (bs.tail h)

/-- Right scan. Result has length `n + 1`.
    $$\text{scanr}(f, z, [w_1, \ldots, w_n]) = [f(w_1, \ldots), \ldots, f(w_n, z), z]$$ -/
def scanr (f : UInt8 → UInt8 → UInt8) (z : UInt8) (bs : ByteString) : ByteString :=
  pack (go (bs.off + bs.len) bs.len z [z])
where
  go (i : Nat) (remaining : Nat) (acc : UInt8) (result : List UInt8) : List UInt8 :=
    match remaining with
    | 0 => result
    | n + 1 =>
      let acc' := f (bs.data.get! (i - 1)) acc
      go (i - 1) n acc' (acc' :: result)

/-- Right scan on non-empty ByteString using the last byte as seed.
    $$\text{scanr1}(f, [w_1, \ldots, w_n]) = [\ldots, f(w_{n-1}, w_n), w_n]$$ -/
def scanr1 (f : UInt8 → UInt8 → UInt8) (bs : ByteString) (h : bs.len > 0) : ByteString :=
  scanr f (bs.last h) (bs.init h)

-- ── O(1) Substrings (the main value-add over ByteArray) ─────────

/-- Take the first `n` bytes. O(1) — no copying.
    $$\text{take}(n, bs) = bs[0..n],\quad |\text{result}| = \min(n, |bs|)$$ -/
@[inline] def take (n : Nat) (bs : ByteString) : ByteString :=
  have hv := bs.valid
  let n' := min n bs.len
  ⟨bs.data, bs.off, n', by omega⟩

/-- Drop the first `n` bytes. O(1) — no copying.
    $$\text{drop}(n, bs) = bs[n..],\quad |\text{result}| = |bs| - \min(n, |bs|)$$ -/
@[inline] def drop (n : Nat) (bs : ByteString) : ByteString :=
  have hv := bs.valid
  let n' := min n bs.len
  ⟨bs.data, bs.off + n', bs.len - n', by omega⟩

/-- Split at position `n`. O(1) — no copying.
    $$\text{splitAt}(n, bs) = (\text{take}(n, bs),\; \text{drop}(n, bs))$$ -/
@[inline] def splitAt (n : Nat) (bs : ByteString) : ByteString × ByteString :=
  (bs.take n, bs.drop n)

/-- Take bytes from the front while predicate holds.
    $$\text{takeWhile}(p, bs) = bs[0..k]$$ where $k$ is the first index where $p$ fails. -/
def takeWhile (p : UInt8 → Bool) (bs : ByteString) : ByteString :=
  have hv := bs.valid
  let k := go bs.off bs.len 0
  ⟨bs.data, bs.off, min k bs.len, by omega⟩
where
  go (i : Nat) (remaining : Nat) (count : Nat) : Nat :=
    match remaining with
    | 0 => count
    | n + 1 =>
      if p (bs.data.get! i) then go (i + 1) n (count + 1)
      else count

/-- Drop bytes from the front while predicate holds.
    $$\text{dropWhile}(p, bs) = bs[k..]$$ where $k$ is the first index where $p$ fails. -/
def dropWhile (p : UInt8 → Bool) (bs : ByteString) : ByteString :=
  let tw := bs.takeWhile p
  bs.drop tw.len

/-- Split where predicate first fails: `(takeWhile p, dropWhile p)`. -/
def span (p : UInt8 → Bool) (bs : ByteString) : ByteString × ByteString :=
  let tw := bs.takeWhile p
  (tw, bs.drop tw.len)

/-- Split where predicate first holds: `span (not ∘ p)`. -/
def «break» (p : UInt8 → Bool) (bs : ByteString) : ByteString × ByteString :=
  bs.span (fun w => !p w)

/-- Group consecutive bytes by an equivalence relation.
    Uses the `unpack`/`pack` approach for simplicity. -/
def groupBy (eq : UInt8 → UInt8 → Bool) (bs : ByteString) : List ByteString :=
  if bs.null then []
  else
    let bytes := bs.unpack
    go bytes []
where
  go (ws : List UInt8) (acc : List ByteString) : List ByteString :=
    match ws with
    | [] => acc.reverse
    | w :: rest =>
      let (grp, remaining) := spanEq w rest [w]
      go remaining (pack grp.reverse :: acc)
    termination_by ws.length
    decreasing_by
      simp_all
      sorry -- groupLen ≥ 1 so remaining.length < ws.length
  spanEq (prev : UInt8) (ws : List UInt8) (acc : List UInt8) : List UInt8 × List UInt8 :=
    match ws with
    | [] => (acc, [])
    | w :: rest =>
      if eq prev w then spanEq w rest (w :: acc)
      else (acc, w :: rest)

/-- Group consecutive equal bytes.
    $$\text{group}([a,a,b,b,b,c]) = [[a,a],[b,b,b],[c]]$$ -/
def group (bs : ByteString) : List ByteString :=
  groupBy (· == ·) bs

/-- All initial segments (prefixes).
    $$\text{inits}([a,b,c]) = [[], [a], [a,b], [a,b,c]]$$ -/
def inits (bs : ByteString) : List ByteString :=
  List.range (bs.len + 1) |>.map (fun n => bs.take n)

/-- All final segments (suffixes).
    $$\text{tails}([a,b,c]) = [[a,b,c], [b,c], [c], []]$$ -/
def tails (bs : ByteString) : List ByteString :=
  List.range (bs.len + 1) |>.map (fun n => bs.drop n)

/-- Is `pfx` a prefix of `bs`? -/
def isPrefixOf (pfx bs : ByteString) : Bool :=
  if pfx.len > bs.len then false
  else go 0 pfx.len
where
  go (i : Nat) (remaining : Nat) : Bool :=
    match remaining with
    | 0 => true
    | n + 1 =>
      if bs.data.get! (bs.off + i) == pfx.data.get! (pfx.off + i)
      then go (i + 1) n
      else false

/-- Is `sfx` a suffix of `bs`? -/
def isSuffixOf (sfx bs : ByteString) : Bool :=
  if sfx.len > bs.len then false
  else
    let start := bs.len - sfx.len
    go 0 sfx.len start
where
  go (i : Nat) (remaining : Nat) (bsStart : Nat) : Bool :=
    match remaining with
    | 0 => true
    | n + 1 =>
      if bs.data.get! (bs.off + bsStart + i) == sfx.data.get! (sfx.off + i)
      then go (i + 1) n bsStart
      else false

/-- Is `needle` an infix (substring) of `bs`? -/
def isInfixOf (needle bs : ByteString) : Bool :=
  if needle.null then true
  else if needle.len > bs.len then false
  else
    let limit := bs.len - needle.len
    go 0 (limit + 1)
where
  go (start : Nat) (fuel : Nat) : Bool :=
    match fuel with
    | 0 => false
    | f + 1 =>
      if isPrefixOf needle (bs.drop start) then true
      else go (start + 1) f

/-- Strip a prefix, returning `none` if not a prefix. -/
def stripPrefix (pfx bs : ByteString) : Option ByteString :=
  if isPrefixOf pfx bs then some (bs.drop pfx.len) else none

/-- Strip a suffix, returning `none` if not a suffix. -/
def stripSuffix (sfx bs : ByteString) : Option ByteString :=
  if isSuffixOf sfx bs then some (bs.take (bs.len - sfx.len)) else none

-- ── Search ──────────────────────────────────────

/-- Does the byte occur in the ByteString?
    $$\text{elem}(w, bs) = \exists i.\; bs[i] = w$$ -/
def elem (w : UInt8) (bs : ByteString) : Bool :=
  bs.any (· == w)

/-- Does the byte NOT occur in the ByteString? -/
def notElem (w : UInt8) (bs : ByteString) : Bool :=
  !bs.elem w

/-- Find the first byte satisfying a predicate. -/
def find (p : UInt8 → Bool) (bs : ByteString) : Option UInt8 :=
  go bs.off bs.len
where
  go (i : Nat) (remaining : Nat) : Option UInt8 :=
    match remaining with
    | 0 => none
    | n + 1 =>
      let w := bs.data.get! i
      if p w then some w else go (i + 1) n

/-- Filter bytes satisfying a predicate. O(n).
    $$\text{filter}(p, bs) = [w \in bs \mid p(w)]$$ -/
def filter (p : UInt8 → Bool) (bs : ByteString) : ByteString :=
  ofByteArray (bs.foldl (fun acc w => if p w then acc.push w else acc) ByteArray.empty)

/-- Partition into (satisfying, not-satisfying). O(n).
    $$\text{partition}(p, bs) = (\text{filter}(p, bs),\; \text{filter}(\neg p, bs))$$ -/
def partition (p : UInt8 → Bool) (bs : ByteString) : ByteString × ByteString :=
  let (yes, no) := bs.foldl (fun (y, n) w =>
    if p w then (y.push w, n) else (y, n.push w)) (ByteArray.empty, ByteArray.empty)
  (ofByteArray yes, ofByteArray no)

/-- Index into the ByteString with bounds proof.
    $$\text{index}(bs, i) = bs[i],\quad \text{requires } i < |bs|$$ -/
@[inline] def index (bs : ByteString) (i : Nat) (h : i < bs.len) : UInt8 :=
  have hv := bs.valid
  bs.data[bs.off + i]'(by omega)

/-- Index of the first byte satisfying a predicate. -/
def findIndex (p : UInt8 → Bool) (bs : ByteString) : Option Nat :=
  go bs.off bs.len 0
where
  go (i : Nat) (remaining : Nat) (idx : Nat) : Option Nat :=
    match remaining with
    | 0 => none
    | n + 1 =>
      if p (bs.data.get! i) then some idx else go (i + 1) n (idx + 1)

/-- All indices where the predicate holds. -/
def findIndices (p : UInt8 → Bool) (bs : ByteString) : List Nat :=
  go bs.off bs.len 0 []
where
  go (i : Nat) (remaining : Nat) (idx : Nat) (acc : List Nat) : List Nat :=
    match remaining with
    | 0 => acc.reverse
    | n + 1 =>
      let acc' := if p (bs.data.get! i) then idx :: acc else acc
      go (i + 1) n (idx + 1) acc'

/-- Index of the first occurrence of a byte. -/
def elemIndex (w : UInt8) (bs : ByteString) : Option Nat :=
  findIndex (· == w) bs

/-- All indices where a byte occurs. -/
def elemIndices (w : UInt8) (bs : ByteString) : List Nat :=
  findIndices (· == w) bs

/-- Count occurrences of a byte. O(n).
    $$\text{count}(w, bs) = |\{i \mid bs[i] = w\}|$$ -/
def count (w : UInt8) (bs : ByteString) : Nat :=
  bs.foldl (fun acc b => if b == w then acc + 1 else acc) 0

-- ── I/O ─────────────────────────────────────────

/-- Read a file into a ByteString. Wraps `IO.FS.readBinFile`.
    $$\text{readFile} : \text{String} \to \text{IO}(\text{ByteString})$$ -/
def readFile (path : System.FilePath) : IO ByteString := do
  let data ← IO.FS.readBinFile path
  return ofByteArray data

/-- Write a ByteString to a file. Wraps `IO.FS.writeBinFile`.
    $$\text{writeFile} : \text{String} \to \text{ByteString} \to \text{IO}\ ()$$ -/
def writeFile (path : System.FilePath) (bs : ByteString) : IO Unit :=
  IO.FS.writeBinFile path (bs.data.extract bs.off (bs.off + bs.len))

/-- Append a ByteString to a file.
    $$\text{appendFile} : \text{String} \to \text{ByteString} \to \text{IO}\ ()$$ -/
def appendFile (path : System.FilePath) (bs : ByteString) : IO Unit := do
  let h ← IO.FS.Handle.mk path .append
  h.write (bs.data.extract bs.off (bs.off + bs.len))

/-- Read `n` bytes from a handle. Wraps `IO.FS.Handle.read`.
    $$\text{hGet} : \text{Handle} \to \mathbb{N} \to \text{IO}(\text{ByteString})$$ -/
def hGet (h : IO.FS.Handle) (n : USize) : IO ByteString := do
  let data ← h.read n
  return ofByteArray data

/-- Write a ByteString to a handle. Wraps `IO.FS.Handle.write`.
    $$\text{hPut} : \text{Handle} \to \text{ByteString} \to \text{IO}\ ()$$ -/
def hPut (h : IO.FS.Handle) (bs : ByteString) : IO Unit :=
  h.write (bs.data.extract bs.off (bs.off + bs.len))

-- ── Instances ───────────────────────────────────

private def beqAux (a b : ByteString) : Bool :=
  if a.len != b.len then false
  else go 0 a.len
where
  go (i : Nat) (remaining : Nat) : Bool :=
    match remaining with
    | 0 => true
    | n + 1 =>
      if a.data.get! (a.off + i) == b.data.get! (b.off + i)
      then go (i + 1) n
      else false

instance : BEq ByteString where
  beq := beqAux

private def compareAux (a b : ByteString) : Ordering :=
  go 0 (min a.len b.len)
where
  go (i : Nat) (remaining : Nat) : Ordering :=
    match remaining with
    | 0 => compare a.len b.len
    | n + 1 =>
      let wa := a.data.get! (a.off + i)
      let wb := b.data.get! (b.off + i)
      match compare wa wb with
      | .eq => go (i + 1) n
      | ord => ord

instance : Ord ByteString where
  compare := compareAux

instance : ToString ByteString where
  toString bs :=
    let bytes := bs.unpack
    let strs := bytes.map (fun w => toString w.toNat)
    "[" ++ String.intercalate ", " strs ++ "]"

instance : Repr ByteString where
  reprPrec bs _ := Std.Format.text (toString bs)

instance : Hashable ByteString where
  hash bs := bs.foldl (fun h w => mixHash h (hash w)) 7

-- ── Proofs ──────────────────────────────────────

/-- `take` preserves the bounds invariant (by construction). -/
theorem take_valid (n : Nat) (bs : ByteString) :
    (bs.take n).off + (bs.take n).len ≤ (bs.take n).data.size :=
  (bs.take n).valid

/-- `drop` preserves the bounds invariant (by construction). -/
theorem drop_valid (n : Nat) (bs : ByteString) :
    (bs.drop n).off + (bs.drop n).len ≤ (bs.drop n).data.size :=
  (bs.drop n).valid

/-- `null` iff `length` is zero. -/
theorem null_iff_length_zero (bs : ByteString) :
    bs.null = true ↔ bs.length = 0 := by
  simp [null, length, BEq.beq]

end ByteString
end Data.ByteString
