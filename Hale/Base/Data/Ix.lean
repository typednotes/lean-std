/-
  Hale.Base.Data.Ix — Index class for array-like range operations

  Provides the `Ix` typeclass mirroring Haskell's `Data.Ix`, with instances
  for `Nat`, `Int`, `Char`, `Bool`, and products.

  ## Design

  `Ix` abstracts over types that can serve as array indices. Given a pair
  of bounds $(lo, hi)$, `range` enumerates all valid indices, `index`
  maps an index to its position, and `inRange` tests membership.

  The `index` return type carries a proof that the returned position is
  less than `rangeSize bounds`, ensuring array-safe indexing by construction.
-/

namespace Data

/-- Typeclass for indexable types supporting range enumeration.

    $$\text{Ix}(\alpha)$$ provides operations for enumerating and
    indexing over bounded ranges $[lo, hi]$.

    Corresponds to Haskell's `Data.Ix.Ix`. -/
class Ix (α : Type u) where
  /-- Enumerate all indices in the range $[lo, hi]$.
      $$\text{range}(lo, hi) = [lo, lo+1, \ldots, hi]$$ -/
  range : α × α → List α
  /-- The number of indices in the range.
      $$\text{rangeSize}(lo, hi) = |\text{range}(lo, hi)|$$ -/
  rangeSize : α × α → Nat := fun bounds => (range bounds).length
  /-- Map an index to its zero-based position in the range, bounded by `rangeSize`.
      $$\text{index}((lo, hi), i) = \begin{cases} \text{some}\langle i - lo, \_ \rangle & lo \leq i \leq hi \\ \text{none} & \text{otherwise} \end{cases}$$
      The returned index carries a proof that it is less than `rangeSize bounds`. -/
  index : (bounds : α × α) → α → Option {n : Nat // n < rangeSize bounds}
  /-- Test whether an index is within the given bounds.
      $$\text{inRange}((lo, hi), i) \iff lo \leq i \leq hi$$ -/
  inRange : α × α → α → Bool

-- ── Nat instance ────────────────────────────────

instance : Ix Nat where
  range bounds :=
    let (lo, hi) := bounds
    if lo > hi then []
    else (List.range (hi - lo + 1)).map (· + lo)
  rangeSize bounds :=
    let (lo, hi) := bounds
    if lo > hi then 0 else hi - lo + 1
  index bounds i :=
    let (lo, hi) := bounds
    if h : i >= lo && i <= hi then
      -- TODO: prove index < rangeSize for Nat instance
      some ⟨i - lo, by sorry⟩
    else none
  inRange bounds i :=
    let (lo, hi) := bounds
    i >= lo && i <= hi

-- ── Int instance ────────────────────────────────

instance : Ix Int where
  range bounds :=
    let (lo, hi) := bounds
    if lo > hi then []
    else
      let n := (hi - lo + 1).toNat
      (List.range n).map (fun i => lo + Int.ofNat i)
  rangeSize bounds :=
    let (lo, hi) := bounds
    if lo > hi then 0 else (hi - lo + 1).toNat
  index bounds i :=
    let (lo, hi) := bounds
    if h : i >= lo && i <= hi then
      -- TODO: prove index < rangeSize for Int instance
      some ⟨(i - lo).toNat, by sorry⟩
    else none
  inRange bounds i :=
    let (lo, hi) := bounds
    i >= lo && i <= hi

-- ── Char instance ───────────────────────────────

instance : Ix Char where
  range bounds :=
    let (lo, hi) := bounds
    if lo.toNat > hi.toNat then []
    else (List.range (hi.toNat - lo.toNat + 1)).map (fun i => Char.ofNat (lo.toNat + i))
  rangeSize bounds :=
    let (lo, hi) := bounds
    if lo.toNat > hi.toNat then 0 else hi.toNat - lo.toNat + 1
  index bounds c :=
    let (lo, hi) := bounds
    if h : c.toNat >= lo.toNat && c.toNat <= hi.toNat then
      -- TODO: prove index < rangeSize for Char instance
      some ⟨c.toNat - lo.toNat, by sorry⟩
    else none
  inRange bounds c :=
    let (lo, hi) := bounds
    c.toNat >= lo.toNat && c.toNat <= hi.toNat

-- ── Bool instance ───────────────────────────────

private def boolToNat : Bool → Nat
  | false => 0
  | true => 1

instance : Ix Bool where
  range bounds :=
    let (lo, hi) := bounds
    match lo, hi with
    | false, false => [false]
    | false, true  => [false, true]
    | true,  true  => [true]
    | true,  false => []
  rangeSize bounds :=
    let (lo, hi) := bounds
    match lo, hi with
    | false, false => 1
    | false, true  => 2
    | true,  true  => 1
    | true,  false => 0
  index bounds b :=
    let (lo, hi) := bounds
    if h : boolToNat b >= boolToNat lo && boolToNat b <= boolToNat hi then
      some ⟨boolToNat b - boolToNat lo, by
        -- TODO: prove index < rangeSize for Bool instance
        sorry⟩
    else none
  inRange bounds b :=
    let (lo, hi) := bounds
    boolToNat b >= boolToNat lo && boolToNat b <= boolToNat hi

-- ── Product instance ────────────────────────────

instance [Ix α] [Ix β] : Ix (α × β) where
  range bounds :=
    let ((loA, loB), (hiA, hiB)) := bounds
    let as := Ix.range (loA, hiA)
    let bs := Ix.range (loB, hiB)
    as.flatMap (fun a => bs.map (fun b => (a, b)))
  rangeSize bounds :=
    let ((loA, loB), (hiA, hiB)) := bounds
    Ix.rangeSize (loA, hiA) * Ix.rangeSize (loB, hiB)
  index bounds pair :=
    let ((loA, loB), (hiA, hiB)) := bounds
    let (a, b) := pair
    match Ix.index (loA, hiA) a, Ix.index (loB, hiB) b with
    | some ia, some ib =>
      let bSize := Ix.rangeSize (loB, hiB)
      some ⟨ia.val * bSize + ib.val, by
        -- TODO: prove ia * bSize + ib < rangeSize for Product instance
        sorry⟩
    | _, _ => none
  inRange bounds pair :=
    let ((loA, loB), (hiA, hiB)) := bounds
    let (a, b) := pair
    Ix.inRange (loA, hiA) a && Ix.inRange (loB, hiB) b

-- ── Proofs ──────────────────────────────────────

/-- `inRange` is consistent with `index`: a value is in range iff `index` returns `some`.
    $$\text{inRange}(b, x) \iff \text{index}(b, x).\text{isSome}$$
    For the `Nat` instance. -/
theorem Ix.inRange_iff_index_isSome_nat (bounds : Nat × Nat) (x : Nat) :
    Ix.inRange bounds x = (Ix.index bounds x).isSome := by
  simp [Ix.inRange, Ix.index]
  split <;> simp_all

end Data
