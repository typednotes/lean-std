import Hale.Base.Data.List.NonEmpty

/-
  Hale.Base.Data.List — Extended list operations

  Supplements Lean's built-in `List` with operations from Haskell's `Data.List`.
  Uses `namespace List'` to avoid clashing with Lean's `List`.

  ## Design

  Lean already provides most basic list operations (`map`, `filter`, `foldl`,
  `foldr`, `zip`, `take`, `drop`, `reverse`, `append`, `length`, etc.).
  We add combinators that Lean's stdlib lacks: `nub`, `group`, `transpose`,
  `tails`, `inits`, `unfoldr`, `scanr`, `mapAccumL`, `mapAccumR`, `sortOn`,
  `maximumBy`, `minimumBy`, and set-like operations.
-/

namespace Data

namespace List'

-- ── Removing duplicates ─────────────────────────

/-- Remove duplicate elements, preserving first occurrence, using a custom equality.
    $$\text{nubBy}(eq, [x_1, \ldots, x_n])$$ keeps $x_i$ if no earlier $x_j$
    satisfies $\text{eq}(x_j, x_i)$. -/
def nubBy (eq : α → α → Bool) (l : List α) : List α :=
  go l []
where
  go : List α → List α → List α
    | [], acc => acc.reverse
    | x :: xs, acc =>
      if acc.any (eq x) then go xs acc
      else go xs (x :: acc)

/-- Remove duplicate elements using `BEq`, preserving first occurrence.
    $$\text{nub} = \text{nubBy}(\text{BEq.beq})$$ -/
def nub [BEq α] (l : List α) : List α := nubBy (· == ·) l

-- ── Grouping ────────────────────────────────────

/-- Group consecutive equal elements by a custom equality.
    $$\text{groupBy}(eq, [a, a, b, b, b, a]) = [[a, a], [b, b, b], [a]]$$ -/
def groupBy (eq : α → α → Bool) : List α → List (List α)
  | [] => []
  | x :: xs =>
    let ys := xs.takeWhile (eq x)
    let zs := xs.drop ys.length
    (x :: ys) :: groupBy eq zs
termination_by l => l.length
decreasing_by
  simp_all [List.length_cons]
  omega

/-- Group consecutive equal elements using `BEq`.
    $$\text{group} = \text{groupBy}(\text{BEq.beq})$$ -/
def group [BEq α] (l : List α) : List (List α) := groupBy (· == ·) l

-- ── Transposing ─────────────────────────────────

/-- Transpose a list of lists (rows to columns).
    $$\text{transpose}([[1,2,3],[4,5,6]]) = [[1,4],[2,5],[3,6]]$$
    Uses fuel-based termination. -/
def transpose (xss : List (List α)) : List (List α) :=
  let maxLen := xss.foldl (fun acc l => max acc l.length) 0
  go xss maxLen
where
  go : List (List α) → Nat → List (List α)
  | _, 0 => []
  | [], _ => []
  | xss, fuel + 1 =>
    let heads := xss.filterMap List.head?
    if heads.isEmpty then []
    else
      let tails := xss.map (fun l => l.drop 1)
      heads :: go tails fuel

-- ── Sublists ────────────────────────────────────

/-- All suffixes, from longest to shortest, including `[]`.
    Returns `NonEmpty` since every list has at least the empty suffix.
    $$\text{tails}([1,2,3]) = [[1,2,3],[2,3],[3],[]]$$ -/
def tails : List α → List.NonEmpty (List α)
  | [] => List.NonEmpty.singleton []
  | x :: xs => ⟨x :: xs, (tails xs).toList⟩

/-- All prefixes, from shortest to longest.
    Returns `NonEmpty` since every list has at least the empty prefix.
    $$\text{inits}([1,2,3]) = [[],[1],[1,2],[1,2,3]]$$ -/
def inits : List α → List.NonEmpty (List α)
  | [] => List.NonEmpty.singleton []
  | x :: xs => ⟨[], (inits xs).toList.map (x :: ·)⟩

/-- All subsequences (power set), including `[]`.
    $$|\text{subsequences}(l)| = 2^{|l|}$$ -/
def subsequences : List α → List (List α)
  | [] => [[]]
  | x :: xs =>
    let rest := subsequences xs
    rest ++ rest.map (x :: ·)

/-- Insert `x` at every position in `ys`. -/
private def insertEverywhere (x : α) : List α → List (List α)
  | [] => [[x]]
  | y :: ys => (x :: y :: ys) :: (insertEverywhere x ys).map (y :: ·)

/-- All permutations of a list.
    $$|\text{permutations}(l)| = |l|!$$ -/
def permutations : List α → List (List α)
  | [] => [[]]
  | x :: xs =>
    let perms := permutations xs
    perms.flatMap (insertEverywhere x)

-- ── Building ────────────────────────────────────

/-- Build a list from a seed by repeatedly applying `f`.
    $$\text{unfoldr}(f, b_0) = [a_1, a_2, \ldots]$$
    where $f(b_i) = \text{some}(a_{i+1}, b_{i+1})$ until $f(b_n) = \text{none}$.
    Fuel-limited to ensure termination. -/
def unfoldr (f : β → Option (α × β)) (seed : β) (fuel : Nat := 10000) : List α :=
  match fuel with
  | 0 => []
  | fuel + 1 =>
    match f seed with
    | none => []
    | some (a, seed') => a :: unfoldr f seed' fuel

-- ── Scans ───────────────────────────────────────

/-- Right-to-left scan, producing all intermediate accumulators.
    Returns `NonEmpty` since the result always includes at least the initial accumulator `z`.
    $$\text{scanr}(f, z, [x_1, \ldots, x_n]) = [f(x_1, f(x_2, \ldots f(x_n, z))), \ldots, f(x_n, z), z]$$ -/
def scanr (f : α → β → β) (z : β) : List α → List.NonEmpty β
  | [] => List.NonEmpty.singleton z
  | x :: xs =>
    let rest := scanr f z xs
    ⟨f x rest.head, rest.toList⟩

-- ── Accumulating maps ───────────────────────────

/-- Left-to-right map with accumulator.
    $$\text{mapAccumL}(f, s_0, [x_1, \ldots, x_n]) = (s_n, [y_1, \ldots, y_n])$$
    where $(s_i, y_i) = f(s_{i-1}, x_i)$. -/
def mapAccumL (f : σ → α → σ × β) (init : σ) : List α → σ × List β
  | [] => (init, [])
  | x :: xs =>
    let (s', y) := f init x
    let (s'', ys) := mapAccumL f s' xs
    (s'', y :: ys)

/-- Right-to-left map with accumulator.
    $$\text{mapAccumR}(f, s_0, [x_1, \ldots, x_n]) = (s_n, [y_1, \ldots, y_n])$$
    where processing goes right-to-left. -/
def mapAccumR (f : σ → α → σ × β) (init : σ) : List α → σ × List β
  | [] => (init, [])
  | x :: xs =>
    let (s', ys) := mapAccumR f init xs
    let (s'', y) := f s' x
    (s'', y :: ys)

-- ── Joining ─────────────────────────────────────

/-- Intercalate: insert a separator between lists and flatten.
    $$\text{intercalate}(sep, [l_1, \ldots, l_n]) = l_1 \mathbin{++} sep \mathbin{++} l_2 \mathbin{++} \cdots \mathbin{++} l_n$$ -/
def intercalate (sep : List α) : List (List α) → List α
  | [] => []
  | [x] => x
  | x :: xs => x ++ sep ++ intercalate sep xs

-- ── Sorting ─────────────────────────────────────

/-- Sort by a derived key.
    $$\text{sortOn}(f, l)$$ sorts $l$ by comparing $f(x)$ values. -/
def sortOn [Ord β] (f : α → β) (l : List α) : List α :=
  l.toArray.qsort (fun a b => compare (f a) (f b) == .lt) |>.toList

-- ── Extrema ─────────────────────────────────────

/-- Maximum element by a custom comparator, or `none` for empty lists.
    $$\text{maximumBy}(\text{cmp}, l)$$ -/
def maximumBy (cmp : α → α → Ordering) : List α → Option α
  | [] => none
  | x :: xs => some (xs.foldl (fun acc y => if cmp acc y == .lt then y else acc) x)

/-- Minimum element by a custom comparator, or `none` for empty lists.
    $$\text{minimumBy}(\text{cmp}, l)$$ -/
def minimumBy (cmp : α → α → Ordering) : List α → Option α
  | [] => none
  | x :: xs => some (xs.foldl (fun acc y => if cmp acc y == .gt then y else acc) x)

-- ── Deletion / Set operations ───────────────────

/-- Delete the first occurrence matching the predicate.
    $$\text{deleteBy}(eq, x, l)$$ removes the first $y$ in $l$ with $\text{eq}(x, y)$. -/
def deleteBy (eq : α → α → Bool) (x : α) : List α → List α
  | [] => []
  | y :: ys => if eq x y then ys else y :: deleteBy eq x ys

/-- List union by a custom equality.
    $$\text{unionBy}(eq, l_1, l_2)$$ appends elements of $l_2$ not in $l_1$. -/
def unionBy (eq : α → α → Bool) (xs ys : List α) : List α :=
  xs ++ ys.filter (fun y => !xs.any (eq y))

/-- List intersection by a custom equality.
    $$\text{intersectBy}(eq, l_1, l_2)$$ keeps elements of $l_1$ that are in $l_2$. -/
def intersectBy (eq : α → α → Bool) (xs ys : List α) : List α :=
  xs.filter (fun x => ys.any (eq x))

/-- Insert into a sorted list, maintaining order.
    $$\text{insertBy}(\text{cmp}, x, l)$$ places $x$ before the first element
    greater than it. -/
def insertBy (cmp : α → α → Ordering) (x : α) : List α → List α
  | [] => [x]
  | y :: ys => if cmp x y != .gt then x :: y :: ys else y :: insertBy cmp x ys

/-- `genericLength` is `List.length` in Lean (which already returns `Nat`).
    $$\text{genericLength}(l) = |l|$$ -/
@[inline] def genericLength (l : List α) : Nat := l.length

-- ── Proofs ──────────────────────────────────────

/-- The `tails` function produces `length + 1` suffixes (as a `NonEmpty`).
    $$|\text{tails}(l).\text{toList}| = |l| + 1$$ -/
theorem tails_length (l : List α) : (tails l).toList.length = l.length + 1 := by
  induction l with
  | nil => rfl
  | cons _ xs ih =>
    unfold tails
    simp only [List.NonEmpty.toList, List.length_cons]
    have : (tails xs).tail.length + 1 = (tails xs).toList.length := by
      simp [List.NonEmpty.toList]
    omega

/-- The `inits` function produces `length + 1` prefixes (as a `NonEmpty`).
    $$|\text{inits}(l).\text{toList}| = |l| + 1$$ -/
theorem inits_length (l : List α) : (inits l).toList.length = l.length + 1 := by
  induction l with
  | nil => rfl
  | cons _ xs ih =>
    unfold inits
    simp only [List.NonEmpty.toList, List.length_cons, List.length_map]
    have : (inits xs).tail.length + 1 = (inits xs).toList.length := by
      simp [List.NonEmpty.toList]
    omega

/-- `tails` of the empty list is the singleton `[[]]`. -/
theorem tails_nil : tails ([] : List α) = List.NonEmpty.singleton [] := rfl

/-- `inits` of the empty list is the singleton `[[]]`. -/
theorem inits_nil : inits ([] : List α) = List.NonEmpty.singleton [] := rfl

end List'
end Data
