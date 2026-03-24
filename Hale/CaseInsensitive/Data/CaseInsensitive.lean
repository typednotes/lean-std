/-
  Hale.CaseInsensitive.Data.CaseInsensitive — Case-insensitive string comparison

  Provides a wrapper type `CI α` that compares values case-insensitively.
  The original value is preserved for display, while a case-folded copy
  is used for equality and hashing.

  ## Design

  Mirrors Haskell's `Data.CaseInsensitive`. The `CI` type stores both
  the `original` value and a pre-computed `foldedCase` value. Equality,
  ordering, and hashing use only `foldedCase`.

  ## Guarantees

  - `BEq` and `Hashable` are consistent: equal values have equal hashes
  - `original` is preserved exactly as provided
  - `ToString` uses `original` (round-trips display)
-/

namespace Data

/-- Case-folding typeclass. Provides a function to fold a value to a canonical case.
    $$\text{foldCase} : \alpha \to \alpha$$ -/
class FoldCase (α : Type) where
  foldCase : α → α

/-- A case-insensitive wrapper. Stores the original value and a pre-computed
    case-folded version. Equality and hashing compare only the folded form.

    $$\text{CI}(\alpha) = \{ \text{original} : \alpha,\; \text{foldedCase} : \alpha \}$$

    Invariant: `foldedCase = FoldCase.foldCase original` -/
structure CI (α : Type) [FoldCase α] where
  protected mk ::
  /-- The original, unmodified value. -/
  original : α
  /-- The case-folded value, used for comparison. -/
  foldedCase : α
  /-- Invariant: `foldedCase` is always the case-folded form of `original`. -/
  inv : foldedCase = FoldCase.foldCase original

namespace CI

/-- Smart constructor that computes the folded form automatically.
    $$\text{mk'}(x) = \text{CI}(x, \text{foldCase}(x))$$ -/
@[inline] def mk' [FoldCase α] (x : α) : CI α :=
  CI.mk x (FoldCase.foldCase x) rfl

/-- Map a function over both original and folded values.
    $$\text{map}(f, \text{CI}(o, fc)) = \text{CI}(f(o), \text{foldCase}(f(o)))$$ -/
@[inline] def map [FoldCase α] [FoldCase β] (f : α → β) (ci : CI α) : CI β :=
  mk' (f ci.original)

instance [FoldCase α] [BEq α] : BEq (CI α) where
  beq a b := a.foldedCase == b.foldedCase

instance [FoldCase α] [Hashable α] : Hashable (CI α) where
  hash ci := hash ci.foldedCase

instance [FoldCase α] [Ord α] : Ord (CI α) where
  compare a b := compare a.foldedCase b.foldedCase

instance [FoldCase α] [ToString α] : ToString (CI α) where
  toString ci := toString ci.original

instance [FoldCase α] [Repr α] : Repr (CI α) where
  reprPrec ci n := reprPrec ci.original n

-- FoldCase instances

instance : FoldCase String where
  foldCase s := s.toLower

instance : FoldCase Char where
  foldCase c := c.toLower

-- Proofs

/-- Two CI values are equal iff their folded cases are equal. -/
theorem ci_eq_iff [FoldCase α] [BEq α] (a b : CI α) : (a == b) = (a.foldedCase == b.foldedCase) := rfl

end CI
end Data
