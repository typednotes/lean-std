/-
  Hale.Base.Data.Maybe — Option utilities with Haskell-compatible API

  Provides Haskell `Data.Maybe` functions over Lean's `Option` type.
  In Haskell, `Maybe a = Nothing | Just a`. In Lean, `Option α = none | some α`.
-/

namespace Data.Maybe

/-- Case analysis on `Option` (Haskell's `maybe`).

    $$\text{maybe}(b, f, \text{none}) = b, \quad \text{maybe}(b, f, \text{some}\;a) = f(a)$$ -/
@[inline] def maybe (b : β) (f : α → β) : Option α → β
  | none => b
  | some a => f a

/-- Extract from `Option` with a default (Haskell's `fromMaybe`).

    $$\text{fromMaybe}(d, \text{none}) = d, \quad \text{fromMaybe}(d, \text{some}\;a) = a$$ -/
@[inline] def fromMaybe (d : α) : Option α → α
  | none => d
  | some a => a

/-- Test if `some` (Haskell's `isJust`). -/
@[inline] def isJust : Option α → Bool := Option.isSome

/-- Test if `none` (Haskell's `isNothing`). -/
@[inline] def isNothing : Option α → Bool := Option.isNone

/-- Collect `some` values from a list (Haskell's `catMaybes`).

    $$\text{catMaybes}([x_1, \ldots, x_n]) = [a \mid x_i = \text{some}\;a]$$ -/
def catMaybes : List (Option α) → List α
  | [] => []
  | none :: rest => catMaybes rest
  | some a :: rest => a :: catMaybes rest

/-- Map and filter in one pass (Haskell's `mapMaybe`).

    $$\text{mapMaybe}(f, xs) = \text{catMaybes}(\text{map}\;f\;xs)$$ -/
def mapMaybe (f : α → Option β) : List α → List β
  | [] => []
  | x :: rest =>
    match f x with
    | none => mapMaybe f rest
    | some b => b :: mapMaybe f rest

/-- First element or `none` (Haskell's `listToMaybe`). -/
@[inline] def listToMaybe : List α → Option α
  | [] => none
  | x :: _ => some x

/-- `Option` to singleton or empty list (Haskell's `maybeToList`). -/
@[inline] def maybeToList : Option α → List α
  | none => []
  | some a => [a]

/-- Safe unwrap with proof that value is `some` (Haskell's `fromJust` made safe).

    $$\text{fromJust}(\text{some}\;a, \_) = a$$ -/
def fromJust : (o : Option α) → o.isSome = true → α
  | some a, _ => a

-- ── Proofs ─────────────────────────────────────

/-- `maybe` on `none` returns the default. -/
theorem maybe_none (b : β) (f : α → β) : maybe b f none = b := rfl

/-- `maybe` on `some` applies the function. -/
theorem maybe_some (b : β) (f : α → β) (a : α) : maybe b f (some a) = f a := rfl

/-- `fromMaybe` on `none` returns the default. -/
theorem fromMaybe_none (d : α) : fromMaybe d none = d := rfl

/-- `fromMaybe` on `some` extracts the value. -/
theorem fromMaybe_some (d a : α) : fromMaybe d (some a) = a := rfl

/-- `catMaybes` of empty list is empty. -/
theorem catMaybes_nil : catMaybes ([] : List (Option α)) = [] := rfl

/-- `mapMaybe` of empty list is empty. -/
theorem mapMaybe_nil (f : α → Option β) : mapMaybe f [] = [] := rfl

/-- `maybeToList` / `listToMaybe` roundtrip. -/
theorem maybeToList_listToMaybe (o : Option α) :
    listToMaybe (maybeToList o) = o := by
  cases o <;> rfl

/-- `catMaybes` is `mapMaybe id`. -/
theorem catMaybes_eq_mapMaybe_id (l : List (Option α)) :
    catMaybes l = mapMaybe id l := by
  induction l with
  | nil => rfl
  | cons x rest ih =>
    cases x <;> simp [catMaybes, mapMaybe, id, ih]

end Data.Maybe
