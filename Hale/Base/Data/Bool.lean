/-
  Hale.Base.Data.Bool — Boolean utilities

  Provides `bool` (case analysis as a function) and related utilities,
  mirroring Haskell's `Data.Bool`.
-/

namespace Data.DataBool

/-- Case analysis on `Bool` as a function.

    $$\text{bool}(x, y, b) = \begin{cases} x & \text{if } b = \text{false} \\ y & \text{if } b = \text{true} \end{cases}$$

    This is the Church encoding eliminator for `Bool`. -/
@[inline] def bool (ifFalse ifTrue : α) : Bool → α
  | false => ifFalse
  | true => ifTrue

/-- Guard: returns `[x]` if condition is true, `[]` otherwise.

    $$\text{guard'}(b, x) = \begin{cases} [x] & \text{if } b \\ [] & \text{otherwise} \end{cases}$$ -/
@[inline] def guard' (b : Bool) (x : α) : List α :=
  if b then [x] else []

-- ── Proofs ─────────────────────────────────────

/-- `bool` on `false` returns the first argument. -/
theorem bool_false (x y : α) : bool x y false = x := rfl

/-- `bool` on `true` returns the second argument. -/
theorem bool_true (x y : α) : bool x y true = y := rfl

/-- Guard on `true` returns a singleton list. -/
theorem guard'_true (x : α) : guard' true x = [x] := rfl

/-- Guard on `false` returns the empty list. -/
theorem guard'_false (x : α) : guard' false x = [] := rfl

/-- `bool` is the unique function satisfying both `bool_false` and `bool_true`. -/
theorem bool_ite (x y : α) (b : Bool) : bool x y b = if b then y else x := by
  cases b <;> rfl

end Data.DataBool
