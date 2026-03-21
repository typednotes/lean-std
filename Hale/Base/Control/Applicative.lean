/-
  Hale.Base.Control.Applicative — Utility functions for Alternative

  Lean 4 already provides the `Alternative` typeclass. This module ports
  the utility combinators from Haskell's `Control.Applicative` that work
  with `Alternative`.

  Note: `many` and `some` are omitted because they require laziness and
  would infinite-loop in strict Lean.
-/

namespace Control.Applicative

/-- Try an action, returning `some` on success or `none` on failure.

    $$\text{optional}\;fa = (\text{some} \mathbin{<\$>} fa) \mathbin{<|>} \text{pure}\;\text{none}$$ -/
def optional [Alternative f] (fa : f α) : f (Option α) :=
  (some <$> fa) <|> pure none

/-- Fold a list of alternatives with `<|>`, starting from `failure`.

    $$\text{asum}\;[a_1, \ldots, a_n] = a_1 \mathbin{<|>} \cdots \mathbin{<|>} a_n \mathbin{<|>} \text{failure}$$ -/
def asum [Alternative f] : List (f α) → f α
  | [] => failure
  | x :: xs => x <|> asum xs

/-- Conditional failure: succeeds with `()` if the boolean is true,
    fails otherwise.

    $$\text{guard}\;b = \begin{cases} \text{pure}\;() & \text{if } b \\ \text{failure} & \text{otherwise} \end{cases}$$ -/
def guard [Alternative f] (b : Bool) : f Unit :=
  if b then pure () else failure

end Control.Applicative
