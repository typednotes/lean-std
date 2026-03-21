/-
  Hale.Base.Either — Sum type with bifunctorial API

  `Either α β` represents a value that is either `Left α` or `Right β`.
  This is Haskell's `Either`, providing a right-biased monad.

  ## Design

  While Lean has `Sum` and `Except`, `Either` provides:
  - A right-biased `Monad` instance (unlike `Except` which requires `ε` fixed)
  - `Bifunctor` instance
  - `partitionEithers` with length preservation proof
-/

import Hale.Base.Data.Bifunctor

namespace Data

/-- `Either α β` is a sum type: either `Left α` or `Right β`.

    Right-biased: `Functor`, `Monad` act on the `Right` case.
    $$\text{Either}(\alpha, \beta) \cong \alpha + \beta$$
-/
inductive Either (α : Type u) (β : Type v) where
  /-- The left case, typically representing an error or alternative. -/
  | left : α → Either α β
  /-- The right case, typically representing success. -/
  | right : β → Either α β
deriving BEq, Ord, Repr, Hashable

namespace Either

/-- Test if a value is `Left`.
    $$\text{isLeft}(x) = \begin{cases} \text{true} & \text{if } x = \text{Left}(a) \\ \text{false} & \text{if } x = \text{Right}(b) \end{cases}$$
-/
@[inline] def isLeft : Either α β → Bool
  | left _ => true
  | right _ => false

/-- Test if a value is `Right`.
    $$\text{isRight}(x) = \neg\,\text{isLeft}(x)$$
-/
@[inline] def isRight : Either α β → Bool
  | left _ => false
  | right _ => true

/-- Extract from `Left`, or return a default.
    $$\text{fromLeft}(d, \text{Left}(a)) = a, \quad \text{fromLeft}(d, \text{Right}(b)) = d$$
-/
@[inline] def fromLeft (default : α) : Either α β → α
  | left a => a
  | right _ => default

/-- Extract from `Right`, or return a default.
    $$\text{fromRight}(d, \text{Right}(b)) = b, \quad \text{fromRight}(d, \text{Left}(a)) = d$$
-/
@[inline] def fromRight (default : β) : Either α β → β
  | left _ => default
  | right b => b

/-- Case analysis: apply `f` to `Left`, `g` to `Right`.
    $$\text{either}(f, g, \text{Left}(a)) = f(a), \quad \text{either}(f, g, \text{Right}(b)) = g(b)$$
-/
@[inline] def either (f : α → γ) (g : β → γ) : Either α β → γ
  | left a => f a
  | right b => g b

/-- Map over the left component.
    $$\text{mapLeft}(f, \text{Left}(a)) = \text{Left}(f(a))$$
-/
@[inline] def mapLeft (f : α → γ) : Either α β → Either γ β
  | left a => left (f a)
  | right b => right b

/-- Map over the right component.
    $$\text{mapRight}(f, \text{Right}(b)) = \text{Right}(f(b))$$
-/
@[inline] def mapRight (f : β → γ) : Either α β → Either α γ
  | left a => left a
  | right b => right (f b)

/-- Swap `Left` and `Right`.
    $$\text{swap}(\text{Left}(a)) = \text{Right}(a), \quad \text{swap}(\text{Right}(b)) = \text{Left}(b)$$
-/
@[inline] def swap : Either α β → Either β α
  | left a => right a
  | right b => left b

/-- Partition a list of `Either` into lefts and rights.

    Given $[e_1, \ldots, e_n]$, produces $(ls, rs)$ where $ls$ are the `Left` values
    and $rs$ are the `Right` values.

    **Length preservation:** $|ls| + |rs| = n$ -/
def partitionEithers (l : List (Either α β)) : List α × List β :=
  l.foldr (fun e (ls, rs) =>
    match e with
    | left a => (a :: ls, rs)
    | right b => (ls, b :: rs)
  ) ([], [])

-- ── Proofs ─────────────────────────────────────

/-- Swapping twice is identity. -/
theorem swap_swap (e : Either α β) : e.swap.swap = e := by
  cases e <;> rfl

/-- `isLeft` and `isRight` are complementary. -/
theorem isLeft_not_isRight (e : Either α β) : e.isLeft = !e.isRight := by
  cases e <;> rfl

/-- Partition preserves total count:
    $$|\text{fst}(\text{partitionEithers}(l))| + |\text{snd}(\text{partitionEithers}(l))| = |l|$$
-/
theorem partitionEithers_length (l : List (Either α β)) :
    let (ls, rs) := partitionEithers l
    ls.length + rs.length = l.length := by
  induction l with
  | nil => rfl
  | cons e es ih =>
    simp [partitionEithers, List.foldr]
    cases e <;> simp_all [partitionEithers] <;> omega

-- Functor laws
/-- Identity law: `map id = id` for `Either`. -/
theorem map_id (e : Either α β) : Either.mapRight id e = e := by
  cases e <;> rfl

/-- Composition law: `map (f ∘ g) = map f ∘ map g` for `Either`. -/
theorem map_comp (f : γ → δ) (g : β → γ) (e : Either α β) :
    Either.mapRight (f ∘ g) e = Either.mapRight f (Either.mapRight g e) := by
  cases e <;> rfl

-- ── Instances ──────────────────────────────────

instance : Functor (Either α) where
  map := Either.mapRight

instance : Pure (Either α) where
  pure := Either.right

instance : Bind (Either α) where
  bind e f := match e with
    | left a => left a
    | right b => f b

instance : Seq (Either α) where
  seq f x := match f with
    | left a => left a
    | right g => g <$> (x ())

instance : Applicative (Either α) where

instance : Monad (Either α) where

-- Monad laws
/-- Left identity: `pure a >>= f = f a`. -/
theorem pure_bind (a : β) (f : β → Either α γ) :
    (Either.right a) >>= f = f a := rfl

/-- Right identity: `m >>= pure = m`. -/
theorem bind_pure (e : Either α β) :
    e >>= Either.right = e := by
  cases e <;> rfl

/-- Associativity: `(m >>= f) >>= g = m >>= (λ x → f x >>= g)`. -/
theorem bind_assoc (e : Either α β) (f : β → Either α γ) (g : γ → Either α δ) :
    (e >>= f) >>= g = e >>= (fun x => f x >>= g) := by
  cases e <;> rfl

instance : Bifunctor Either where
  bimap f g e := match e with
    | left a => left (f a)
    | right b => right (g b)

instance [ToString α] [ToString β] : ToString (Either α β) where
  toString
    | left a => s!"Left({a})"
    | right b => s!"Right({b})"

end Either
end Data
