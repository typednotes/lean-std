/-
  Hale.Base.Data.Functor.Sum — Sum (coproduct) of two functors

  `FunctorSum F G α` is either an `F α` or a `G α`. The sum of two functors
  is itself a functor; mapping applies the function to whichever branch is present.
-/

namespace Data.Functor

/-- The **sum (coproduct) functor** $(\text{FunctorSum}\;F\;G)\;\alpha = F\;\alpha + G\;\alpha$.

    Holds either an `F α` (via `inl`) or a `G α` (via `inr`).
    Mapping over a sum maps through whichever branch is inhabited.

    $$\text{fmap}\;f\;(\text{inl}\;a) = \text{inl}\;(\text{fmap}\;f\;a)$$
    $$\text{fmap}\;f\;(\text{inr}\;b) = \text{inr}\;(\text{fmap}\;f\;b)$$ -/
inductive FunctorSum (F G : Type u → Type v) (α : Type u) where
  /-- Left injection: wraps an $F\;\alpha$. -/
  | inl : F α → FunctorSum F G α
  /-- Right injection: wraps a $G\;\alpha$. -/
  | inr : G α → FunctorSum F G α

namespace FunctorSum

/-- `Functor` instance for `FunctorSum F G`: maps through whichever branch is present.

    $$\text{fmap}\;f\;(\text{inl}\;a) = \text{inl}\;(\text{fmap}\;f\;a)$$
    $$\text{fmap}\;f\;(\text{inr}\;b) = \text{inr}\;(\text{fmap}\;f\;b)$$ -/
instance [Functor F] [Functor G] : Functor (FunctorSum F G) where
  map f
    | .inl a => .inl (f <$> a)
    | .inr b => .inr (f <$> b)

/-- **Identity law** for sum functors: $\text{fmap}\;\text{id} = \text{id}$.

    Proceeds by cases: each branch reduces to the identity law of the respective functor. -/
theorem map_id [Functor F] [Functor G]
    [LawfulFunctor F] [LawfulFunctor G]
    (x : FunctorSum F G α) :
    (id <$> x) = x := by
  cases x with
  | inl a => simp [Functor.map, id_map]
  | inr b => simp [Functor.map, id_map]

/-- **Composition law** for sum functors:
    $\text{fmap}\;(f \circ g) = \text{fmap}\;f \circ \text{fmap}\;g$.

    Proceeds by cases: each branch reduces to the composition law of the respective functor. -/
theorem map_comp [Functor F] [Functor G]
    [LawfulFunctor F] [LawfulFunctor G]
    (f : β → γ) (g : α → β) (x : FunctorSum F G α) :
    ((f ∘ g) <$> x) = (f <$> (g <$> x)) := by
  cases x with
  | inl a => simp [Functor.map, comp_map]
  | inr b => simp [Functor.map, comp_map]

end FunctorSum
end Data.Functor
