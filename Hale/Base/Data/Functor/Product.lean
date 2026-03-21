/-
  Hale.Base.Data.Functor.Product — Product of two functors

  `Product F G α` pairs an `F α` with a `G α`. The product of two functors
  is itself a functor; mapping applies the function to both components.
-/

namespace Data.Functor

/-- The **product functor** $(\text{Product}\;F\;G)\;\alpha = F\;\alpha \times G\;\alpha$.

    Pairs two functorial values at the same type parameter.
    Mapping over a product maps through both components independently.

    $$\text{fmap}\;f\;(\text{Product}\;(a, b)) = \text{Product}\;(\text{fmap}\;f\;a,\;\text{fmap}\;f\;b)$$ -/
structure Product (F G : Type u → Type v) (α : Type u) where
  /-- Unwrap the product to a pair: $F\;\alpha \times G\;\alpha$. -/
  runProduct : F α × G α

namespace Product

/-- `Functor` instance for `Product F G`: maps through both components.

    $$\text{fmap}\;f\;(\text{Product}\;(a, b)) = \text{Product}\;(\text{fmap}\;f\;a,\;\text{fmap}\;f\;b)$$ -/
instance [Functor F] [Functor G] : Functor (Product F G) where
  map f p := ⟨(f <$> p.runProduct.1, f <$> p.runProduct.2)⟩

/-- `BEq` instance for `Product F G α`: both components must be equal.

    $$(\text{Product}\;(a_1, b_1)) = (\text{Product}\;(a_2, b_2)) \iff a_1 = a_2 \land b_1 = b_2$$ -/
instance [BEq (F α)] [BEq (G α)] : BEq (Product F G α) where
  beq a b := a.runProduct.1 == b.runProduct.1 && a.runProduct.2 == b.runProduct.2

/-- **Identity law** for product functors: $\text{fmap}\;\text{id} = \text{id}$.

    If $F$ and $G$ are both lawful functors, their product is also lawful. -/
theorem map_id [Functor F] [Functor G]
    [LawfulFunctor F] [LawfulFunctor G]
    (x : Product F G α) :
    (id <$> x) = x := by
  simp [Functor.map, id_map]

/-- **Composition law** for product functors:
    $\text{fmap}\;(f \circ g) = \text{fmap}\;f \circ \text{fmap}\;g$.

    Follows from the composition laws of $F$ and $G$ individually. -/
theorem map_comp [Functor F] [Functor G]
    [LawfulFunctor F] [LawfulFunctor G]
    (f : β → γ) (g : α → β) (x : Product F G α) :
    ((f ∘ g) <$> x) = (f <$> (g <$> x)) := by
  simp [Functor.map, comp_map]

end Product
end Data.Functor
