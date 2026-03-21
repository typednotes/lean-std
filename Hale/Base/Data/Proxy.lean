/-
  Hale.Base.Data.Proxy — Phantom type proxy

  Provides `Proxy`, a type that carries a phantom type parameter but no data.
  Useful for passing type-level information without runtime cost.
-/

namespace Data

/-- The proxy type $\text{Proxy}(\alpha)$ carries a phantom type parameter $\alpha$
but contains no data. It is the terminal object in the category of types:
every type has exactly one function into $\text{Proxy}(\alpha)$.

$$\text{Proxy} : \text{Type}\; u \to \text{Type}$$ -/
structure Proxy (α : Type u) : Type where
  mk ::
deriving Inhabited

namespace Proxy

/-- `BEq` instance for $\text{Proxy}(\alpha)$ — always true since there is only one value. -/
instance : BEq (Proxy α) where
  beq _ _ := true

/-- `Ord` instance for $\text{Proxy}(\alpha)$ — always `Ordering.eq` since all values are equal. -/
instance : Ord (Proxy α) where
  compare _ _ := .eq

/-- `Repr` instance for $\text{Proxy}(\alpha)$, displaying as `Proxy.mk`. -/
instance : Repr (Proxy α) where
  reprPrec _ _ := "Proxy.mk"

/-- `Hashable` instance for $\text{Proxy}(\alpha)$ — always hashes to $0$. -/
instance : Hashable (Proxy α) where
  hash _ := 0

/-- `ToString` instance for $\text{Proxy}(\alpha)$. -/
instance : ToString (Proxy α) where
  toString _ := "Proxy"

/-- `Functor` instance for $\text{Proxy}$. Since $\text{Proxy}$ carries no data,
`map` is the identity on the structure:

$$\text{map}\; f\; \text{Proxy.mk} = \text{Proxy.mk}$$ -/
instance : Functor Proxy where
  map _ _ := Proxy.mk

/-- `Pure` instance for $\text{Proxy}$:

$$\text{pure}\; a = \text{Proxy.mk}$$ -/
instance : Pure Proxy where
  pure _ := Proxy.mk

/-- `Bind` instance for $\text{Proxy}$:

$$\text{bind}\; \text{Proxy.mk}\; f = \text{Proxy.mk}$$ -/
instance : Bind Proxy where
  bind _ _ := Proxy.mk

/-- `Seq` instance for $\text{Proxy}$:

$$\text{seq}\; \text{Proxy.mk}\; f = \text{Proxy.mk}$$ -/
instance : Seq Proxy where
  seq _ _ := Proxy.mk

/-- `SeqLeft` instance for $\text{Proxy}$. -/
instance : SeqLeft Proxy where
  seqLeft _ _ := Proxy.mk

/-- `SeqRight` instance for $\text{Proxy}$. -/
instance : SeqRight Proxy where
  seqRight _ _ := Proxy.mk

/-- `Applicative` instance for $\text{Proxy}$. -/
instance : Applicative Proxy where

/-- `Monad` instance for $\text{Proxy}$. -/
instance : Monad Proxy where

-- ══════════════════════════════════════════════
-- Functor laws
-- ══════════════════════════════════════════════

/-- Functor identity law: $\text{map}\;\text{id} = \text{id}$.

$$\text{map}\;\text{id}\;(\text{Proxy.mk}) = \text{Proxy.mk}$$ -/
theorem map_id (p : Proxy α) : Functor.map id p = p := by
  cases p; rfl

/-- Functor composition law: $\text{map}\;(f \circ g) = \text{map}\;f \circ \text{map}\;g$.

$$\text{map}\;(f \circ g)\;\text{Proxy.mk} = \text{map}\;f\;(\text{map}\;g\;\text{Proxy.mk})$$ -/
theorem map_comp (f : β → γ) (g : α → β) (p : Proxy α) :
    Functor.map (f ∘ g) p = Functor.map f (Functor.map g p) := by
  cases p; rfl

-- ══════════════════════════════════════════════
-- Monad laws
-- ══════════════════════════════════════════════

/-- Left identity (pure/bind): $\text{bind}\;(\text{pure}\;a)\;f = f\;a$.

For $\text{Proxy}$, both sides reduce to $\text{Proxy.mk}$. -/
theorem pure_bind (a : α) (f : α → Proxy β) : bind (pure a) f = f a := by
  cases (f a); rfl

/-- Right identity (bind/pure): $\text{bind}\;m\;\text{pure} = m$.

$$\text{bind}\;\text{Proxy.mk}\;\text{pure} = \text{Proxy.mk}$$ -/
theorem bind_pure (p : Proxy α) : bind p pure = p := by
  cases p; rfl

/-- Associativity: $\text{bind}\;(\text{bind}\;m\;f)\;g = \text{bind}\;m\;(\lambda x.\;\text{bind}\;(f\;x)\;g)$.

For $\text{Proxy}$, all sides reduce to $\text{Proxy.mk}$. -/
theorem bind_assoc (p : Proxy α) (f : α → Proxy β) (g : β → Proxy γ) :
    bind (bind p f) g = bind p (fun x => bind (f x) g) := by
  cases p; rfl

end Proxy
end Data
