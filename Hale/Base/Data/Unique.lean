/-
  Hale.Base.Data.Unique — Unique identifiers

  Provides globally unique values via an IO-based counter,
  mirroring Haskell's `Data.Unique`.
-/

namespace Data

/-- A globally unique identifier, allocated via `newUnique`.

    $$\text{Unique} \cong \mathbb{N}$$

    Two `Unique` values are equal iff they were produced by the same
    call to `newUnique`. -/
structure Unique where
  /-- The underlying identifier. -/
  id : Nat
deriving BEq, Hashable, Repr

instance : Ord Unique where
  compare a b := compare a.id b.id

instance : ToString Unique where
  toString u := s!"Unique({u.id})"

/-- Global counter for unique ID allocation. -/
private initialize uniqueCounter : IO.Ref Nat ← IO.mkRef 0

/-- Allocate a fresh `Unique` value. Each call returns a distinct value.

    $$\text{newUnique} : \text{IO}(\text{Unique})$$ -/
def newUnique : IO Unique := do
  let n ← uniqueCounter.get
  uniqueCounter.set (n + 1)
  pure ⟨n⟩

/-- Extract the underlying `Nat` from a `Unique`. -/
@[inline] def Unique.hashUnique (u : Unique) : Nat := u.id

end Data
