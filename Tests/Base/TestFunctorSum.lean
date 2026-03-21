import Hale
import Hale.Base.Data.Functor.Sum
import Tests.Harness

open Data.Functor Tests

/-
  Coverage:
  - Proofs: FunctorSum.map_id, FunctorSum.map_comp
  - Tested: Construction (inl, inr), functor map on both branches
  - Not covered: None
-/

namespace TestFunctorSum

/-- Helper: extract the List from an inl branch, or [] if inr. -/
private def getInl (x : FunctorSum List Option Nat) : List Nat :=
  match x with
  | .inl l => l
  | .inr _ => []

/-- Helper: extract the Option from an inr branch, or none if inl. -/
private def getInr (x : FunctorSum List Option Nat) : Option Nat :=
  match x with
  | .inl _ => none
  | .inr o => o

def tests : List TestResult :=
  [ -- Construction
    checkEq "FunctorSum inl" [1, 2, 3]
      (getInl (FunctorSum.inl [1, 2, 3]))
  , checkEq "FunctorSum inr" (some 42)
      (getInr (FunctorSum.inr (some 42)))
  -- Functor map on inl
  , checkEq "FunctorSum map inl" [2, 4, 6]
      (getInl (Functor.map (· * 2) (FunctorSum.inl [1, 2, 3] : FunctorSum List Option Nat)))
  -- Functor map on inr
  , checkEq "FunctorSum map inr" (some 10)
      (getInr (Functor.map (· * 2) (FunctorSum.inr (some 5) : FunctorSum List Option Nat)))
  -- Functor map on inr none
  , checkEq "FunctorSum map inr none" (none : Option Nat)
      (getInr (Functor.map (· * 2) (FunctorSum.inr none : FunctorSum List Option Nat)))
  -- Proof coverage
  , proofCovered "FunctorSum.map_id" "Hale.Base.Data.Functor.Sum"
  , proofCovered "FunctorSum.map_comp" "Hale.Base.Data.Functor.Sum"
  ]
end TestFunctorSum
