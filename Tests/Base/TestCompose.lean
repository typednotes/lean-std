import Hale
import Tests.Harness

open Data.Functor Tests

/-
  Coverage:
  - Proofs: Compose.map_id, Compose.map_comp
  - Tested: Construction, map
  - Not covered: None
-/

namespace TestCompose

def tests : List TestResult :=
  [ checkEq "Compose getCompose" [some 1, none, some 3]
      (Compose.mk [some 1, none, some 3] : Compose List Option Nat).getCompose
  , checkEq "Compose map" [some 2, none, some 6]
      ((Functor.map (· * 2) (Compose.mk [some 1, none, some 3]) : Compose List Option Nat).getCompose)
  , checkEq "Compose map on Identity Option" (Identity.mk (some 10))
      ((Functor.map (· * 2) (Compose.mk (Identity.mk (some 5))) : Compose Identity Option Nat).getCompose)
  -- Proof coverage
  , proofCovered "Compose.map_id" "Hale.Base.Data.Functor.Compose"
  , proofCovered "Compose.map_comp" "Hale.Base.Data.Functor.Compose"
  ]
end TestCompose
