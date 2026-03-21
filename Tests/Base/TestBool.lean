import Hale
import Tests.Harness

open Tests

/-
  Coverage:
  - Proofs: bool_false, bool_true, guard'_true, guard'_false, bool_ite
  - Tested: bool, guard'
  - Not covered: None
-/

namespace TestBool

def tests : List TestResult :=
  [ checkEq "bool false" "no" (Data.DataBool.bool "no" "yes" false)
  , checkEq "bool true" "yes" (Data.DataBool.bool "no" "yes" true)
  , checkEq "guard' true" [42] (Data.DataBool.guard' true 42)
  , checkEq "guard' false" ([] : List Nat) (Data.DataBool.guard' false 42)
  -- Proof coverage
  , proofCovered "bool_false" "Hale.Base.Data.Bool"
  , proofCovered "bool_true" "Hale.Base.Data.Bool"
  , proofCovered "guard'_true" "Hale.Base.Data.Bool"
  , proofCovered "guard'_false" "Hale.Base.Data.Bool"
  , proofCovered "bool_ite" "Hale.Base.Data.Bool"
  ]
end TestBool
