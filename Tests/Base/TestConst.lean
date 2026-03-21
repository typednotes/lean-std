import Hale
import Tests.Harness

open Data.Functor Tests

/-
  Coverage:
  - Proofs: map_val, map_id, map_comp
  - Tested: Construction, functor map, BEq, Ord
  - Not covered: None
-/

namespace TestConst

def tests : List TestResult :=
  [ checkEq "Const getConst" 42 (Const.mk (β := String) 42).getConst
  , checkEq "Const functor map preserves value" 42
      ((Functor.map (· ++ "!") (Const.mk (β := String) 42) : Const Nat String).getConst)
  , check "Const BEq equal" (Const.mk (β := String) 42 == Const.mk (β := String) 42)
  , check "Const BEq not equal" !(Const.mk (β := String) 42 == Const.mk (β := String) 99)
  -- Ord
  , check "Const Ord lt" (compare (Const.mk (β := String) 1) (Const.mk (β := String) 2) == .lt)
  , check "Const Ord eq" (compare (Const.mk (β := String) 5) (Const.mk (β := String) 5) == .eq)
  -- Proof coverage
  , proofCovered "Const.map_val" "Hale.Base.Data.Functor.Const"
  , proofCovered "Const.map_id" "Hale.Base.Data.Functor.Const"
  , proofCovered "Const.map_comp" "Hale.Base.Data.Functor.Const"
  ]
end TestConst
