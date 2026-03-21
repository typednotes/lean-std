import Hale
import Tests.Harness

open Data Tests

/-
  Coverage:
  - Proofs: add_exact, sub_exact, scale_pos, neg_neg, fromInt_zero
  - Tested: Construction, arithmetic, toString, mul, toRatio
  - Not covered: None
-/

namespace TestFixed

def tests : List TestResult :=
  [ checkEq "Fixed fromInt" 300 (Fixed.fromInt (p := 2) 3).raw
  , checkEq "Fixed add raw" 457 ((Fixed.fromInt (p := 2) 3 + (⟨157⟩ : Fixed 2)).raw)
  , check "Fixed toString 3.00" (toString (Fixed.fromInt (p := 2) 3) == "3.00")
  , check "Fixed toString 1.57" (toString (⟨157⟩ : Fixed 2) == "1.57")
  , checkEq "Fixed sub" 143 ((Fixed.fromInt (p := 2) 3 - (⟨157⟩ : Fixed 2)).raw)
  , checkEq "Fixed neg" (-300) ((-(Fixed.fromInt (p := 2) 3)).raw)
  , checkEq "Fixed scale 2" 100 (Fixed.scale 2)
  , checkEq "Fixed scale 0" 1 (Fixed.scale 0)
  -- Mul
  , checkEq "Fixed mul raw" 471 ((Fixed.fromInt (p := 2) 3 * (⟨157⟩ : Fixed 2)).raw)
  -- toRatio
  , check "Fixed toRatio" (toString (Fixed.toRatio (⟨157⟩ : Fixed 2)) == "157/100")
  -- Double negation
  , checkEq "Fixed double neg" 300 ((-(-Fixed.fromInt (p := 2) 3)).raw)
  -- fromInt 0
  , checkEq "Fixed fromInt 0" 0 (Fixed.fromInt (p := 2) 0).raw
  -- Proof coverage
  , proofCovered "add_exact" "Hale.Base.Data.Fixed"
  , proofCovered "sub_exact" "Hale.Base.Data.Fixed"
  , proofCovered "scale_pos" "Hale.Base.Data.Fixed"
  , proofCovered "neg_neg" "Hale.Base.Data.Fixed"
  , proofCovered "fromInt_zero" "Hale.Base.Data.Fixed"
  ]
end TestFixed
