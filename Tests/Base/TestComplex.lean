import Hale
import Tests.Harness

open Data Tests

/-
  Coverage:
  - Proofs: conjugate_conjugate, add_comm'
  - Tested: Construction, add, conjugate, magnitudeSquared, neg, toString, ofReal, i
  - Not covered: None
-/

namespace TestComplex

def tests : List TestResult :=
  [ checkEq "Complex re" 3 (Complex.mk 3 4 : Complex Int).re
  , checkEq "Complex im" 4 (Complex.mk 3 4 : Complex Int).im
  , checkEq "Complex add re" 4 ((Complex.mk 3 4 + Complex.mk 1 (-2) : Complex Int).re)
  , checkEq "Complex add im" 2 ((Complex.mk 3 4 + Complex.mk 1 (-2) : Complex Int).im)
  , checkEq "Complex conjugate" (-4) (Complex.conjugate (Complex.mk 3 4 : Complex Int)).im
  , checkEq "Complex magnitudeSquared" 25 (Complex.magnitudeSquared (Complex.mk 3 4 : Complex Int))
  , checkEq "Complex neg" (-3) ((-(Complex.mk 3 4 : Complex Int)).re)
  , check "Complex toString" (toString (Complex.mk 3 4 : Complex Int) != "")
  -- ofReal
  , checkEq "Complex ofReal re" 5 (Complex.ofReal (α := Int) 5).re
  , checkEq "Complex ofReal im" 0 (Complex.ofReal (α := Int) 5).im
  -- i unit
  , checkEq "Complex i re" 0 (Complex.i (α := Int)).re
  , checkEq "Complex i im" 1 (Complex.i (α := Int)).im
  -- Proof coverage
  , proofCovered "Complex.conjugate_conjugate" "Hale.Base.Data.Complex"
  , proofCovered "Complex.add_comm'" "Hale.Base.Data.Complex"
  ]
end TestComplex
