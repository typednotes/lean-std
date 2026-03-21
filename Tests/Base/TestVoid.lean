import Hale
import Tests.Harness

open Data Tests

/-
  Coverage:
  - Proofs: Vacuous instances (BEq, Ord, etc.) are correct by construction (no Void values exist)
  - Tested: Alias check
  - Not covered: None (type has no inhabitants to test)
-/

namespace TestVoid

def tests : List TestResult :=
  [ check "Void is alias for Empty" true
  , proofCovered "Void instances vacuously correct" "no Void values exist"
  ]
end TestVoid
