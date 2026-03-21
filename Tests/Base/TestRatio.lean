import Hale
import Tests.Harness

open Data Tests

/-
  Coverage:
  - Proofs: None yet (Ratio operations go through mk' normalization)
  - Tested: Arithmetic, comparison, ordering, floor/ceil/round, abs, inv, div, identity, commutativity
  - Not covered: Formal proofs of add_comm, mul_comm (require gcd commutativity proofs)
-/

namespace TestRatio

def tests : List TestResult :=
  [ check "Ratio 1/2 + 1/3 = 5/6" (toString (Ratio.mk' 1 2 (by omega) + Ratio.mk' 1 3 (by omega)) == "5/6")
  , check "Ratio 1/2 * 1/3 = 1/6" (toString (Ratio.mk' 1 2 (by omega) * Ratio.mk' 1 3 (by omega)) == "1/6")
  , checkEq "Ratio floor 5/3" 1 (Ratio.mk' 5 3 (by omega)).floor
  , checkEq "Ratio ceil 5/3" 2 (Ratio.mk' 5 3 (by omega)).ceil
  , check "Ratio fromInt 3" (toString (Ratio.fromInt 3) == "3")
  , check "Ratio ordering 1/3 < 1/2" (compare (Ratio.mk' 1 3 (by omega)) (Ratio.mk' 1 2 (by omega)) == .lt)
  , check "Ratio equality 2/4 == 1/2" (Ratio.mk' 2 4 (by omega) == Ratio.mk' 1 2 (by omega))
  , check "Ratio sub 1/2 - 1/3 = 1/6" (toString (Ratio.mk' 1 2 (by omega) - Ratio.mk' 1 3 (by omega)) == "1/6")
  , check "Ratio neg" (toString (-(Ratio.mk' 1 2 (by omega))) == "-1/2")
  -- abs
  , check "Ratio abs positive" (toString (Ratio.abs (Ratio.mk' 3 4 (by omega))) == "3/4")
  , check "Ratio abs negative" (toString (Ratio.abs (Ratio.mk' (-3) 4 (by omega))) == "3/4")
  -- inv and div
  , check "Ratio inv 2/3" (toString (Ratio.inv (Ratio.mk' 2 3 (by omega)) (by native_decide)) == "3/2")
  , check "Ratio div 1/2 ÷ 1/3 = 3/2" (toString (Ratio.div (Ratio.mk' 1 2 (by omega)) (Ratio.mk' 1 3 (by omega)) (by native_decide)) == "3/2")
  -- round
  , checkEq "Ratio round 5/3" 2 (Ratio.mk' 5 3 (by omega)).round
  , checkEq "Ratio round -5/3" (-3) (Ratio.mk' (-5) 3 (by omega)).round
  , checkEq "Ratio round 1/2" 1 (Ratio.mk' 1 2 (by omega)).round
  -- commutativity (concrete)
  , check "Ratio add commutative" (
      Ratio.mk' 1 3 (by omega) + Ratio.mk' 1 4 (by omega) ==
      Ratio.mk' 1 4 (by omega) + Ratio.mk' 1 3 (by omega))
  , check "Ratio mul commutative" (
      Ratio.mk' 2 3 (by omega) * Ratio.mk' 3 5 (by omega) ==
      Ratio.mk' 3 5 (by omega) * Ratio.mk' 2 3 (by omega))
  -- identity
  , check "Ratio add identity" (
      Ratio.mk' 3 7 (by omega) + Ratio.zero == Ratio.mk' 3 7 (by omega))
  , check "Ratio mul identity" (
      Ratio.mk' 3 7 (by omega) * Ratio.one == Ratio.mk' 3 7 (by omega))
  -- edge cases
  , check "Ratio fromInt 0 == zero" (Ratio.fromInt 0 == Ratio.zero)
  , check "Ratio normalization 6/4 == 3/2" (Ratio.mk' 6 4 (by omega) == Ratio.mk' 3 2 (by omega))
  ]
end TestRatio
