import Hale.Base.Data.Ix
import Tests.Harness

open Data Tests

namespace TestIx

def tests : List TestResult :=
  [ -- Nat range
    checkEq "Ix Nat range [3,7]" [3, 4, 5, 6, 7] (Ix.range (3, 7))
  , checkEq "Ix Nat range [5,5]" [5] (Ix.range (5, 5))
  , checkEq "Ix Nat range [5,3] empty" ([] : List Nat) (Ix.range (5, 3))
  -- Nat index
  , checkEq "Ix Nat index (3,7) 5" (some 2) (Ix.index (3, 7) 5)
  , checkEq "Ix Nat index (3,7) 3" (some 0) (Ix.index (3, 7) 3)
  , checkEq "Ix Nat index (3,7) 7" (some 4) (Ix.index (3, 7) 7)
  , checkEq "Ix Nat index (3,7) 8 out" (none : Option Nat) (Ix.index (3, 7) 8)
  , checkEq "Ix Nat index (3,7) 2 out" (none : Option Nat) (Ix.index (3, 7) 2)
  -- Nat inRange
  , check "Ix Nat inRange (3,7) 5" (Ix.inRange (3, 7) 5)
  , check "Ix Nat inRange (3,7) 3" (Ix.inRange (3, 7) 3)
  , check "Ix Nat inRange (3,7) 7" (Ix.inRange (3, 7) 7)
  , check "Ix Nat not inRange (3,7) 8" (!Ix.inRange (3, 7) 8)
  -- Nat rangeSize
  , checkEq "Ix Nat rangeSize (3,7)" 5 (Ix.rangeSize (3, 7))
  , checkEq "Ix Nat rangeSize (5,3)" 0 (Ix.rangeSize (5, 3))
  -- Char range
  , checkEq "Ix Char range ('a','f')" ['a', 'b', 'c', 'd', 'e', 'f'] (Ix.range ('a', 'f'))
  , checkEq "Ix Char range ('a','a')" ['a'] (Ix.range ('a', 'a'))
  -- Char index
  , checkEq "Ix Char index ('a','f') 'c'" (some 2) (Ix.index ('a', 'f') 'c')
  , checkEq "Ix Char index ('a','f') 'z' out" (none : Option Nat) (Ix.index ('a', 'f') 'z')
  -- Char inRange
  , check "Ix Char inRange ('a','f') 'd'" (Ix.inRange ('a', 'f') 'd')
  , check "Ix Char not inRange ('a','f') 'z'" (!Ix.inRange ('a', 'f') 'z')
  -- Bool range
  , checkEq "Ix Bool range (false,true)" [false, true] (Ix.range (false, true))
  , checkEq "Ix Bool range (false,false)" [false] (Ix.range (false, false))
  , checkEq "Ix Bool range (true,true)" [true] (Ix.range (true, true))
  , checkEq "Ix Bool range (true,false) empty" ([] : List Bool) (Ix.range (true, false))
  -- Bool index
  , checkEq "Ix Bool index (false,true) true" (some 1) (Ix.index (false, true) true)
  , checkEq "Ix Bool index (false,true) false" (some 0) (Ix.index (false, true) false)
  -- Int range
  , checkEq "Ix Int range (-2,2)" ([-2, -1, 0, 1, 2] : List Int) (Ix.range ((-2 : Int), 2))
  , checkEq "Ix Int index (-2,2) 0" (some 2) (Ix.index ((-2 : Int), (2 : Int)) (0 : Int))
  -- Proof coverage
  , proofCovered "Ix.inRange_iff_index_isSome_nat" "Hale.Base.Data.Ix"
  ]

end TestIx
