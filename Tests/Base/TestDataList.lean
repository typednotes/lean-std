import Hale.Base.Data.List
import Tests.Harness

open Data Tests

namespace TestDataList

def tests : List TestResult :=
  [ -- nub
    checkEq "List' nub" [1, 2, 3] (List'.nub [1, 2, 3, 2, 1])
  , checkEq "List' nub empty" ([] : List Nat) (List'.nub [])
  , checkEq "List' nub singleton" [1] (List'.nub [1])
  -- group
  , checkEq "List' group" [[1, 1], [2, 2, 2], [3]] (List'.group [1, 1, 2, 2, 2, 3])
  , checkEq "List' group empty" ([] : List (List Nat)) (List'.group [])
  , checkEq "List' group no dups" [[1], [2], [3]] (List'.group [1, 2, 3])
  -- transpose
  , checkEq "List' transpose" [[1, 4], [2, 5], [3, 6]] (List'.transpose [[1, 2, 3], [4, 5, 6]])
  , checkEq "List' transpose empty" ([] : List (List Nat)) (List'.transpose [])
  -- tails
  , checkEq "List' tails" [[1, 2, 3], [2, 3], [3], []] (List'.tails [1, 2, 3])
  , checkEq "List' tails empty" ([[]] : List (List Nat)) (List'.tails [])
  -- inits
  , checkEq "List' inits" [[], [1], [1, 2], [1, 2, 3]] (List'.inits [1, 2, 3])
  , checkEq "List' inits empty" ([[]] : List (List Nat)) (List'.inits [])
  -- subsequences
  , checkEq "List' subsequences [1,2]" [[], [2], [1], [1, 2]] (List'.subsequences [1, 2])
  , checkEq "List' subsequences empty" ([[]] : List (List Nat)) (List'.subsequences [])
  -- unfoldr
  , checkEq "List' unfoldr countdown"
      [5, 4, 3, 2, 1]
      (List'.unfoldr (fun n => if n == 0 then none else some (n, n - 1)) 5)
  -- scanr
  , checkEq "List' scanr (+) 0" [6, 5, 3, 0] (List'.scanr (· + ·) 0 [1, 2, 3])
  , checkEq "List' scanr empty" [0] (List'.scanr (· + ·) 0 ([] : List Nat))
  -- mapAccumL
  , check "List' mapAccumL running sum"
      (let (s, ys) := List'.mapAccumL (fun acc x => (acc + x, acc + x)) 0 [1, 2, 3]
       s == 6 && ys == [1, 3, 6])
  -- mapAccumR
  , check "List' mapAccumR running sum right"
      (let (s, ys) := List'.mapAccumR (fun acc x => (acc + x, acc + x)) 0 [1, 2, 3]
       s == 6 && ys == [6, 5, 3])
  -- intercalate
  , checkEq "List' intercalate" [1, 0, 2, 0, 3] (List'.intercalate [0] [[1], [2], [3]])
  , checkEq "List' intercalate empty" ([] : List Nat) (List'.intercalate [0] [])
  -- sortOn
  , checkEq "List' sortOn negate" [3, 2, 1] (List'.sortOn (fun (n : Nat) => Int.negSucc n) [1, 2, 3])
  -- maximumBy / minimumBy
  , checkEq "List' maximumBy" (some 3) (List'.maximumBy compare [1, 3, 2])
  , checkEq "List' minimumBy" (some 1) (List'.minimumBy compare [3, 1, 2])
  , checkEq "List' maximumBy empty" (none : Option Nat) (List'.maximumBy compare [])
  , checkEq "List' minimumBy empty" (none : Option Nat) (List'.minimumBy compare [])
  -- deleteBy
  , checkEq "List' deleteBy" [1, 3, 2] (List'.deleteBy (· == ·) 2 [1, 2, 3, 2])
  , checkEq "List' deleteBy not found" [1, 2, 3] (List'.deleteBy (· == ·) 5 [1, 2, 3])
  -- unionBy
  , checkEq "List' unionBy" [1, 2, 3, 4] (List'.unionBy (· == ·) [1, 2, 3] [2, 3, 4])
  -- intersectBy
  , checkEq "List' intersectBy" [2, 3] (List'.intersectBy (· == ·) [1, 2, 3] [2, 3, 4])
  -- insertBy
  , checkEq "List' insertBy" [1, 2, 3, 4] (List'.insertBy compare 3 [1, 2, 4])
  , checkEq "List' insertBy front" [0, 1, 2] (List'.insertBy compare 0 [1, 2])
  , checkEq "List' insertBy end" [1, 2, 3] (List'.insertBy compare 3 [1, 2])
  -- genericLength
  , checkEq "List' genericLength" 3 (List'.genericLength [1, 2, 3])
  -- Proof coverage
  , proofCovered "List'.tails_length" "Hale.Base.Data.List"
  , proofCovered "List'.inits_length" "Hale.Base.Data.List"
  , proofCovered "List'.tails_nil" "Hale.Base.Data.List"
  , proofCovered "List'.inits_nil" "Hale.Base.Data.List"
  ]

end TestDataList
