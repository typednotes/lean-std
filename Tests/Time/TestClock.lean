import Hale.Time
import Tests.Harness

open Data.Time Tests

/-
  Coverage:
  - Proofs: fromSeconds_toSeconds, diffUTCTime_self (in source)
  - Tested: getCurrentTime, diffUTCTime, addUTCTime, NominalDiffTime arithmetic
  - Not covered: None
-/

namespace TestClock

def tests : IO (List TestResult) := do
  -- getCurrentTime returns a value
  let t1 ← getCurrentTime
  IO.sleep 10
  let t2 ← getCurrentTime

  -- Time progresses
  let diff := diffUTCTime t2 t1

  -- NominalDiffTime arithmetic
  let d1 := NominalDiffTime.fromSeconds 5
  let d2 := NominalDiffTime.fromSeconds 3
  let sum := d1 + d2
  let sub := d1 - d2

  -- addUTCTime
  let t3 := addUTCTime (NominalDiffTime.fromSeconds 10) t1

  pure [
    check "getCurrentTime returns value" (t1.nanosSinceEpoch > 0)
  , check "time progresses" (diff.nanoseconds > 0)
  , checkEq "NominalDiffTime add" 8 sum.toSeconds
  , checkEq "NominalDiffTime sub" 2 sub.toSeconds
  , checkEq "fromSeconds 5 toSeconds" 5 d1.toSeconds
  , check "addUTCTime increases" (t3.nanosSinceEpoch > t1.nanosSinceEpoch)
  , checkEq "NominalDiffTime.zero" 0 NominalDiffTime.zero.nanoseconds
  -- Proof coverage
  , proofCovered "fromSeconds_toSeconds" "Data.Time.NominalDiffTime.fromSeconds_toSeconds"
  , proofCovered "diffUTCTime_self" "Data.Time.diffUTCTime_self"
  ]

end TestClock
