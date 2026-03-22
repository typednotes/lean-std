import Hale
import Tests.Harness

open Control Tests

/-
  Coverage:
  - Proofs: None (IO-based)
  - Tested: mkAutoUpdate basic usage, value updates over time
  - Not covered: None
-/

namespace TestAutoUpdate

def tests : IO (List TestResult) := do
  -- Create a counter that increments on each update
  let counter ← IO.mkRef (0 : Nat)
  let settings : UpdateSettings Nat :=
    { updateFreq := 50000  -- 50ms for testing
    , updateAction := do
        let n ← counter.get
        counter.set (n + 1)
        pure (n + 1)
    }
  let getter ← mkAutoUpdate settings
  -- Initial value should be 1 (eagerly computed)
  let v0 ← getter
  -- Wait for a few updates
  IO.sleep 200
  let v1 ← getter
  pure [
    checkEq "initial value" 1 v0
  , check "value updated" (v1 > v0)
  ]

end TestAutoUpdate
