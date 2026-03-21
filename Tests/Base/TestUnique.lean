import Hale
import Tests.Harness

open Data Tests

/-
  Coverage:
  - Proofs: None (IO-based)
  - Tested: newUnique uniqueness, BEq, Ord, ToString
  - Not covered: None
-/

namespace TestUnique

def tests : IO (List TestResult) := do
  let u1 ← newUnique
  let u2 ← newUnique
  let u3 ← newUnique
  pure [
    -- Uniqueness
    check "Unique u1 != u2" (!(u1 == u2))
  , check "Unique u2 != u3" (!(u2 == u3))
  , check "Unique u1 != u3" (!(u1 == u3))
  -- Ordering
  , check "Unique ordered" (compare u1 u2 == .lt)
  -- ToString
  , check "Unique toString" (toString u1 != "")
  ]
end TestUnique
