import Hale
import Tests.Harness

open Data Tests

/-
  Coverage:
  - Proofs: ci_eq_iff (in source)
  - Tested: mk', BEq, Hashable, Ord, ToString, map, FoldCase instances
  - Not covered: None
-/

namespace TestCaseInsensitive

def tests : List TestResult :=
  let hello := CI.mk' (α := String) "Hello"
  let hELLO := CI.mk' (α := String) "HELLO"
  let world := CI.mk' (α := String) "World"
  [ -- Equality (case-insensitive)
    check "CI eq case-insensitive" (hello == hELLO)
  , check "CI neq different" (!(hello == world))
  -- Original preserved
  , checkEq "CI original preserved" "Hello" hello.original
  , checkEq "CI foldedCase" "hello" hello.foldedCase
  -- ToString uses original
  , checkEq "CI toString" "Hello" (toString hello)
  -- Ordering
  , check "CI ord hello < world" (compare hello world == .lt)
  , check "CI ord HELLO < world" (compare hELLO world == .lt)
  -- Map
  , let mapped := CI.map (α := String) (· ++ "!") hello
    checkEq "CI map original" "Hello!" mapped.original
  -- FoldCase String
  , checkEq "foldCase String" "hello world" (FoldCase.foldCase "Hello World")
  -- Proof coverage
  , proofCovered "ci_eq_iff" "Data.CI.ci_eq_iff"
  ]

end TestCaseInsensitive
