import Hale
import Hale.Base.Control.Applicative
import Tests.Harness

open Tests

/-
  Coverage:
  - Tested: optional (some/none), asum, guard (true/false)
  - Not covered: None
-/

namespace TestApplicative

-- Use fully qualified names to avoid ambiguity with Lean builtins

def tests : List TestResult :=
  [ -- optional with Option
    checkEq "optional some" (some (some 5))
      (Control.Applicative.optional (some 5))
  , checkEq "optional none" (some none)
      (Control.Applicative.optional (none : Option Nat))
  -- asum with Option
  , checkEq "asum Option all none" (none : Option Nat)
      (Control.Applicative.asum [none, none, none])
  , checkEq "asum Option first some" (some 1)
      (Control.Applicative.asum [none, some 1, some 2])
  , checkEq "asum Option empty" (none : Option Nat)
      (Control.Applicative.asum [])
  -- guard with Option
  , checkEq "guard true Option" (some ())
      (Control.Applicative.guard (f := Option) true)
  , checkEq "guard false Option" (none : Option Unit)
      (Control.Applicative.guard (f := Option) false)
  ]
end TestApplicative
