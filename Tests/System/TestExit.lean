import Hale.Base.System.Exit
import Tests.Harness

open System Tests

namespace TestExit

def tests : List TestResult :=
  [ -- ExitCode construction
    check "ExitCode.success exists" true
  , check "ExitCode.failure 1" (ExitCode.failure 1 == ExitCode.failure 1)
  , check "ExitCode.failure different codes" (ExitCode.failure 1 != ExitCode.failure 2)
  , check "ExitCode.success != failure" (ExitCode.success != ExitCode.failure 0)

  -- ToString
  , checkEq "toString success" "ExitSuccess" (toString ExitCode.success)
  , checkEq "toString failure 1" "ExitFailure(1)" (toString (ExitCode.failure 1))
  , checkEq "toString failure 42" "ExitFailure(42)" (toString (ExitCode.failure 42))

  -- toUInt32
  , checkEq "success toUInt32" (0 : UInt32) ExitCode.success.toUInt32
  , checkEq "failure 1 toUInt32" (1 : UInt32) (ExitCode.failure 1).toUInt32
  , checkEq "failure 255 toUInt32" (255 : UInt32) (ExitCode.failure 255).toUInt32

  -- isSuccess
  , check "success isSuccess" ExitCode.success.isSuccess
  , check "failure not isSuccess" (!(ExitCode.failure 1).isSuccess)

  -- Proof coverage
  , proofCovered "success_toUInt32" "ExitCode.success_toUInt32"
  , proofCovered "isSuccess_iff" "ExitCode.isSuccess_iff"
  ]

end TestExit
