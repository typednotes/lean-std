import Tests.Harness
import Tests.Base.TestVoid
import Tests.Base.TestFunction
import Tests.Base.TestNewtype
import Tests.Base.TestBifunctor
import Tests.Base.TestContravariant
import Tests.Base.TestConst
import Tests.Base.TestIdentity
import Tests.Base.TestCompose
import Tests.Base.TestCategory
import Tests.Base.TestNonEmpty
import Tests.Base.TestEither
import Tests.Base.TestOrd
import Tests.Base.TestTuple
import Tests.Base.TestFoldable
import Tests.Base.TestTraversable
import Tests.Base.TestRatio
import Tests.Base.TestComplex
import Tests.Base.TestFixed
import Tests.Base.TestArrow
-- New base modules
import Tests.Base.TestProxy
import Tests.Base.TestBool
import Tests.Base.TestMaybe
import Tests.Base.TestDataString
import Tests.Base.TestProduct
import Tests.Base.TestFunctorSum
import Tests.Base.TestApplicative
import Tests.Base.TestMonad
import Tests.Base.TestDataChar
import Tests.Base.TestBits
import Tests.Base.TestIx
import Tests.Base.TestDataList
import Tests.Base.TestUnique
import Tests.Base.TestIORef
-- Control
import Tests.Control.TestMVar
import Tests.Control.TestChan
import Tests.Control.TestQSem
import Tests.Control.TestQSemN
import Tests.Control.TestConcurrent
import Tests.Control.TestException
-- System
import Tests.System.TestSysIO
import Tests.System.TestExit
import Tests.System.TestEnvironment
-- ByteString
import Tests.ByteString.TestByteString
import Tests.ByteString.TestShort
import Tests.ByteString.TestLazy
import Tests.ByteString.TestBuilder
import Tests.ByteString.TestChar8
import Tests.ByteString.TestLazyChar8

open Tests

def main : IO UInt32 := do
  let mut totalFailures : Nat := 0

  let suites : List (String × List TestResult) :=
    [ ("Void",          TestVoid.tests)
    , ("Function",      TestFunction.tests)
    , ("Newtype",       TestNewtype.tests)
    , ("Bifunctor",     TestBifunctor.tests)
    , ("Contravariant", TestContravariant.tests)
    , ("Const",         TestConst.tests)
    , ("Identity",      TestIdentity.tests)
    , ("Compose",       TestCompose.tests)
    , ("Category",      TestCategory.tests)
    , ("NonEmpty",      TestNonEmpty.tests)
    , ("Either",        TestEither.tests)
    , ("Ord",           TestOrd.tests)
    , ("Tuple",         TestTuple.tests)
    , ("Foldable",      TestFoldable.tests)
    , ("Traversable",   TestTraversable.tests)
    , ("Ratio",         TestRatio.tests)
    , ("Complex",       TestComplex.tests)
    , ("Fixed",         TestFixed.tests)
    , ("Arrow",         TestArrow.tests)
    -- New base modules
    , ("Proxy",         TestProxy.tests)
    , ("Bool",          TestBool.tests)
    , ("Maybe",         TestMaybe.tests)
    , ("DataString",    TestDataString.tests)
    , ("Product",       TestProduct.tests)
    , ("FunctorSum",    TestFunctorSum.tests)
    , ("Applicative",   TestApplicative.tests)
    , ("Monad",         TestMonad.tests)
    , ("DataChar",      TestDataChar.tests)
    , ("Bits",          TestBits.tests)
    , ("Ix",            TestIx.tests)
    , ("DataList",      TestDataList.tests)
    , ("Exit",          TestExit.tests)
    -- ByteString
    , ("ByteString",    TestByteString.tests)
    , ("ShortByteString", TestShort.tests)
    , ("LazyByteString", TestLazy.tests)
    , ("Builder",       TestBuilder.tests)
    , ("Char8",         TestChar8.tests)
    , ("LazyChar8",    TestLazyChar8.tests)
    ]

  for (name, tests) in suites do
    let failures ← runTests name tests
    totalFailures := totalFailures + failures

  -- IO test suites
  let ioSuites : List (String × IO (List TestResult)) :=
    [ ("MVar",       TestMVar.tests)
    , ("Chan",       TestChan.tests)
    , ("QSem",       TestQSem.tests)
    , ("QSemN",      TestQSemN.tests)
    , ("Concurrent", TestConcurrent.tests)
    , ("Unique",     TestUnique.tests)
    , ("IORef",      TestIORef.tests)
    , ("Exception",  TestException.tests)
    , ("SysIO",      TestSysIO.tests)
    , ("Environment", TestEnvironment.tests)
    ]

  for (name, testsIO) in ioSuites do
    let failures ← runIOTests name testsIO
    totalFailures := totalFailures + failures

  IO.println ""
  if totalFailures == 0 then
    IO.println s!"All tests passed!"
    return 0
  else
    IO.println s!"{totalFailures} test(s) failed."
    return 1
