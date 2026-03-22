import Hale.UnliftIO
import Tests.Harness

open Control.Monad.IO Tests

/-
  Coverage:
  - Proofs: None (IO-based)
  - Tested: IO instance, ReaderT instance, withRunInIO, toIO
  - Not covered: None
-/

namespace TestUnliftIO

def tests : IO (List TestResult) := do
  -- Test IO instance: withRunInIO should run the action directly
  let v1 ← MonadUnliftIO.withRunInIO (m := IO) fun run => run Nat (pure 42)
  -- Test ReaderT instance: withRunInIO captures environment
  let v2 ← (do
    MonadUnliftIO.withRunInIO (m := ReaderT Nat IO) fun run => do
      run Nat (do let env ← read; pure env)
      : ReaderT Nat IO Nat).run 99
  -- Test withRunInIO in ReaderT preserves environment
  let v3 ← (do
    MonadUnliftIO.withRunInIO (m := ReaderT String IO) fun run => do
      let s ← run String read
      pure s : ReaderT String IO String).run "hello"
  -- Test toIO
  let v4IO ← (do
    MonadUnliftIO.toIO (m := ReaderT Nat IO) (do
      let n ← read
      pure (n + 1)) : ReaderT Nat IO (IO Nat)).run 10
  let v4 ← v4IO
  pure [
    checkEq "IO withRunInIO" 42 v1
  , checkEq "ReaderT withRunInIO" 99 v2
  , checkEq "ReaderT preserves env" "hello" v3
  , checkEq "ReaderT toIO" 11 v4
  ]

end TestUnliftIO
