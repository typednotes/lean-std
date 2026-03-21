import Hale.Base.Control.Exception
import Hale.Base.Data.Either
import Tests.Harness

open Control.Exception Data Tests

namespace TestException

def tests : IO (List TestResult) := do
  let mut results : List TestResult := []

  -- try' catches errors
  results := results ++ [← checkIO "try' catches thrown error" do
    let r ← try' (throw (.userError "boom") : IO Nat)
    pure (r.isLeft)]

  -- try' passes through success
  results := results ++ [← checkIO "try' passes success" do
    let r ← try' (pure 42 : IO Nat)
    pure (r.isRight)]

  -- try' returns correct value on success
  results := results ++ [← checkEqIO "try' returns Right value" (Either.right 42) do
    try' (pure 42 : IO Nat)]

  -- catch' recovers from error
  results := results ++ [← checkEqIO "catch' recovers from error" "recovered" do
    catch' (throw (.userError "fail") : IO String) fun _ => pure "recovered"]

  -- catch' passes through success
  results := results ++ [← checkEqIO "catch' passes success" "ok" do
    catch' (pure "ok") fun _ => pure "recovered"]

  -- finally': cleanup runs on success
  results := results ++ [← checkIO "finally': cleanup runs on success" do
    let ref ← IO.mkRef false
    let _ ← finally' (pure 42 : IO Nat) (ref.set true)
    ref.get]

  -- finally': cleanup runs on failure
  results := results ++ [← checkIO "finally': cleanup runs on failure" do
    let ref ← IO.mkRef false
    let r ← (finally' (throw (.userError "boom") : IO Nat) (ref.set true)).toBaseIO
    let cleaned ← ref.get
    pure (cleaned && !r.isOk)]

  -- bracket: release runs on success
  results := results ++ [← checkIO "bracket: release runs on success" do
    let ref ← IO.mkRef false
    let _ ← bracket (pure 1) (fun _ => ref.set true) (fun n => pure (n + 1))
    ref.get]

  -- bracket: release runs on failure
  results := results ++ [← checkIO "bracket: release runs on failure" do
    let ref ← IO.mkRef false
    let act : IO Nat := bracket (pure 1) (fun _ => ref.set true) (fun _ => throw (IO.userError "boom"))
    let r ← act.toBaseIO
    let released ← ref.get
    pure (released && !r.isOk)]

  -- onException: cleanup runs on failure
  results := results ++ [← checkIO "onException: cleanup runs on failure" do
    let ref ← IO.mkRef false
    let r ← (onException (throw (.userError "boom") : IO Nat) (ref.set true)).toBaseIO
    let cleaned ← ref.get
    pure (cleaned && !r.isOk)]

  -- onException: cleanup does NOT run on success
  results := results ++ [← checkIO "onException: cleanup skipped on success" do
    let ref ← IO.mkRef false
    let _ ← onException (pure 42 : IO Nat) (ref.set true)
    let cleaned ← ref.get
    pure (!cleaned)]

  -- evaluate returns the value
  results := results ++ [← checkEqIO "evaluate returns value" 99 do
    evaluate 99]

  pure results

end TestException
