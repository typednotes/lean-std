import Hale.Base.System.Environment
import Tests.Harness

open System.Environment Tests

namespace TestEnvironment

def tests : IO (List TestResult) := do
  let mut results : List TestResult := []

  -- lookupEnv for PATH (should be some on any system)
  results := results ++ [← checkIO "lookupEnv PATH is some" do
    let v ← lookupEnv "PATH"
    pure v.isSome]

  -- lookupEnv for nonexistent variable
  results := results ++ [← checkIO "lookupEnv nonexistent is none" do
    let v ← lookupEnv "HALE_NONEXISTENT_VAR_12345"
    pure v.isNone]

  -- getEnv for PATH returns non-empty string
  results := results ++ [← checkIO "getEnv PATH non-empty" do
    let v ← getEnv "PATH"
    pure (!v.isEmpty)]

  -- getEnv for nonexistent returns empty string
  results := results ++ [← checkEqIO "getEnv nonexistent returns empty" "" do
    getEnv "HALE_NONEXISTENT_VAR_12345"]

  -- getHome returns something on macOS/Linux
  results := results ++ [← checkIO "getHome is some" do
    let v ← getHome
    pure v.isSome]

  -- getPath returns something
  results := results ++ [← checkIO "getPath is some" do
    let v ← getPath
    pure v.isSome]

  pure results

end TestEnvironment
