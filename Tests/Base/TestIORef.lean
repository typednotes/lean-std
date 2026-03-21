import Hale.Base.Data.IORef
import Tests.Harness

open Data Tests

namespace TestIORef

def tests : IO (List TestResult) := do
  let mut results : List TestResult := []

  -- newIORef / readIORef
  results := results ++ [← checkEqIO "IORef new then read" 42 do
    let ref ← newIORef 42
    readIORef ref]

  -- writeIORef
  results := results ++ [← checkEqIO "IORef write then read" 99 do
    let ref ← newIORef 0
    writeIORef ref 99
    readIORef ref]

  -- modifyIORef
  results := results ++ [← checkEqIO "IORef modify (+10)" 52 do
    let ref ← newIORef 42
    modifyIORef ref (· + 10)
    readIORef ref]

  -- modifyIORef'
  results := results ++ [← checkEqIO "IORef modify' (*2)" 84 do
    let ref ← newIORef 42
    modifyIORef' ref (· * 2)
    readIORef ref]

  -- atomicModifyIORef
  results := results ++ [← checkEqIO "IORef atomicModify returns derived" "old=10" do
    let ref ← newIORef 10
    let result ← atomicModifyIORef ref (fun n => (n + 1, s!"old={n}"))
    pure result]

  -- verify atomicModifyIORef updates the ref
  results := results ++ [← checkEqIO "IORef atomicModify updates ref" 11 do
    let ref ← newIORef 10
    let _ ← atomicModifyIORef ref (fun n => (n + 1, n))
    readIORef ref]

  -- multiple modifications
  results := results ++ [← checkEqIO "IORef multiple modifies" 15 do
    let ref ← newIORef (0 : Nat)
    modifyIORef ref (· + 5)
    modifyIORef ref (· + 5)
    modifyIORef ref (· + 5)
    readIORef ref]

  -- atomicWriteIORef
  results := results ++ [← checkEqIO "IORef atomicWrite" 77 do
    let ref ← newIORef 0
    atomicWriteIORef ref 77
    readIORef ref]

  pure results

end TestIORef
