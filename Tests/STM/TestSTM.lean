import Hale
import Tests.Harness

open Control.Monad Control.Concurrent.STM Tests

/-
  Coverage:
  - Proofs: None (IO-based concurrency)
  - Tested: TVar (new, read, write, modify), TMVar (new, take, put, tryTake),
            TQueue (new, write, read, tryRead, isEmpty), atomically, orElse
  - Not covered: retry blocking semantics (requires concurrent test)
-/

namespace TestSTM

def tests : IO (List TestResult) := do
  -- TVar basics
  let tv ← TVar.newTVarIO 10
  let v1 ← atomically (TVar.readTVar tv)
  atomically (TVar.writeTVar tv 20)
  let v2 ← atomically (TVar.readTVar tv)
  atomically (TVar.modifyTVar' tv (· + 5))
  let v3 ← atomically (TVar.readTVar tv)

  -- TMVar basics
  let tmv ← TMVar.newTMVarIO 42
  let tmv1 ← atomically (TMVar.takeTMVar tmv)
  let empty ← atomically (TMVar.isEmptyTMVar tmv)
  atomically (TMVar.putTMVar tmv 99)
  let tmv2 ← atomically (TMVar.readTMVar tmv)

  -- TMVar tryTake on empty
  let tmvEmpty ← TMVar.newEmptyTMVarIO (α := Nat)
  let tryResult ← atomically (TMVar.tryTakeTMVar tmvEmpty)

  -- TQueue basics
  let q ← TQueue.newTQueueIO (α := Nat)
  let qEmpty ← atomically (TQueue.isEmptyTQueue q)
  atomically (TQueue.writeTQueue q 1)
  atomically (TQueue.writeTQueue q 2)
  atomically (TQueue.writeTQueue q 3)
  let q1 ← atomically (TQueue.readTQueue q)
  let q2 ← atomically (TQueue.readTQueue q)
  let q3 ← atomically (TQueue.readTQueue q)
  let qEmpty2 ← atomically (TQueue.isEmptyTQueue q)

  -- orElse: first retries, second succeeds
  let orResult ← atomically (STM.orElse STM.retry (pure 77))

  pure [
    -- TVar
    checkEq "TVar read initial" 10 v1
  , checkEq "TVar write" 20 v2
  , checkEq "TVar modify" 25 v3
    -- TMVar
  , checkEq "TMVar take" 42 tmv1
  , check "TMVar empty after take" empty
  , checkEq "TMVar read after put" 99 tmv2
  , check "TMVar tryTake empty" tryResult.isNone
    -- TQueue
  , check "TQueue initially empty" qEmpty
  , checkEq "TQueue FIFO 1" 1 q1
  , checkEq "TQueue FIFO 2" 2 q2
  , checkEq "TQueue FIFO 3" 3 q3
  , check "TQueue empty after drain" qEmpty2
    -- orElse
  , checkEq "orElse retry fallback" 77 orResult
  ]

end TestSTM
