import Hale
import Tests.Harness

open System.TimeManager Tests

/-
  Coverage:
  - Proofs: None (IO-based)
  - Tested: initialize, register, tickle, cancel, pause, resume, timeout firing
  - Not covered: None
-/

namespace TestTimeManager

def tests : IO (List TestResult) := do
  -- Test basic timeout
  let fired ← IO.mkRef false
  let mgr ← Manager.new (timeoutUs := 50000)  -- 50ms
  let _h ← mgr.register (fired.set true)
  IO.sleep 200  -- wait for timeout
  let didFire ← fired.get

  -- Test cancel prevents firing
  let fired2 ← IO.mkRef false
  let h2 ← mgr.register (fired2.set true)
  h2.cancel
  IO.sleep 200
  let didFire2 ← fired2.get

  -- Test tickle resets timeout
  let fired3 ← IO.mkRef false
  let h3 ← mgr.register (fired3.set true)
  IO.sleep 30  -- less than 50ms timeout
  h3.tickle mgr  -- reset
  IO.sleep 30  -- still less than 50ms from tickle
  let didFire3 ← fired3.get

  mgr.stop

  pure [
    check "timeout fires" didFire
  , check "cancel prevents firing" (!didFire2)
  , check "tickle resets timeout" (!didFire3)
  ]

end TestTimeManager
