/-
  Hale.TimeManager.System.TimeManager — Connection timeout management

  Provides a `Manager` that periodically sweeps registered handles
  and fires timeout callbacks for expired connections.

  ## Design

  Mirrors Haskell's `System.TimeManager`. Uses `IO.asTask` with `.dedicated`
  priority for the background sweep loop (dedicated OS thread, avoiding
  thread pool starvation) and `IO.Ref` for per-handle state.

  The sweep task is governed by a `Std.CancellationToken` so `Manager.stop`
  terminates the background thread promptly — this prevents compiled binaries
  from hanging on exit.

  ## Guarantees

  - `tickle` resets a handle's timeout (O(1))
  - `cancel` prevents future timeout firing
  - Sweep runs at configurable intervals
  - Thread-safe via `IO.Ref` atomicity
  - `stop` terminates the background sweep cooperatively
-/

import Hale.Time
import Std.Sync.CancellationToken

namespace System.TimeManager

/-- State of a timeout handle. -/
inductive HandleState where
  | active : Nat → HandleState    -- deadline (nanoseconds)
  | paused : HandleState
  | canceled : HandleState
deriving BEq, Repr

/-- A timeout handle for a single connection.
    $$\text{Handle} = \text{IO.Ref}(\text{HandleState})$$ -/
structure Handle where
  state : IO.Ref HandleState
  onTimeout : IO Unit

/-- The timeout manager. Periodically checks all registered handles.
    $$\text{Manager} = \{ \text{timeout} : \mathbb{N},\; \text{handles} : \text{IO.Ref}(\text{Array}(\text{Handle})) \}$$ -/
structure Manager where
  /-- Timeout duration in microseconds. -/
  timeoutUs : Nat
  /-- All registered handles. -/
  handles : IO.Ref (Array Handle)
  /-- Cancellation token for stopping the background sweep. -/
  token : Std.CancellationToken

/-- Initialize a new timeout manager with the given timeout (microseconds).
    Starts a background sweep task on a dedicated OS thread.
    $$\text{initialize} : \mathbb{N} \to \text{IO}(\text{Manager})$$ -/
def Manager.new (timeoutUs : Nat := 30000000) : IO Manager := do
  let handles ← IO.mkRef (#[] : Array Handle)
  let token ← Std.CancellationToken.new
  let mgr : Manager := ⟨timeoutUs, handles, token⟩
  -- Launch sweep loop on a dedicated OS thread
  let _task ← IO.asTask (prio := .dedicated) do
    while !(← token.isCancelled) do
      IO.sleep (timeoutUs / 1000).toUInt32  -- convert μs to ms
      unless (← token.isCancelled) do
        let now ← IO.monoNanosNow
        let hs ← handles.get
        for h in hs do
          let st ← h.state.get
          match st with
          | .active deadline =>
            if now > deadline then
              h.state.set .canceled
              try h.onTimeout catch _ => pure ()
          | _ => pure ()
        -- Compact: remove canceled handles
        let hs' ← handles.get
        let active ← hs'.toList.filterM fun h => do
          let st ← h.state.get
          pure (st != .canceled)
        handles.set active.toArray
  pure mgr

/-- Register a new connection with the manager. Returns a handle for
    managing its timeout.
    $$\text{register} : \text{Manager} \to \text{IO}(\text{Unit}) \to \text{IO}(\text{Handle})$$ -/
def Manager.register (mgr : Manager) (onTimeout : IO Unit) : IO Handle := do
  let now ← IO.monoNanosNow
  let deadline := now + mgr.timeoutUs * 1000  -- μs to ns
  let state ← IO.mkRef (HandleState.active deadline)
  let handle : Handle := ⟨state, onTimeout⟩
  mgr.handles.modify (·.push handle)
  pure handle

/-- Stop the manager's background sweep. The background task will exit
    after the current sleep interval completes. -/
def Manager.stop (mgr : Manager) : IO Unit :=
  mgr.token.cancel .cancel

/-- Reset the timeout for this handle (called on activity).
    $$\text{tickle} : \text{Handle} \to \text{Manager} \to \text{IO}(\text{Unit})$$ -/
def Handle.tickle (h : Handle) (mgr : Manager) : IO Unit := do
  let now ← IO.monoNanosNow
  let deadline := now + mgr.timeoutUs * 1000
  h.state.set (.active deadline)

/-- Cancel this handle's timeout. It will never fire.
    $$\text{cancel} : \text{Handle} \to \text{IO}(\text{Unit})$$ -/
def Handle.cancel (h : Handle) : IO Unit :=
  h.state.set .canceled

/-- Pause the timeout. The handle won't expire until resumed.
    $$\text{pause} : \text{Handle} \to \text{IO}(\text{Unit})$$ -/
def Handle.pause (h : Handle) : IO Unit :=
  h.state.set .paused

/-- Resume a paused handle with a fresh deadline.
    $$\text{resume} : \text{Handle} \to \text{Manager} \to \text{IO}(\text{Unit})$$ -/
def Handle.resume (h : Handle) (mgr : Manager) : IO Unit := do
  let now ← IO.monoNanosNow
  let deadline := now + mgr.timeoutUs * 1000
  h.state.set (.active deadline)

end System.TimeManager
