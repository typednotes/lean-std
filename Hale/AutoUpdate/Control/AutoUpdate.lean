/-
  Hale.AutoUpdate.Control.AutoUpdate — Periodically updated cached values

  Provides a mechanism to run an IO action periodically in the background
  and cache its result. Consumers get the cached value without blocking.

  ## Design

  Mirrors Haskell's `Control.AutoUpdate`. Uses `IO.asTask` with `.dedicated`
  priority for the background update loop (dedicated OS thread, avoiding
  thread pool starvation) and `IO.Ref` for the cached value.

  The background task is governed by a `Std.CancellationToken` so it can be
  stopped cleanly — this prevents compiled binaries from hanging on exit.

  ## Guarantees

  - The cached value is always available (non-blocking reads)
  - Updates happen at the configured interval (best-effort timing)
  - The initial value is computed eagerly before returning the getter
  - `stop` terminates the background task cooperatively
-/

import Std.Sync.CancellationToken

namespace Control

/-- Configuration for an auto-updating cached value.
    $$\text{UpdateSettings}(\alpha) = \{ \text{interval} : \mathbb{N},\; \text{action} : \text{IO}(\alpha) \}$$ -/
structure UpdateSettings (α : Type) where
  /-- Update interval in microseconds. -/
  updateFreq : Nat := 1000000  -- 1 second default
  /-- The IO action to run periodically to refresh the value. -/
  updateAction : IO α

namespace UpdateSettings

/-- Default settings with 1-second interval. -/
def default (action : IO α) : UpdateSettings α :=
  { updateAction := action }

end UpdateSettings

/-- Handle to a running auto-update background task.
    Provides a getter for the cached value and a stop action. -/
structure AutoUpdate (α : Type) where
  /-- Read the current cached value (non-blocking). -/
  get : IO α
  /-- Stop the background update task. -/
  stop : IO Unit

/-- Create an auto-updating cached value. Returns an `AutoUpdate` handle
    with a non-blocking getter and a stop action.

    The background task runs `settings.updateAction` every `settings.updateFreq`
    microseconds and stores the result. Uses a dedicated OS thread to avoid
    thread pool starvation.

    $$\text{mkAutoUpdate} : \text{UpdateSettings}(\alpha) \to \text{IO}(\text{AutoUpdate}(\alpha))$$ -/
def mkAutoUpdate [Inhabited α] (settings : UpdateSettings α) : IO (AutoUpdate α) := do
  -- Compute initial value eagerly
  let initial ← settings.updateAction
  let ref ← IO.mkRef initial
  let token ← Std.CancellationToken.new
  -- Launch background update loop on a dedicated OS thread
  let _task ← IO.asTask (prio := .dedicated) do
    while !(← token.isCancelled) do
      IO.sleep (settings.updateFreq / 1000).toUInt32  -- convert μs to ms
      unless (← token.isCancelled) do
        let val ← settings.updateAction
        ref.set val
  pure { get := ref.get, stop := token.cancel .cancel }

end Control
