/-
  Hale.AutoUpdate.Control.AutoUpdate — Periodically updated cached values

  Provides a mechanism to run an IO action periodically in the background
  and cache its result. Consumers get the cached value without blocking.

  ## Design

  Mirrors Haskell's `Control.AutoUpdate`. Uses `IO.asTask` for the background
  update loop and `IO.Ref` for the cached value.

  ## Guarantees

  - The cached value is always available (non-blocking reads)
  - Updates happen at the configured interval (best-effort timing)
  - The initial value is computed eagerly before returning the getter
-/

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

/-- Create an auto-updating cached value. Returns an `IO α` action that
    reads the current cached value (non-blocking).

    The background task runs `settings.updateAction` every `settings.updateFreq`
    microseconds and stores the result.

    $$\text{mkAutoUpdate} : \text{UpdateSettings}(\alpha) \to \text{IO}(\text{IO}(\alpha))$$ -/
def mkAutoUpdate [Inhabited α] (settings : UpdateSettings α) : IO (IO α) := do
  -- Compute initial value eagerly
  let initial ← settings.updateAction
  let ref ← IO.mkRef initial
  -- Launch background update loop
  let _task ← IO.asTask (prio := .dedicated) do
    while true do
      IO.sleep (settings.updateFreq.toUInt32)
      let val ← settings.updateAction
      ref.set val
  -- Return a getter that reads the cached value
  pure ref.get

end Control
