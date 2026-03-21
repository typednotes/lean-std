/-
  Hale.Base.Control.Exception — Structured exception handling

  Wraps Lean's `IO` error mechanism (`EIO IO.Error`) to provide
  Haskell-style combinators for catching, bracketing, and cleanup.

  ## Typing guarantees

  * **Resource safety (bracket):** The release action always runs,
    whether the use action succeeds or throws.  This is structural:
    `release` is called unconditionally after `use`.
  * **Cleanup ordering (finally):** The cleanup action runs after the
    primary action regardless of outcome.
  * **Selective cleanup (onException):** Cleanup runs only on failure.

  ## Axiom-dependent properties (documented, not machine-checked)

  * Correctness depends on Lean's `EIO.toBaseIO` faithfully capturing
    all exceptions as `Except.error`.
  * Asynchronous exceptions (e.g., thread cancellation) are not handled —
    Lean does not expose an async exception mechanism.
-/

import Hale.Base.Data.Either

namespace Control.Exception

/-- Try an action, catching IO errors into `Either String α`.

$$\text{try'} : \text{IO}\ \alpha \to \text{IO}\ (\text{Either}\ \text{String}\ \alpha)$$

On success, returns `Right a`.  On failure, returns `Left (toString e)`. -/
def try' (action : IO α) : IO (Data.Either String α) := do
  match ← action.toBaseIO with
  | .ok a    => pure (.right a)
  | .error e => pure (.left (toString e))

/-- Catch an IO error with a handler.

$$\text{catch'} : \text{IO}\ \alpha \to (\text{String} \to \text{IO}\ \alpha) \to \text{IO}\ \alpha$$

If `action` succeeds, returns its result.  If it throws, the stringified
error is passed to `handler`. -/
def catch' (action : IO α) (handler : String → IO α) : IO α := do
  match ← action.toBaseIO with
  | .ok a    => pure a
  | .error e => handler (toString e)

/-- Bracket pattern: acquire a resource, use it, and release it.

$$\text{bracket} : \text{IO}\ \alpha \to (\alpha \to \text{IO}\ \text{Unit}) \to (\alpha \to \text{IO}\ \beta) \to \text{IO}\ \beta$$

**Resource safety:** `release` is always called, whether `use` succeeds
or throws.  If `use` throws, the original error is re-thrown after
`release` completes. -/
def bracket (acquire : IO α) (release : α → IO Unit) (use : α → IO β) : IO β := do
  let a ← acquire
  let result ← (use a).toBaseIO
  release a
  match result with
  | .ok b    => pure b
  | .error e => throw e

/-- Ensure cleanup runs whether the action succeeds or fails.

$$\text{finally'} : \text{IO}\ \alpha \to \text{IO}\ \text{Unit} \to \text{IO}\ \alpha$$

Equivalent to `bracket (pure ()) (fun _ => cleanup) (fun _ => action)`.
Named `finally'` to avoid conflict with Lean's `finally` keyword. -/
def finally' (action : IO α) (cleanup : IO Unit) : IO α := do
  let result ← action.toBaseIO
  cleanup
  match result with
  | .ok a    => pure a
  | .error e => throw e

/-- Run cleanup only if the action throws.

$$\text{onException} : \text{IO}\ \alpha \to \text{IO}\ \text{Unit} \to \text{IO}\ \alpha$$

On success, cleanup is skipped.  On failure, cleanup runs then the
original error is re-thrown. -/
def onException (action : IO α) (cleanup : IO Unit) : IO α := do
  let result ← action.toBaseIO
  match result with
  | .ok a    => pure a
  | .error e => do cleanup; throw e

/-- Evaluate a pure value in IO, forcing any thunks.

$$\text{evaluate} : \alpha \to \text{IO}\ \alpha$$

In Lean (strict evaluation), this is simply `pure`.  Provided for
Haskell API compatibility. -/
@[inline] def evaluate (a : α) : IO α := pure a

end Control.Exception
