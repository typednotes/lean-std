/-
  Hale.Base.System.Exit — Exit codes and process termination

  Provides Haskell-compatible `ExitCode` type and exit functions,
  wrapping Lean's `IO.Process.exit`.

  ## Typing guarantees

  * **Exhaustive exit codes:** `ExitCode` is a closed sum —
    either `success` (code 0) or `failure n` for arbitrary `UInt32`.
  * **No-return:** `exitWith`, `exitSuccess`, `exitFailure` return
    `IO α` for any `α`, encoding non-return at the type level.

  ## Axiom-dependent properties

  * Actual process termination depends on the Lean runtime calling
    the OS `exit()` syscall.
  * Lean's `IO.Process.exit` takes `UInt8`, so exit codes above 255
    are truncated via `UInt32.toUInt8`.
-/

namespace System

/-- Exit codes for process termination.

$$\text{ExitCode} ::= \text{success} \mid \text{failure}(n : \mathbb{N}_{32})$$ -/
inductive ExitCode where
  /-- Successful termination (code 0). -/
  | success : ExitCode
  /-- Failure with an exit code. -/
  | failure : UInt32 → ExitCode
  deriving BEq, Repr

namespace ExitCode

instance : ToString ExitCode where
  toString
    | .success   => "ExitSuccess"
    | .failure n => s!"ExitFailure({n})"

/-- Convert an `ExitCode` to its numeric representation.

$$\text{toUInt32}(\text{success}) = 0, \quad \text{toUInt32}(\text{failure}(n)) = n$$ -/
@[inline] def toUInt32 : ExitCode → UInt32
  | .success   => 0
  | .failure n => n

/-- Test if the exit code represents success.

$$\text{isSuccess}(\text{success}) = \text{true}$$ -/
@[inline] def isSuccess : ExitCode → Bool
  | .success => true
  | .failure _ => false

-- ── Proofs ─────────────────────────────────────

/-- Success has code 0. -/
theorem success_toUInt32 : ExitCode.success.toUInt32 = 0 := rfl

/-- `isSuccess` is true only for `success`. -/
theorem isSuccess_iff (c : ExitCode) : c.isSuccess = true ↔ c = .success := by
  cases c with
  | success => simp [isSuccess]
  | failure n => simp [isSuccess]

end ExitCode

/-- Exit the process with the given exit code.

$$\text{exitWith} : \text{ExitCode} \to \text{IO}\ \alpha$$

**Note:** Lean's `IO.Process.exit` takes `UInt8`, so codes above 255
are truncated. -/
def exitWith (code : ExitCode) : IO α :=
  match code with
  | .success   => IO.Process.exit 0
  | .failure n => IO.Process.exit n.toUInt8

/-- Exit successfully (code 0).

$$\text{exitSuccess} : \text{IO}\ \alpha$$ -/
def exitSuccess : IO α := exitWith .success

/-- Exit with failure code 1.

$$\text{exitFailure} : \text{IO}\ \alpha$$ -/
def exitFailure : IO α := exitWith (.failure 1)

end System
