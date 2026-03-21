/-
  Hale.Base.System.Environment — Environment variable and argument access

  Wraps Lean's `IO.getEnv` to provide Haskell-compatible access
  to environment variables.

  ## Typing guarantees

  * **Option-based lookup:** `lookupEnv` returns `Option String`, making
    missing variables explicit in the type.
  * **Default fallback:** `getEnv` provides a total accessor returning
    empty string for missing variables.

  ## Axiom-dependent properties

  * Correctness depends on Lean's FFI to the OS environment.
  * `IO.getEnv` is `BaseIO`, meaning it cannot throw IO errors.
-/

namespace System.Environment

/-- Look up an environment variable by name.

$$\text{lookupEnv} : \text{String} \to \text{IO}\ (\text{Option}\ \text{String})$$

Returns `some value` if the variable is set, `none` otherwise. -/
@[inline] def lookupEnv (name : String) : IO (Option String) :=
  IO.getEnv name

/-- Get an environment variable, returning empty string if not set.

$$\text{getEnv} : \text{String} \to \text{IO}\ \text{String}$$

Total accessor: returns `""` when the variable is absent. -/
def getEnv (name : String) : IO String := do
  match ← lookupEnv name with
  | some v => pure v
  | none   => pure ""

/-- Get the value of the `HOME` environment variable.

$$\text{getHome} : \text{IO}\ (\text{Option}\ \text{String})$$ -/
def getHome : IO (Option String) := lookupEnv "HOME"

/-- Get the value of the `PATH` environment variable.

$$\text{getPath} : \text{IO}\ (\text{Option}\ \text{String})$$ -/
def getPath : IO (Option String) := lookupEnv "PATH"

end System.Environment
