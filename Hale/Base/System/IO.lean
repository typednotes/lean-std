/-
  Hale.Base.System.IO — Handle-based IO operations

  Wraps Lean's `IO.FS.Handle` and `IO.FS.Stream` to provide
  Haskell-compatible handle-based IO in the style of `System.IO`.

  ## Design

  Lean's `IO.getStdin`/`IO.getStdout`/`IO.getStderr` return `IO.FS.Stream`,
  while file handles are `IO.FS.Handle`.  We use `IO.FS.Handle` as the
  primary handle type for file operations, and provide stream-based wrappers
  for standard IO.

  ## Typing guarantees

  * **Mode safety:** `IOMode` is a closed enumeration; `toFSMode` maps it
    exhaustively to `IO.FS.Mode`.
  * **withFile cleanup:** The handle is scoped to the callback; no handle
    leak is possible.

  ## Axiom-dependent properties

  * Correctness of actual IO depends on Lean's runtime and OS behaviour.
-/

namespace System.SysIO

/-- IO mode for opening files.

$$\text{IOMode} ::= \text{readMode} \mid \text{writeMode} \mid \text{appendMode} \mid \text{readWriteMode}$$ -/
inductive IOMode where
  /-- Open for reading. -/
  | readMode
  /-- Open for writing (truncates existing content). -/
  | writeMode
  /-- Open for appending. -/
  | appendMode
  /-- Open for both reading and writing. -/
  | readWriteMode
  deriving BEq, Repr

/-- Handle is an alias for Lean's `IO.FS.Handle`.

$$\text{Handle} \triangleq \text{IO.FS.Handle}$$ -/
abbrev Handle := IO.FS.Handle

/-- Standard input stream.

$$\text{stdin} : \text{IO}\ \text{IO.FS.Stream}$$ -/
def stdin : IO IO.FS.Stream := IO.getStdin

/-- Standard output stream.

$$\text{stdout} : \text{IO}\ \text{IO.FS.Stream}$$ -/
def stdout : IO IO.FS.Stream := IO.getStdout

/-- Standard error stream.

$$\text{stderr} : \text{IO}\ \text{IO.FS.Stream}$$ -/
def stderr : IO IO.FS.Stream := IO.getStderr

/-- Write a string to a handle.

$$\text{hPutStr} : \text{Handle} \to \text{String} \to \text{IO}\ \text{Unit}$$ -/
@[inline] def hPutStr (h : Handle) (s : String) : IO Unit := h.putStr s

/-- Write a string followed by a newline to a handle.

$$\text{hPutStrLn} : \text{Handle} \to \text{String} \to \text{IO}\ \text{Unit}$$ -/
def hPutStrLn (h : Handle) (s : String) : IO Unit := do
  h.putStr s
  h.putStr "\n"

/-- Read a line from a handle.

$$\text{hGetLine} : \text{Handle} \to \text{IO}\ \text{String}$$ -/
@[inline] def hGetLine (h : Handle) : IO String := h.getLine

/-- Flush a handle's output buffer.

$$\text{hFlush} : \text{Handle} \to \text{IO}\ \text{Unit}$$ -/
@[inline] def hFlush (h : Handle) : IO Unit := h.flush

/-- Read all remaining contents from a handle.

$$\text{hGetContents} : \text{Handle} \to \text{IO}\ \text{String}$$

Uses Lean's `readToEnd` which reads until EOF. -/
@[inline] def hGetContents (h : Handle) : IO String := h.readToEnd

/-- Convert an `IOMode` to Lean's `IO.FS.Mode`.

$$\text{toFSMode} : \text{IOMode} \to \text{IO.FS.Mode}$$ -/
def toFSMode : IOMode → IO.FS.Mode
  | .readMode      => .read
  | .writeMode     => .write
  | .appendMode    => .append
  | .readWriteMode => .readWrite

/-- Open a file with the given mode, use it via the callback, then return.

$$\text{withFile} : \text{FilePath} \to \text{IOMode} \to (\text{Handle} \to \text{IO}\ \alpha) \to \text{IO}\ \alpha$$

The handle is scoped to the callback `f`. -/
def withFile (path : System.FilePath) (mode : IOMode) (f : Handle → IO α) : IO α := do
  let h ← IO.FS.Handle.mk path (toFSMode mode)
  f h

/-- Convenience: write a string to stdout.

$$\text{putStr} : \text{String} \to \text{IO}\ \text{Unit}$$ -/
@[inline] def putStr (s : String) : IO Unit := IO.print s

/-- Convenience: write a string with newline to stdout.

$$\text{putStrLn} : \text{String} \to \text{IO}\ \text{Unit}$$ -/
@[inline] def putStrLn (s : String) : IO Unit := IO.println s

/-- Convenience: read a line from stdin.

$$\text{getLine} : \text{IO}\ \text{String}$$ -/
def getLine : IO String := do (← IO.getStdin).getLine

end System.SysIO
