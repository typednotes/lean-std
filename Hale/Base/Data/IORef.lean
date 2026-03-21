/-
  Hale.Base.Data.IORef — Mutable references in IO

  Provides a Haskell-compatible API for mutable references, wrapping
  Lean's `IO.Ref`. Since Lean is strictly evaluated, `modifyIORef` and
  `modifyIORef'` are identical (both strict).

  ## Design

  `IORef α` is an abbreviation for `IO.Ref α`. All operations delegate
  directly to Lean's standard library. No proofs are provided since
  `IO` is opaque.
-/

namespace Data

/-- A mutable reference in `IO`, wrapping Lean's `IO.Ref`.
    $$\text{IORef}(\alpha) \cong \text{IO.Ref}(\alpha)$$ -/
abbrev IORef (α : Type) := IO.Ref α

/-- Create a new `IORef` with an initial value.
    $$\text{newIORef} : \alpha \to \text{IO}(\text{IORef}\ \alpha)$$ -/
@[inline] def newIORef (a : α) : IO (IORef α) := IO.mkRef a

/-- Read the current value of an `IORef`.
    $$\text{readIORef} : \text{IORef}\ \alpha \to \text{IO}\ \alpha$$ -/
@[inline] def readIORef (ref : IORef α) : IO α := ref.get

/-- Write a new value to an `IORef`, replacing the old.
    $$\text{writeIORef} : \text{IORef}\ \alpha \to \alpha \to \text{IO}\ ()$$ -/
@[inline] def writeIORef (ref : IORef α) (a : α) : IO Unit := ref.set a

/-- Apply a function to the value in an `IORef`.
    $$\text{modifyIORef}(\text{ref}, f) \equiv \text{ref} \gets f(\text{ref})$$ -/
@[inline] def modifyIORef (ref : IORef α) (f : α → α) : IO Unit := ref.modify f

/-- Strict version of `modifyIORef`. In Lean (strict evaluation) this is identical to `modifyIORef`.
    $$\text{modifyIORef'} = \text{modifyIORef}$$ -/
@[inline] def modifyIORef' (ref : IORef α) (f : α → α) : IO Unit := ref.modify f

/-- Atomically modify an `IORef`, returning a derived value.
    Applies $f$ to the current value, stores the first component, and returns the second.
    $$\text{atomicModifyIORef}(\text{ref}, f) : \text{IO}\ \beta$$
    where $f : \alpha \to (\alpha \times \beta)$

    Note: not truly atomic w.r.t. concurrent access (use `MVar` or `Mutex` for that). -/
def atomicModifyIORef (ref : IORef α) (f : α → α × β) : IO β := do
  let a ← ref.get
  let (a', b) := f a
  ref.set a'
  pure b

/-- Atomically modify an `IORef` without returning a value.
    $$\text{atomicModifyIORef\_}(\text{ref}, f) \equiv \text{ref} \gets \pi_1(f(\text{ref}))$$ -/
def atomicModifyIORef_ (ref : IORef α) (f : α → α × β) : IO Unit := do
  let _ ← atomicModifyIORef ref f

/-- Atomically write a new value, returning the old.
    $$\text{atomicWriteIORef}(\text{ref}, a) \equiv \text{writeIORef}(\text{ref}, a)$$
    In Lean this is just `writeIORef` (strict evaluation makes it equivalent). -/
@[inline] def atomicWriteIORef (ref : IORef α) (a : α) : IO Unit := ref.set a

end Data
