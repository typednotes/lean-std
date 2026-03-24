# Zero-Cost POSIX Compliance: Encoding the Socket State Machine in Lean 4's Type System

> *The best runtime check is the one that never runs.*

## The problem

The POSIX socket API is a state machine. A socket must be created, then
bound, then set to listen, before it can accept connections. Calling
operations in the wrong order — `send` on an unbound socket, `accept`
before `listen`, `close` twice — is undefined behaviour in C.

Every production socket library deals with this in one of three ways:

1. **Runtime checks** — assert the state at every call, throw on violation
   (Python, Java, Go).
2. **Documentation** — trust the programmer to read the man page (C, Rust).
3. **Ignore it** — let the OS return `EBADF` and hope someone checks the
   return code.

All three push the bug to runtime. Lean 4 offers a fourth option:
**make the bug unrepresentable at the type level, then erase the proof
at compile time so the generated code is identical to raw C**.

## The state machine

```
Fresh ──bind──→ Bound ──listen──→ Listening ──accept──→ Connected
  │                                                      (send/recv)
  └──connect──→ Connected                                    │
                                                             │
Any state ──close(proof: state ≠ .closed)──→ Closed
```

Five states, seven transitions, and one proof obligation. That is the
entire POSIX socket protocol. Let us encode it.

## Step 1: States as an inductive type

```lean
inductive SocketState where
  | fresh      -- socket() returned, nothing else happened
  | bound      -- bind() succeeded
  | listening  -- listen() succeeded
  | connected  -- connect() or accept() produced this socket
  | closed     -- close() succeeded — terminal state
deriving DecidableEq
```

`DecidableEq` gives us `by decide` for free — the compiler can prove
any two concrete states are distinct without any user effort.

## Step 2: The socket carries its state as a phantom parameter

```lean
structure Socket (state : SocketState) where
  protected mk ::
  raw : RawSocket    -- opaque FFI handle (lean_alloc_external)
```

The `state` parameter exists only at the type level. It is **erased at
runtime**: a `Socket .fresh` and a `Socket .connected` have the exact
same memory layout (a single pointer to the OS file descriptor). Zero
overhead.

The constructor is `protected` to prevent casual state fabrication.

## Step 3: Each function declares its pre- and post-state

```lean
-- Creation: produces .fresh
def socket (fam : Family) (typ : SocketType) : IO (Socket .fresh)

-- Binding: requires .fresh, produces .bound
def bind (s : Socket .fresh) (addr : SockAddr) : IO (Socket .bound)

-- Listening: requires .bound, produces .listening
def listen (s : Socket .bound) (backlog : Nat) : IO (Socket .listening)

-- Accepting: requires .listening, produces .connected
def accept (s : Socket .listening) : IO (Socket .connected × SockAddr)

-- Connecting: requires .fresh, produces .connected
def connect (s : Socket .fresh) (addr : SockAddr) : IO (Socket .connected)

-- Sending/receiving: requires .connected
def send (s : Socket .connected) (data : ByteArray) : IO Nat
def recv (s : Socket .connected) (maxlen : Nat)     : IO ByteArray
```

The Lean 4 kernel threads these constraints through the program. If you
write `send freshSocket data`, the kernel sees `Socket .fresh` where it
expects `Socket .connected`, and reports a type error. No runtime check.
No assertion. No exception. No branch in the generated code.

## Step 4: Double-close prevention via proof obligation

```lean
def close (s : Socket state)
          (_h : state ≠ .closed := by decide)
          : IO (Socket .closed)
```

This is where dependent types shine brightest. The second parameter is a
**proof** that the socket is not already closed. Let us trace what happens
for each concrete state:

| Call | Proof obligation | `by decide` | Result |
|------|-----------------|-------------|--------|
| `close (s : Socket .fresh)` | `.fresh ≠ .closed` | **trivially true** | compiles |
| `close (s : Socket .bound)` | `.bound ≠ .closed` | **trivially true** | compiles |
| `close (s : Socket .listening)` | `.listening ≠ .closed` | **trivially true** | compiles |
| `close (s : Socket .connected)` | `.connected ≠ .closed` | **trivially true** | compiles |
| `close (s : Socket .closed)` | `.closed ≠ .closed` | **impossible** | **type error** |

For the first four, the default tactic `by decide` discharges the proof
automatically — the caller writes nothing. For the fifth, the proposition
`.closed ≠ .closed` is **logically false**: no proof can exist, so the
program is rejected at compile time.

The proof is erased during compilation. The generated C code is:
```c
lean_object* close(lean_object* socket) {
    close_fd(lean_get_external_data(socket));
    return lean_io_result_mk_ok(lean_box(0));
}
```

No branch. No flag. No state field. The proof did its job during
type-checking and vanished.

## Step 5: Distinctness theorems

We also prove that all five states are pairwise distinct:

```lean
theorem SocketState.fresh_ne_bound     : .fresh ≠ .bound     := by decide
theorem SocketState.fresh_ne_listening : .fresh ≠ .listening := by decide
theorem SocketState.fresh_ne_connected : .fresh ≠ .connected := by decide
theorem SocketState.fresh_ne_closed    : .fresh ≠ .closed    := by decide
theorem SocketState.bound_ne_listening : .bound ≠ .listening := by decide
-- ... (11 theorems total)
```

These are trivially proved by `decide` (the kernel evaluates the `BEq`
instance). They exist so that downstream code can use them as lemmas
without re-proving distinctness.

## What the compiler actually rejects

```lean
-- ❌ send on a fresh socket
let s ← socket .inet .stream
send s "hello".toUTF8
-- Error: type mismatch — expected Socket .connected, got Socket .fresh

-- ❌ accept before listen
let s ← socket .inet .stream
let s ← bind s addr
accept s
-- Error: type mismatch — expected Socket .listening, got Socket .bound

-- ❌ double close
let s ← socket .inet .stream
let s ← close s
close s
-- Error: state ≠ .closed — proposition .closed ≠ .closed is false

-- ✅ correct sequence
let s ← socket .inet .stream
let s ← bind s ⟨"0.0.0.0", 8080⟩
let s ← listen s 128
let (conn, addr) ← accept s
let _ ← send conn "HTTP/1.1 200 OK\r\n\r\n".toUTF8
let _ ← close conn
let _ ← close s
```

## Try it yourself

Lean 4 runs in the browser via [live.lean-lang.org](https://live.lean-lang.org).

Paste this into the editor and observe the red squiggles:

```lean
-- Simplified version (no FFI, just the type-level encoding)
inductive SocketState where
  | fresh | bound | listening | connected | closed
deriving DecidableEq, Repr

structure Socket (state : SocketState) where
  id : Nat

def bind (s : Socket .fresh) : Socket .bound := ⟨s.id⟩
def listen (s : Socket .bound) : Socket .listening := ⟨s.id⟩
def accept (s : Socket .listening) : Socket .connected := ⟨s.id⟩
def send (s : Socket .connected) (msg : String) : String := msg
def close (s : Socket state) (_h : state ≠ .closed := by decide) : Socket .closed := ⟨s.id⟩

-- ✅ This type-checks:
def good : String :=
  let s : Socket .fresh := ⟨42⟩
  let s := bind s
  let s := listen s
  let s := accept s
  let msg := send s "hello"
  let _ := close s
  msg

-- ❌ Uncomment this — it will NOT type-check:
-- def bad_double_close : Socket .closed :=
--   let s : Socket .fresh := ⟨42⟩
--   let s := close s
--   close s   -- Error: .closed ≠ .closed is false

-- ❌ Uncomment this — it will NOT type-check:
-- def bad_send_fresh : String :=
--   let s : Socket .fresh := ⟨42⟩
--   send s "hello"   -- Error: expected .connected, got .fresh
```

Try uncommenting the failing examples. The error messages are immediate
and precise — the kernel tells you exactly which state transition is
invalid.

## The punchline

| Approach | Lines of state-checking code | Runtime cost | Catches at |
|----------|------|------|------|
| C (man page) | 0 | 0 | never (UB) |
| Python (runtime) | ~50 | branch per call | runtime |
| Rust (typestate) | ~30 | 0 | compile-time |
| **Lean 4 (dependent types)** | **0** | **0** | **compile-time** |

Lean 4 is unique: the proof obligation `state ≠ .closed` is a **real
logical proposition** that the kernel verifies. It is not a lint, not a
static analysis heuristic, not a convention. It is a mathematical proof
of protocol compliance, checked by the same kernel that verifies
Mathlib's theorems — and then thrown away so the generated code runs at
the speed of C.

---

*This post is part of the [Hale](../) documentation — a port of
Haskell's web ecosystem to Lean 4 with maximalist typing.*
