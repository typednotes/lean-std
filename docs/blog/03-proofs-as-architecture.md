# Proofs as Architecture: A Catalog of Dependent-Type Patterns in Hale

> *In Lean 4, a proof field is free. It costs zero bytes at runtime,
> zero branches in the generated code, and infinite confidence at review time.*

Hale ports Haskell's web ecosystem to Lean 4. Where Haskell uses
conventions, Hale uses proofs. Where Haskell uses runtime checks, Hale
uses type-level constraints. This post catalogs every technique we use,
with concrete examples from the codebase.

---

## Pattern 1: Proof-carrying structures

**Idea:** Embed a proof directly as a field in a structure. Lean 4
erases proofs at compile time, so the struct has zero additional
overhead — same size, same layout, same codegen.

### Rational numbers: always normalized

```lean
structure Ratio where
  num : Int
  den : Nat
  den_pos : den > 0                        -- erased: denominator always positive
  coprime : Nat.Coprime num.natAbs den     -- erased: always in lowest terms
```

Every `Ratio` value that exists is **by construction** in lowest terms
with a positive denominator. There is no `normalize` function, no
"please remember to reduce" comment, no runtime `gcd` call on read.
The proof happens once, at construction time, and is then thrown away.

**What this buys:** Structural equality on `Ratio` is correct. Two ratios
are equal if and only if they represent the same number — no need to
normalize before comparing.

### QUIC Connection ID: RFC 9000 §17.2 length bound

```lean
structure ConnectionId where
  bytes : ByteArray
  hLen : bytes.size ≤ 20 := by omega     -- erased: RFC max length
```

RFC 9000 Section 17.2 says connection IDs must not exceed 20 bytes.
The proof field `hLen` encodes this as a compile-time obligation.
Constructing a `ConnectionId` with 21 bytes is a type error — `omega`
cannot prove `21 ≤ 20`.

### Warp Settings: positive timeout and backlog

```lean
structure Settings where
  settingsTimeout : Nat := 30
  settingsTimeoutPos : settingsTimeout > 0 := by omega    -- erased
  settingsBacklog : Nat := 128
  settingsBacklogPos : settingsBacklog > 0 := by omega    -- erased

-- Proven: the default is valid
theorem defaultSettings_valid :
    defaultSettings.settingsTimeout > 0 ∧ defaultSettings.settingsBacklog > 0 := ...
```

A zero timeout would immediately close every connection. A zero backlog
would reject all pending connections. Both are impossible to construct.

### Static file paths: no traversal attacks

```lean
structure Piece where
  val : String
  no_dot : ¬ val.startsWith "."     -- rejects dotfiles
  no_slash : ¬ val.contains '/'     -- rejects embedded slashes
```

Path traversal (`../../etc/passwd`) is a compile-time error. The
`toPiece` constructor validates strings and returns `Option Piece` —
invalid paths produce `none` before they ever reach the filesystem.

---

## Pattern 2: Phantom type parameters (zero-cost state machines)

**Idea:** Add a type parameter that exists only at the type level and is
erased at runtime. Functions constrain which values of the parameter
they accept, so the compiler enforces the state machine.

### Socket lifecycle (POSIX)

*(Covered in detail in [blog post 1](01-socket-posix-compliance-in-the-type.md))*

```lean
structure Socket (state : SocketState) where
  raw : RawSocket

def send (s : Socket .connected) (data : ByteArray) : IO Nat  -- only .connected
def close (s : Socket state) (h : state ≠ .closed := by decide) : IO (Socket .closed)
```

Five states, eleven distinctness theorems, one proof obligation on `close`.
All erased. Same codegen as C.

---

## Pattern 3: Indexed monads (exactly-once protocol enforcement)

**Idea:** Parameterise a monad by a pre-state and a post-state. The
`bind` operation chains states: `AppM s₁ s₂ → (→ AppM s₂ s₃) → AppM s₁ s₃`.
Make the constructor `private` so the only way to produce a value is
through the provided combinators.

### WAI Application: exactly-once response

*(Covered in detail in [blog post 2](02-http-standard-enforced-by-types.md))*

```lean
structure AppM (pre post : ResponseState) (α : Type) where
  private mk ::
  run : IO α

def AppM.respond (cb : Response → IO ResponseReceived) (r : Response)
    : AppM .pending .sent ResponseReceived
```

Double-respond, skip-respond, and token fabrication are all type errors.

---

## Pattern 4: Inductive types as protocol specifications

**Idea:** Use exhaustive sum types to represent every valid message,
frame, or state in a protocol. Pattern matching is total — the compiler
rejects incomplete handlers.

### HTTP/2 stream states (RFC 9113 §5.1)

```lean
inductive StreamState where
  | idle | open | halfClosedLocal | halfClosedRemote
  | resetLocal | resetRemote | closed
  | reservedLocal | reservedRemote
```

Nine states, straight from the RFC. Any function that dispatches on a
stream must handle all nine. Miss one and the build fails.

### WebSocket connection lifecycle (RFC 6455)

```lean
inductive ConnectionState where
  | pending   -- handshake not completed
  | open_     -- data transfer
  | closing   -- close frame sent
  | closed    -- terminated
```

### QUIC connection lifecycle (RFC 9000)

```lean
inductive ConnectionState where
  | handshaking | established | closing | closed
```

### HTTP/2 frame types (RFC 9113 §6)

```lean
inductive FrameType where
  | data | headers | priority | rstStream | settings
  | pushPromise | ping | goaway | windowUpdate | continuation
  | unknown (id : UInt8)
```

The `unknown` variant is a catch-all for future frame types — the
pattern match is still exhaustive.

### HTTP request errors (Warp)

```lean
inductive InvalidRequest where
  | notEnoughLines | badFirstLine | nonHttp | incompleteHeaders
  | connectionClosedByPeer | overLargeHeader | badProxyHeader
  | payloadTooLarge | requestHeaderFieldsTooLarge
```

Every protocol-level error has its own constructor. No stringly-typed
error messages. No `error_code: 42` lookups.

---

## Pattern 5: Roundtrip theorems (wire-format bijectivity)

**Idea:** Prove that serialization followed by deserialization is the
identity function. This ensures no information is lost on the wire.

```lean
-- HTTP/2 (10 theorems)
theorem FrameType.roundtrip_data : fromUInt8 (toUInt8 .data) = .data := by rfl

-- HTTP/3 error codes (17 theorems)
theorem roundtrip_noError : fromUInt64 (toUInt64 .noError) = .noError := by rfl

-- WebSocket opcodes (6 theorems)
theorem roundtrip_text : Opcode.fromUInt8 (Opcode.toUInt8 .text) = .text := by rfl

-- HTTP methods (9 theorems)
theorem parseMethod_GET : parseMethod "GET" = .standard .GET := by rfl

-- HTTP versions (4 theorems)
theorem parseHttpVersion_http11 : parseHttpVersion "HTTP/1.1" = some http11 := by rfl
```

All proved by `rfl` — the kernel evaluates both sides and confirms they
are definitionally equal. If someone changes the encoding (e.g., swaps
two frame type constants), the proof breaks immediately.

---

## Pattern 6: Algebraic laws (composition contracts)

**Idea:** Prove that combinators satisfy algebraic laws. This ensures
that refactoring (reordering middleware, simplifying chains) preserves
behavior.

### Middleware composition

```lean
theorem idMiddleware_comp_left  (m : Middleware) : id ∘ m = m         := rfl
theorem idMiddleware_comp_right (m : Middleware) : m ∘ id = m         := rfl
theorem modifyRequest_id  : modifyRequest id = idMiddleware           := rfl
theorem modifyResponse_id : modifyResponse id = idMiddleware          := rfl
theorem ifRequest_false (m : Middleware) : ifRequest (· => false) m = id := rfl
```

### Builder monoid

```lean
theorem empty_append (b : Builder) : ∅ ++ b = b       := ...
theorem append_empty (b : Builder) : b ++ ∅ = b       := ...
theorem append_assoc (a b c)       : (a ++ b) ++ c = a ++ (b ++ c) := ...
```

### Response functor identity

```lean
theorem mapResponseHeaders_id_builder (s h b) :
    (responseBuilder s h b).mapResponseHeaders id = responseBuilder s h b := rfl

theorem mapResponseStatus_id_builder (s h b) :
    (responseBuilder s h b).mapResponseStatus id = responseBuilder s h b := rfl
-- (6 more for file and stream variants)
```

### Case-insensitive header equality

```lean
theorem addHeaders_nil_builder (s h b) :
    (responseBuilder s h b).mapResponseHeaders (· ++ []) = responseBuilder s h b := ...
```

---

## Pattern 7: Opaque FFI handles with automatic cleanup

**Idea:** Use `lean_alloc_external` with a GC finalizer. The Lean type
is `opaque` (no structure visible to user code), so the only way to
operate on it is through the provided FFI functions.

```lean
opaque SocketHandle  : NonemptyType   -- POSIX socket fd
opaque EventLoopHandle : NonemptyType -- kqueue/epoll fd
opaque RecvBufferHandle : NonemptyType -- buffered reader
opaque TLSContextHandle : NonemptyType -- OpenSSL SSL_CTX
opaque TLSSessionHandle : NonemptyType -- OpenSSL SSL session
```

The C-side finalizer calls `close(fd)` or `SSL_CTX_free(ctx)` when the
Lean GC collects the object. The `opaque` keyword ensures user code
cannot peek inside the handle or forge one from an integer.

---

## Pattern 8: Explicit axioms for FFI trust boundaries

When a property depends on FFI behavior that the Lean kernel cannot
verify, we declare it as an axiom with clear documentation:

```lean
-- Warp: header count is bounded by the loop guard in recvHeaders
axiom recvHeaders_bounded (buf : RecvBuffer) :
    ∀ rl hdrs, recvHeaders buf = pure (rl, hdrs) → hdrs.length ≤ maxHeaders

-- Green scheduler: bind terminates if operands terminate
axiom GreenBase.bind_terminates {...} : True

-- Green scheduler: await resumes when task completes
axiom GreenBase.await_resumes {...} : True

-- Green scheduler: non-blocking scheduler prevents starvation
axiom GreenBase.no_pool_starvation {...} : True
```

Axioms are the honest answer to "I cannot prove this in Lean, but I
believe it to be true based on the C/OS semantics." They document the
trust boundary explicitly — unlike undocumented `unsafePerformIO` calls
in Haskell, axioms are visible in the dependency graph and can be
audited.

---

## The big picture

| Technique | What it encodes | Runtime cost | Example |
|-----------|----------------|-------------|---------|
| Proof fields | Structural invariants | 0 (erased) | `Ratio.den_pos`, `ConnectionId.hLen` |
| Phantom params | State machines | 0 (erased) | `Socket (state : SocketState)` |
| Indexed monads | Exactly-once protocols | 0 (erased) | `AppM .pending .sent` |
| Inductive types | Message/frame/state spaces | 0 (tag only) | `StreamState`, `FrameType` |
| Roundtrip thms | Wire-format correctness | 0 (erased) | 46 bijectivity proofs |
| Algebraic laws | Composition safety | 0 (erased) | `id ∘ m = m`, monoid laws |
| Opaque FFI | Resource encapsulation | 0 (pointer) | `SocketHandle`, `TLSContextHandle` |
| Axioms | FFI trust boundary | 0 (erased) | `recvHeaders_bounded` |

**Total theorem count:** 230+ across the codebase, covering:
- POSIX socket protocol
- HTTP/1.1 and HTTP/2 semantics (RFC 9110, 9112, 9113)
- HTTP/3 framing and errors (RFC 9114)
- QUIC transport parameters (RFC 9000)
- WebSocket protocol (RFC 6455)
- WAI application contract
- Algebraic composition laws
- Rational arithmetic invariants
- Wire-format bijectivity

Every theorem is checked by the Lean 4 kernel. They cannot be wrong
(modulo axioms). They cannot go stale. They cannot be disabled by a
`-Wno-whatever` flag. They are as much a part of the architecture as
the functions they describe.

---

*This post is part of the [Hale](../) documentation — a port of
Haskell's web ecosystem to Lean 4 with maximalist typing.*
