# The HTTP Standard, Enforced by Types

> *RFC 9110 has 200 pages. We turned the important ones into theorems.*

## What if the spec could check itself?

HTTP is specified across several RFCs: 9110 (semantics), 9113 (HTTP/2),
9114 (HTTP/3), 9000 (QUIC), 6455 (WebSocket). Each defines methods,
status codes, frame types, error codes, state machines, and properties
like "safe methods do not modify server state" or "1xx responses MUST
NOT contain a body."

In every mainstream HTTP library, these rules are comments in the source
code. In Hale, they are **theorems checked by the Lean 4 kernel**.

---

## 1. Method semantics (RFC 9110 §9.2)

RFC 9110 classifies methods as *safe* (read-only) or *idempotent*
(repeatable). It also states that **every safe method is idempotent**.
We encode the definitions and prove the implication:

```lean
-- The definitions
def Method.isSafe : Method → Bool
  | .standard .GET     => true
  | .standard .HEAD    => true
  | .standard .OPTIONS => true
  | .standard .TRACE   => true
  | _                  => false

def Method.isIdempotent : Method → Bool
  | .standard .PUT    => true
  | .standard .DELETE => true
  | m                 => m.isSafe

-- Per-method proofs (21 theorems)
theorem Method.get_is_safe     : (.standard .GET).isSafe = true      := by rfl
theorem Method.post_not_safe   : (.standard .POST).isSafe = false    := by rfl
theorem Method.put_is_idempotent  : (.standard .PUT).isIdempotent = true  := by rfl
theorem Method.post_not_idempotent : (.standard .POST).isIdempotent = false := by rfl
-- ... (17 more)

-- The universal property: safe ⟹ idempotent (RFC 9110 §9.2.2)
theorem Method.safe_implies_idempotent (m : Method) (h : m.isSafe = true) :
    m.isIdempotent = true := by
  match m with
  | .standard .GET     => rfl
  | .standard .HEAD    => rfl
  | .standard .OPTIONS => rfl
  | .standard .TRACE   => rfl
  | .standard .PUT     => simp [Method.isSafe] at h
  | .standard .POST    => simp [Method.isSafe] at h
  -- ... (exhaustive case analysis, contradictions resolved by simp)
```

This is not a test that runs on four inputs. It is a **proof for all
methods** — including custom ones. The `| .custom _ =>` case must also
be handled, and it is: `isSafe` returns `false` for custom methods, so
the hypothesis `h` is contradictory and `simp` discharges it.

---

## 2. Status code bodies (RFC 9110 §6.4.1)

The RFC says: 1xx, 204, and 304 responses **MUST NOT** contain a body.
We encode this as a predicate and prove it for every relevant status code:

```lean
def Status.mustNotHaveBody (s : Status) : Bool :=
  s.statusCode / 100 == 1 || s.statusCode == 204 || s.statusCode == 304

-- Proven for each status code:
theorem status100_no_body : status100.mustNotHaveBody = true  := by native_decide
theorem status101_no_body : status101.mustNotHaveBody = true  := by native_decide
theorem status204_no_body : status204.mustNotHaveBody = true  := by native_decide
theorem status304_no_body : status304.mustNotHaveBody = true  := by native_decide
theorem status200_may_have_body : status200.mustNotHaveBody = false := by native_decide
theorem status404_may_have_body : status404.mustNotHaveBody = false := by native_decide
```

If someone adds a new status code to the library, they can immediately
check whether it may carry a body — the kernel evaluates the predicate
at compile time.

---

## 3. Exactly-once response (the indexed monad)

In Haskell's WAI, the rule "call `respond` exactly once per request" is
a comment. In Hale, it is an **indexed monad** — a type-level state
machine that the Lean 4 kernel verifies:

```lean
inductive ResponseState where
  | pending    -- no response sent yet
  | sent       -- response has been sent

structure AppM (pre post : ResponseState) (α : Type) where
  private mk ::      -- private: cannot fabricate
  run : IO α

-- The ONLY way to go from .pending to .sent:
def AppM.respond (callback : Response → IO ResponseReceived) (resp : Response)
    : AppM .pending .sent ResponseReceived := ⟨callback resp⟩

-- Every Application must produce AppM .pending .sent:
abbrev Application :=
  Request → (Response → IO ResponseReceived) → AppM .pending .sent ResponseReceived
```

**Why double-respond is a type error:**
```lean
-- After the first respond, the state is .sent.
-- AppM.ibind chains:  AppM s₁ s₂ α → (α → AppM s₂ s₃ β) → AppM s₁ s₃ β
-- A second respond needs pre-state .pending, but we are in .sent.

def doubleRespond (respond : Response → IO ResponseReceived)
    : AppM .pending .sent ResponseReceived :=
  AppM.ibind (AppM.respond respond resp1) fun _ =>
    AppM.respond respond resp2
    -- ❌ Type error: expected AppM .sent _ _, got AppM .pending .sent _
```

**Why skip-respond is a type error:**
```lean
def skipRespond : AppM .pending .sent ResponseReceived :=
  AppM.ipure ResponseReceived.done
  -- ❌ Type error: AppM.ipure has type AppM s s α, so this is
  --    AppM .pending .pending ResponseReceived — not .pending .sent
```

The `private mk` seals the guarantee: you cannot construct
`AppM .pending .sent` without actually calling `respond`.

---

## 4. Transport security (proven at the type level)

Warp supports TCP, TLS, and QUIC. The type system encodes which
transports are secure:

```lean
inductive Transport where
  | tcp
  | tls (major minor : Nat) (alpn : Option String) (cipher : UInt16)
  | quic (alpn : Option String) (cipher : UInt16)

def Transport.isSecure : Transport → Bool
  | .tcp      => false
  | .tls ..   => true
  | .quic ..  => true

-- Proven:
theorem tcp_not_secure : Transport.isSecure .tcp = false := rfl
theorem tls_is_secure (v1 v2 p c) : Transport.isSecure (.tls v1 v2 p c) = true := rfl
theorem quic_is_secure (p c)      : Transport.isSecure (.quic p c) = true := rfl
```

These are trivial — `rfl` suffices because the kernel evaluates the
function on the concrete constructor. But having them as named theorems
means downstream code can prove, say, "if we're on TLS, the connection
is secure" without re-deriving the fact.

---

## 5. Keep-alive semantics (RFC 9112)

HTTP/1.0 defaults to close; HTTP/1.1 defaults to keep-alive. This is
specified in RFC 9112 §9.3 and proven in Hale:

```lean
theorem connAction_http10_default (req : Request)
    (hVer : (req.httpVersion == http11) = false)
    (hNoConn : req.requestHeaders.find? (...) = none) :
    connAction req = .close := by
  unfold connAction; simp [hVer, hNoConn]

theorem connAction_http11_default (req : Request)
    (hVer : (req.httpVersion == http11) = true)
    (hNoConn : req.requestHeaders.find? (...) = none) :
    connAction req = .keepAlive := by
  unfold connAction; simp [hVer, hNoConn]
```

---

## 6. Wire-format bijectivity (roundtrip theorems)

When encoding frame types, error codes, or opcodes to bytes and back,
information must not be lost. We prove this for every codec:

### HTTP/2 frame types (RFC 9113 §6)
```lean
-- 10 roundtrip theorems:
theorem FrameType.roundtrip_data    : fromUInt8 (toUInt8 .data) = .data       := by rfl
theorem FrameType.roundtrip_headers : fromUInt8 (toUInt8 .headers) = .headers := by rfl
-- ... (8 more: priority, rstStream, settings, pushPromise, ping, goaway, windowUpdate, continuation)
```

### HTTP/3 error codes (RFC 9114)
```lean
-- 17 roundtrip theorems:
theorem roundtrip_noError : fromUInt64 (toUInt64 .noError) = .noError := by rfl
theorem roundtrip_generalProtocolError : fromUInt64 (toUInt64 .generalProtocolError) = ... := by rfl
-- ... (15 more)
```

### WebSocket opcodes (RFC 6455 §5.2)
```lean
-- 6 roundtrip theorems:
theorem roundtrip_text  : Opcode.fromUInt8 (Opcode.toUInt8 .text) = .text   := by rfl
theorem roundtrip_close : Opcode.fromUInt8 (Opcode.toUInt8 .close) = .close := by rfl
-- ...
```

These ensure that every frame/error/opcode survives a serialize → deserialize
cycle unchanged. The proof is by `rfl` — the kernel evaluates both sides and
confirms they are definitionally equal.

---

## 7. HTTP/2 stream lifecycle (RFC 9113 §5.1)

The HTTP/2 stream state machine is encoded as an inductive type:

```lean
inductive StreamState where
  | idle            -- not yet opened
  | open            -- HEADERS sent/received
  | halfClosedLocal -- local END_STREAM sent
  | halfClosedRemote -- remote END_STREAM received
  | resetLocal      -- RST_STREAM sent
  | resetRemote     -- RST_STREAM received
  | closed          -- terminal
  | reservedLocal   -- PUSH_PROMISE received
  | reservedRemote  -- PUSH_PROMISE sent
```

Nine states, directly from RFC 9113 §5.1 Figure 2. The exhaustive pattern
matching ensures every state transition is handled in every function that
operates on streams.

---

## The total count

| Category | Theorems | RFC/Standard |
|----------|----------|-------------|
| Method semantics | 21 | RFC 9110 §9.2 |
| Status code properties | 13 | RFC 9110 §6.4.1, §15 |
| Status code ranges | 5 | RFC 9110 §15 |
| HTTP version parsing | 4 | RFC 9110 |
| Method parsing roundtrips | 10 | RFC 9110 |
| Transport security | 3 | TLS 1.2+/QUIC |
| Keep-alive semantics | 2 | RFC 9112 §9.3 |
| Response exactly-once | (type-level) | WAI contract |
| H2 frame roundtrips | 10 | RFC 9113 §6 |
| H3 frame roundtrips | 7 | RFC 9114 |
| H3 error roundtrips | 17 | RFC 9114 |
| WS opcode roundtrips | 6 | RFC 6455 §5.2 |
| Middleware algebra | 5 | WAI composition |
| Response accessor laws | 11 | WAI internal |
| **Total** | **~114** | |

Every one of these is verified by the Lean 4 kernel at compile time.
They cannot be wrong (modulo axioms, which are documented). They cannot
go stale — if someone changes a definition, the proof breaks and the
build fails.

---

*This post is part of the [Hale](../) documentation — a port of
Haskell's web ecosystem to Lean 4 with maximalist typing.*
