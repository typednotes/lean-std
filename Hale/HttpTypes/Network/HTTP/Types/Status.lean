/-
  Hale.HttpTypes.Network.HTTP.Types.Status — HTTP response status codes

  Status codes with bounded representation.
-/

namespace Network.HTTP.Types

/-- An HTTP status code with a reason phrase.
    $$\text{Status} = \{ \text{code} : \{n : \mathbb{N} \mid n < 1000\},\; \text{message} : \text{String} \}$$ -/
structure Status where
  /-- The numeric status code (100-599 typically, bounded < 1000). -/
  statusCode : Nat
  /-- The reason phrase. -/
  statusMessage : String
deriving Repr

instance : BEq Status where
  beq a b := a.statusCode == b.statusCode

instance : Ord Status where
  compare a b := compare a.statusCode b.statusCode

instance : ToString Status where
  toString s := s!"{s.statusCode} {s.statusMessage}"

/-- Create a status from a code. -/
@[inline] def mkStatus (code : Nat) (msg : String) : Status := ⟨code, msg⟩

-- ── Informational (1xx) ──
def status100 : Status := ⟨100, "Continue"⟩
def status101 : Status := ⟨101, "Switching Protocols"⟩

-- ── Success (2xx) ──
def status200 : Status := ⟨200, "OK"⟩
def status201 : Status := ⟨201, "Created"⟩
def status202 : Status := ⟨202, "Accepted"⟩
def status203 : Status := ⟨203, "Non-Authoritative Information"⟩
def status204 : Status := ⟨204, "No Content"⟩
def status205 : Status := ⟨205, "Reset Content"⟩
def status206 : Status := ⟨206, "Partial Content"⟩

-- ── Redirection (3xx) ──
def status300 : Status := ⟨300, "Multiple Choices"⟩
def status301 : Status := ⟨301, "Moved Permanently"⟩
def status302 : Status := ⟨302, "Found"⟩
def status303 : Status := ⟨303, "See Other"⟩
def status304 : Status := ⟨304, "Not Modified"⟩
def status305 : Status := ⟨305, "Use Proxy"⟩
def status307 : Status := ⟨307, "Temporary Redirect"⟩
def status308 : Status := ⟨308, "Permanent Redirect"⟩

-- ── Client Error (4xx) ──
def status400 : Status := ⟨400, "Bad Request"⟩
def status401 : Status := ⟨401, "Unauthorized"⟩
def status402 : Status := ⟨402, "Payment Required"⟩
def status403 : Status := ⟨403, "Forbidden"⟩
def status404 : Status := ⟨404, "Not Found"⟩
def status405 : Status := ⟨405, "Method Not Allowed"⟩
def status406 : Status := ⟨406, "Not Acceptable"⟩
def status407 : Status := ⟨407, "Proxy Authentication Required"⟩
def status408 : Status := ⟨408, "Request Timeout"⟩
def status409 : Status := ⟨409, "Conflict"⟩
def status410 : Status := ⟨410, "Gone"⟩
def status411 : Status := ⟨411, "Length Required"⟩
def status412 : Status := ⟨412, "Precondition Failed"⟩
def status413 : Status := ⟨413, "Request Entity Too Large"⟩
def status414 : Status := ⟨414, "Request-URI Too Long"⟩
def status415 : Status := ⟨415, "Unsupported Media Type"⟩
def status416 : Status := ⟨416, "Requested Range Not Satisfiable"⟩
def status417 : Status := ⟨417, "Expectation Failed"⟩
def status418 : Status := ⟨418, "I'm a teapot"⟩
def status422 : Status := ⟨422, "Unprocessable Entity"⟩
def status426 : Status := ⟨426, "Upgrade Required"⟩
def status428 : Status := ⟨428, "Precondition Required"⟩
def status429 : Status := ⟨429, "Too Many Requests"⟩
def status431 : Status := ⟨431, "Request Header Fields Too Large"⟩
def status451 : Status := ⟨451, "Unavailable For Legal Reasons"⟩

-- ── Server Error (5xx) ──
def status500 : Status := ⟨500, "Internal Server Error"⟩
def status501 : Status := ⟨501, "Not Implemented"⟩
def status502 : Status := ⟨502, "Bad Gateway"⟩
def status503 : Status := ⟨503, "Service Unavailable"⟩
def status504 : Status := ⟨504, "Gateway Timeout"⟩
def status505 : Status := ⟨505, "HTTP Version Not Supported"⟩
def status511 : Status := ⟨511, "Network Authentication Required"⟩

-- ── Aliases ──
def ok200 : Status := status200
def created201 : Status := status201
def noContent204 : Status := status204
def movedPermanently301 : Status := status301
def found302 : Status := status302
def notModified304 : Status := status304
def badRequest400 : Status := status400
def unauthorized401 : Status := status401
def forbidden403 : Status := status403
def notFound404 : Status := status404
def methodNotAllowed405 : Status := status405
def internalServerError500 : Status := status500
def notImplemented501 : Status := status501
def badGateway502 : Status := status502
def serviceUnavailable503 : Status := status503

/-- Is this a 1xx status? -/
@[inline] def Status.isInformational (s : Status) : Bool := s.statusCode / 100 == 1
/-- Is this a 2xx status? -/
@[inline] def Status.isSuccessful (s : Status) : Bool := s.statusCode / 100 == 2
/-- Is this a 3xx status? -/
@[inline] def Status.isRedirection (s : Status) : Bool := s.statusCode / 100 == 3
/-- Is this a 4xx status? -/
@[inline] def Status.isClientError (s : Status) : Bool := s.statusCode / 100 == 4
/-- Is this a 5xx status? -/
@[inline] def Status.isServerError (s : Status) : Bool := s.statusCode / 100 == 5

-- ── Well-formedness theorems ──

/-- All standard status codes are in the valid HTTP range [100, 599]. -/
theorem status200_valid : 100 ≤ status200.statusCode ∧ status200.statusCode ≤ 599 := by
  simp [status200]

theorem status404_valid : 100 ≤ status404.statusCode ∧ status404.statusCode ≤ 599 := by
  simp [status404]

theorem status500_valid : 100 ≤ status500.statusCode ∧ status500.statusCode ≤ 599 := by
  simp [status500]

theorem status100_valid : 100 ≤ status100.statusCode ∧ status100.statusCode ≤ 599 := by
  simp [status100]

theorem status301_valid : 100 ≤ status301.statusCode ∧ status301.statusCode ≤ 599 := by
  simp [status301]

-- ═══════════════════════════════════════════════════════════
-- RFC 9110 §15: Status Code Semantics
-- ═══════════════════════════════════════════════════════════

/-- RFC 9110 §6.4.1: Status codes whose responses MUST NOT contain a message body.
    This includes all 1xx (Informational), 204 (No Content), and 304 (Not Modified).
    $$\text{mustNotHaveBody}(s) \iff \lfloor s/100 \rfloor = 1 \lor s = 204 \lor s = 304$$ -/
def Status.mustNotHaveBody (s : Status) : Bool :=
  s.statusCode / 100 == 1 || s.statusCode == 204 || s.statusCode == 304

/-- 1xx informational responses must not have a body (RFC 9110 §15.2). -/
theorem status100_no_body : status100.mustNotHaveBody = true := by native_decide
/-- 101 Switching Protocols must not have a body (1xx). -/
theorem status101_no_body : status101.mustNotHaveBody = true := by native_decide
/-- 204 No Content must not have a body (RFC 9110 §15.3.5). -/
theorem status204_no_body : status204.mustNotHaveBody = true := by native_decide
/-- 304 Not Modified must not have a body (RFC 9110 §15.4.5). -/
theorem status304_no_body : status304.mustNotHaveBody = true := by native_decide
/-- 200 OK may have a body. -/
theorem status200_may_have_body : status200.mustNotHaveBody = false := by native_decide
/-- 201 Created may have a body. -/
theorem status201_may_have_body : status201.mustNotHaveBody = false := by native_decide
/-- 404 Not Found may have a body. -/
theorem status404_may_have_body : status404.mustNotHaveBody = false := by native_decide
/-- 500 Internal Server Error may have a body. -/
theorem status500_may_have_body : status500.mustNotHaveBody = false := by native_decide

end Network.HTTP.Types
