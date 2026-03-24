/-
  Hale.Http2.Network.HTTP2.Frame.Types — HTTP/2 frame types

  Core types for HTTP/2 framing as defined in RFC 9113.

  ## Design

  Frame types, error codes, and settings are encoded as inductives with
  exhaustive pattern matching rather than raw numeric constants. Conversions
  to/from `UInt8`/`UInt32` are provided as total functions.

  ## Guarantees

  - `FrameType` is a closed inductive covering all RFC 9113 frame types
  - `ErrorCode` covers all defined error codes
  - `SettingsKeyId` covers all defined settings identifiers
  - Numeric conversions are provably inverse for defined values
  - `Settings` carries proof fields enforcing RFC 9113 value constraints

  ## Haskell equivalent
  `Network.HTTP2.Frame.Types` (https://hackage.haskell.org/package/http2)
-/

namespace Network.HTTP2

/-- HTTP/2 stream identifier. Stream 0 is the connection control stream.
    $$\text{StreamId} = \mathbb{N}_{31}$$ (31-bit unsigned, per RFC 9113 Section 4.1) -/
abbrev StreamId := UInt32

/-- HTTP/2 frame types as defined in RFC 9113 Section 6.
    Each variant corresponds to a specific frame type byte. -/
inductive FrameType where
  | data          -- 0x0
  | headers       -- 0x1
  | priority      -- 0x2
  | rstStream     -- 0x3
  | settings      -- 0x4
  | pushPromise   -- 0x5
  | ping          -- 0x6
  | goaway        -- 0x7
  | windowUpdate  -- 0x8
  | continuation  -- 0x9
  | unknown (id : UInt8) -- For forward compatibility
  deriving Repr, Inhabited

instance : BEq FrameType where
  beq
    | .data, .data => true
    | .headers, .headers => true
    | .priority, .priority => true
    | .rstStream, .rstStream => true
    | .settings, .settings => true
    | .pushPromise, .pushPromise => true
    | .ping, .ping => true
    | .goaway, .goaway => true
    | .windowUpdate, .windowUpdate => true
    | .continuation, .continuation => true
    | .unknown a, .unknown b => a == b
    | _, _ => false

instance : ToString FrameType where
  toString
    | .data => "DATA"
    | .headers => "HEADERS"
    | .priority => "PRIORITY"
    | .rstStream => "RST_STREAM"
    | .settings => "SETTINGS"
    | .pushPromise => "PUSH_PROMISE"
    | .ping => "PING"
    | .goaway => "GOAWAY"
    | .windowUpdate => "WINDOW_UPDATE"
    | .continuation => "CONTINUATION"
    | .unknown id => s!"UNKNOWN({id})"

namespace FrameType

/-- Convert a frame type to its wire byte.
    $$\text{toUInt8} : \text{FrameType} \to \text{UInt8}$$ -/
def toUInt8 : FrameType → UInt8
  | .data => 0
  | .headers => 1
  | .priority => 2
  | .rstStream => 3
  | .settings => 4
  | .pushPromise => 5
  | .ping => 6
  | .goaway => 7
  | .windowUpdate => 8
  | .continuation => 9
  | .unknown id => id

/-- Parse a frame type from its wire byte.
    $$\text{fromUInt8} : \text{UInt8} \to \text{FrameType}$$ -/
def fromUInt8 : UInt8 → FrameType
  | 0 => .data
  | 1 => .headers
  | 2 => .priority
  | 3 => .rstStream
  | 4 => .settings
  | 5 => .pushPromise
  | 6 => .ping
  | 7 => .goaway
  | 8 => .windowUpdate
  | 9 => .continuation
  | id => .unknown id

/-- Roundtrip: `fromUInt8 (toUInt8 ft) = ft` for known frame types. -/
theorem fromUInt8_toUInt8_data : fromUInt8 (toUInt8 .data) = .data := by rfl
theorem fromUInt8_toUInt8_headers : fromUInt8 (toUInt8 .headers) = .headers := by rfl
theorem fromUInt8_toUInt8_priority : fromUInt8 (toUInt8 .priority) = .priority := by rfl
theorem fromUInt8_toUInt8_rstStream : fromUInt8 (toUInt8 .rstStream) = .rstStream := by rfl
theorem fromUInt8_toUInt8_settings : fromUInt8 (toUInt8 .settings) = .settings := by rfl
theorem fromUInt8_toUInt8_pushPromise : fromUInt8 (toUInt8 .pushPromise) = .pushPromise := by rfl
theorem fromUInt8_toUInt8_ping : fromUInt8 (toUInt8 .ping) = .ping := by rfl
theorem fromUInt8_toUInt8_goaway : fromUInt8 (toUInt8 .goaway) = .goaway := by rfl
theorem fromUInt8_toUInt8_windowUpdate : fromUInt8 (toUInt8 .windowUpdate) = .windowUpdate := by rfl
theorem fromUInt8_toUInt8_continuation : fromUInt8 (toUInt8 .continuation) = .continuation := by rfl

/-- Roundtrip for the `unknown` variant: encoding then decoding recovers `.unknown id`,
    provided `id` does not collide with a known frame type byte (0–9).
    The theorem is false without this precondition: e.g. `fromUInt8 (toUInt8 (.unknown 0))
    = fromUInt8 0 = .data ≠ .unknown 0`. -/
theorem fromUInt8_toUInt8_unknown (id : UInt8) (h : id.toNat ≥ 10) :
    FrameType.fromUInt8 (FrameType.toUInt8 (.unknown id)) = .unknown id := by
  simp only [toUInt8]
  unfold fromUInt8
  split <;> simp_all

end FrameType

/-- HTTP/2 error codes as defined in RFC 9113 Section 7.
    Used in RST_STREAM and GOAWAY frames. -/
inductive ErrorCode where
  | noError            -- 0x0
  | protocolError      -- 0x1
  | internalError      -- 0x2
  | flowControlError   -- 0x3
  | settingsTimeout    -- 0x4
  | streamClosed       -- 0x5
  | frameSizeError     -- 0x6
  | refusedStream      -- 0x7
  | cancel             -- 0x8
  | compressionError   -- 0x9
  | connectError       -- 0xa
  | enhanceYourCalm    -- 0xb
  | inadequateSecurity -- 0xc
  | http11Required     -- 0xd
  | unknown (code : UInt32)
  deriving Repr, Inhabited

instance : BEq ErrorCode where
  beq
    | .noError, .noError => true
    | .protocolError, .protocolError => true
    | .internalError, .internalError => true
    | .flowControlError, .flowControlError => true
    | .settingsTimeout, .settingsTimeout => true
    | .streamClosed, .streamClosed => true
    | .frameSizeError, .frameSizeError => true
    | .refusedStream, .refusedStream => true
    | .cancel, .cancel => true
    | .compressionError, .compressionError => true
    | .connectError, .connectError => true
    | .enhanceYourCalm, .enhanceYourCalm => true
    | .inadequateSecurity, .inadequateSecurity => true
    | .http11Required, .http11Required => true
    | .unknown a, .unknown b => a == b
    | _, _ => false

instance : ToString ErrorCode where
  toString
    | .noError => "NO_ERROR"
    | .protocolError => "PROTOCOL_ERROR"
    | .internalError => "INTERNAL_ERROR"
    | .flowControlError => "FLOW_CONTROL_ERROR"
    | .settingsTimeout => "SETTINGS_TIMEOUT"
    | .streamClosed => "STREAM_CLOSED"
    | .frameSizeError => "FRAME_SIZE_ERROR"
    | .refusedStream => "REFUSED_STREAM"
    | .cancel => "CANCEL"
    | .compressionError => "COMPRESSION_ERROR"
    | .connectError => "CONNECT_ERROR"
    | .enhanceYourCalm => "ENHANCE_YOUR_CALM"
    | .inadequateSecurity => "INADEQUATE_SECURITY"
    | .http11Required => "HTTP_1_1_REQUIRED"
    | .unknown code => s!"UNKNOWN({code})"

namespace ErrorCode

/-- Convert an error code to its wire representation.
    $$\text{toUInt32} : \text{ErrorCode} \to \text{UInt32}$$ -/
def toUInt32 : ErrorCode → UInt32
  | .noError => 0
  | .protocolError => 1
  | .internalError => 2
  | .flowControlError => 3
  | .settingsTimeout => 4
  | .streamClosed => 5
  | .frameSizeError => 6
  | .refusedStream => 7
  | .cancel => 8
  | .compressionError => 9
  | .connectError => 10
  | .enhanceYourCalm => 11
  | .inadequateSecurity => 12
  | .http11Required => 13
  | .unknown code => code

/-- Parse an error code from its wire representation.
    $$\text{fromUInt32} : \text{UInt32} \to \text{ErrorCode}$$ -/
def fromUInt32 : UInt32 → ErrorCode
  | 0 => .noError
  | 1 => .protocolError
  | 2 => .internalError
  | 3 => .flowControlError
  | 4 => .settingsTimeout
  | 5 => .streamClosed
  | 6 => .frameSizeError
  | 7 => .refusedStream
  | 8 => .cancel
  | 9 => .compressionError
  | 10 => .connectError
  | 11 => .enhanceYourCalm
  | 12 => .inadequateSecurity
  | 13 => .http11Required
  | code => .unknown code

/-- Roundtrip for the `unknown` variant: encoding then decoding recovers `.unknown code`,
    provided `code` does not collide with a known error code (0–13).
    The theorem is false without this precondition: e.g. `fromUInt32 (toUInt32 (.unknown 0))
    = fromUInt32 0 = .noError ≠ .unknown 0`. -/
theorem fromUInt32_toUInt32_unknown (code : UInt32) (h : code.toNat ≥ 14) :
    ErrorCode.fromUInt32 (ErrorCode.toUInt32 (.unknown code)) = .unknown code := by
  simp only [toUInt32]
  unfold fromUInt32
  split <;> simp_all

end ErrorCode

/-- HTTP/2 settings identifiers as defined in RFC 9113 Section 6.5.2. -/
inductive SettingsKeyId where
  | headerTableSize      -- 0x1
  | enablePush           -- 0x2
  | maxConcurrentStreams  -- 0x3
  | initialWindowSize    -- 0x4
  | maxFrameSize         -- 0x5
  | maxHeaderListSize    -- 0x6
  | unknown (id : UInt16)
  deriving Repr, Inhabited

instance : BEq SettingsKeyId where
  beq
    | .headerTableSize, .headerTableSize => true
    | .enablePush, .enablePush => true
    | .maxConcurrentStreams, .maxConcurrentStreams => true
    | .initialWindowSize, .initialWindowSize => true
    | .maxFrameSize, .maxFrameSize => true
    | .maxHeaderListSize, .maxHeaderListSize => true
    | .unknown a, .unknown b => a == b
    | _, _ => false

instance : ToString SettingsKeyId where
  toString
    | .headerTableSize => "HEADER_TABLE_SIZE"
    | .enablePush => "ENABLE_PUSH"
    | .maxConcurrentStreams => "MAX_CONCURRENT_STREAMS"
    | .initialWindowSize => "INITIAL_WINDOW_SIZE"
    | .maxFrameSize => "MAX_FRAME_SIZE"
    | .maxHeaderListSize => "MAX_HEADER_LIST_SIZE"
    | .unknown id => s!"UNKNOWN({id})"

namespace SettingsKeyId

/-- Convert a settings key to its wire representation.
    $$\text{toUInt16} : \text{SettingsKeyId} \to \text{UInt16}$$ -/
def toUInt16 : SettingsKeyId → UInt16
  | .headerTableSize => 1
  | .enablePush => 2
  | .maxConcurrentStreams => 3
  | .initialWindowSize => 4
  | .maxFrameSize => 5
  | .maxHeaderListSize => 6
  | .unknown id => id

/-- Parse a settings key from its wire representation.
    $$\text{fromUInt16} : \text{UInt16} \to \text{SettingsKeyId}$$ -/
def fromUInt16 : UInt16 → SettingsKeyId
  | 1 => .headerTableSize
  | 2 => .enablePush
  | 3 => .maxConcurrentStreams
  | 4 => .initialWindowSize
  | 5 => .maxFrameSize
  | 6 => .maxHeaderListSize
  | id => .unknown id

end SettingsKeyId

/-- HTTP/2 connection settings with their default values per RFC 9113 Section 6.5.2.

    $$\text{Settings} = \{
      \text{headerTableSize} : \mathbb{N},
      \text{enablePush} : \text{Bool},
      \text{maxConcurrentStreams} : \text{Option}(\mathbb{N}),
      \text{initialWindowSize} : \text{UInt32},
      \text{maxFrameSize} : \text{UInt32},
      \text{maxHeaderListSize} : \text{Option}(\mathbb{N})
    \}$$

    ## Dependent-type guarantees

    RFC 9113 Section 6.5.2 constrains certain settings values. These constraints
    are encoded as proof fields that are erased at runtime (zero-cost):

    - **`initialWindowSize`** must be at most $2^{31} - 1$ (`initialWindowSize_valid`)
    - **`maxFrameSize`** must be in $[2^{14},\; 2^{24} - 1]$ (`maxFrameSize_lower`, `maxFrameSize_upper`)

    The default values (65535 and 16384) automatically satisfy these constraints
    via `by native_decide`. Attempting to construct a `Settings` with out-of-range values
    is a type error — the caller must supply a proof or use a validated constructor. -/
structure Settings where
  /-- Maximum size of the header compression table (SETTINGS_HEADER_TABLE_SIZE).
      Default: 4096 octets. -/
  headerTableSize : Nat := 4096
  /-- Whether push is enabled (SETTINGS_ENABLE_PUSH). Default: true. -/
  enablePush : Bool := true
  /-- Maximum number of concurrent streams (SETTINGS_MAX_CONCURRENT_STREAMS).
      Default: unlimited (none). -/
  maxConcurrentStreams : Option Nat := none
  /-- Initial window size for stream-level flow control (SETTINGS_INITIAL_WINDOW_SIZE).
      Default: 65535 (2^16 - 1). RFC 9113 requires <= 2^31 - 1. -/
  initialWindowSize : UInt32 := 65535
  /-- Maximum frame payload size (SETTINGS_MAX_FRAME_SIZE).
      Default: 16384 (2^14). RFC 9113 requires value in [2^14, 2^24 - 1]. -/
  maxFrameSize : UInt32 := 16384
  /-- Maximum size of header list (SETTINGS_MAX_HEADER_LIST_SIZE).
      Default: unlimited (none). -/
  maxHeaderListSize : Option Nat := none
  /-- Proof: `initialWindowSize` is at most $2^{31} - 1$ (RFC 9113 Section 6.5.2). -/
  initialWindowSize_valid : initialWindowSize.toNat ≤ 2147483647 := by native_decide
  /-- Proof: `maxFrameSize` is at least $2^{14}$ (RFC 9113 Section 6.5.2). -/
  maxFrameSize_lower : 16384 ≤ maxFrameSize.toNat := by native_decide
  /-- Proof: `maxFrameSize` is at most $2^{24} - 1$ (RFC 9113 Section 6.5.2). -/
  maxFrameSize_upper : maxFrameSize.toNat ≤ 16777215 := by native_decide

instance : Repr Settings where
  reprPrec s _ :=
    "{ headerTableSize := " ++ repr s.headerTableSize ++
    ", enablePush := " ++ repr s.enablePush ++
    ", maxConcurrentStreams := " ++ repr s.maxConcurrentStreams ++
    ", initialWindowSize := " ++ repr s.initialWindowSize ++
    ", maxFrameSize := " ++ repr s.maxFrameSize ++
    ", maxHeaderListSize := " ++ repr s.maxHeaderListSize ++ " }"

instance : Inhabited Settings where
  default := {}

instance : BEq Settings where
  beq a b :=
    a.headerTableSize == b.headerTableSize &&
    a.enablePush == b.enablePush &&
    a.maxConcurrentStreams == b.maxConcurrentStreams &&
    a.initialWindowSize == b.initialWindowSize &&
    a.maxFrameSize == b.maxFrameSize &&
    a.maxHeaderListSize == b.maxHeaderListSize

/-- Default settings per RFC 9113 Section 6.5.2. -/
def Settings.default : Settings := {}

/-- HTTP/2 frame flags. Stored as a raw byte for extensibility.
    $$\text{FrameFlags} = \text{UInt8}$$ -/
abbrev FrameFlags := UInt8

namespace FrameFlags

/-- No flags set. -/
def none : FrameFlags := 0

/-- END_STREAM flag (0x1). Valid on DATA and HEADERS frames. -/
def endStream : FrameFlags := 0x1

/-- ACK flag (0x1). Valid on SETTINGS and PING frames. -/
def ack : FrameFlags := 0x1

/-- END_HEADERS flag (0x4). Valid on HEADERS, PUSH_PROMISE, and CONTINUATION frames. -/
def endHeaders : FrameFlags := 0x4

/-- PADDED flag (0x8). Valid on DATA, HEADERS, and PUSH_PROMISE frames. -/
def padded : FrameFlags := 0x8

/-- PRIORITY flag (0x20). Valid on HEADERS frames. -/
def priority : FrameFlags := 0x20

/-- Test if a flag is set.
    $$\text{test}(flags, flag) = (flags \mathbin{\&} flag) \neq 0$$ -/
@[inline] def test (flags flag : FrameFlags) : Bool :=
  (flags &&& flag) != 0

/-- Set a flag.
    $$\text{set}(flags, flag) = flags \mathbin{|} flag$$ -/
@[inline] def set (flags flag : FrameFlags) : FrameFlags :=
  flags ||| flag

/-- Clear a flag.
    $$\text{clear}(flags, flag) = flags \mathbin{\&} (\sim flag)$$ -/
@[inline] def clear (flags flag : FrameFlags) : FrameFlags :=
  flags &&& (~~~ flag)

end FrameFlags

/-- HTTP/2 frame header (9 bytes on the wire).
    $$\text{FrameHeader} = \{
      \text{length} : \mathbb{N}_{24},
      \text{frameType} : \text{FrameType},
      \text{flags} : \text{FrameFlags},
      \text{streamId} : \text{StreamId}
    \}$$

    Wire format: `[Length (24)] [Type (8)] [Flags (8)] [R (1)] [Stream ID (31)]` -/
structure FrameHeader where
  /-- Payload length in bytes (24-bit unsigned). -/
  payloadLength : UInt32
  /-- Frame type. -/
  frameType : FrameType
  /-- Flags byte. -/
  flags : FrameFlags
  /-- Stream identifier (31-bit, high bit reserved). -/
  streamId : StreamId
  deriving Repr, Inhabited

instance : BEq FrameHeader where
  beq a b :=
    a.payloadLength == b.payloadLength &&
    a.frameType == b.frameType &&
    a.flags == b.flags &&
    a.streamId == b.streamId

instance : ToString FrameHeader where
  toString h :=
    s!"FrameHeader(type={h.frameType}, length={h.payloadLength}, flags=0x{String.ofList (h.flags.toNat.toDigits 16)}, stream={h.streamId})"

/-- An HTTP/2 frame consisting of a header and payload.
    $$\text{Frame} = \text{FrameHeader} \times \text{ByteArray}$$ -/
structure Frame where
  /-- Frame header. -/
  header : FrameHeader
  /-- Frame payload. -/
  payload : ByteArray
  deriving Inhabited

instance : BEq Frame where
  beq a b := a.header == b.header && a.payload == b.payload

/-- The HTTP/2 connection preface magic bytes (PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n).
    Must be sent by the client as the first 24 bytes. -/
def connectionPreface : ByteArray :=
  -- "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"
  let s := "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"
  s.toUTF8

/-- Length of the connection preface in bytes. Always 24. -/
def connectionPrefaceLength : Nat := 24

/-- Frame header size in bytes. Always 9. -/
def frameHeaderSize : Nat := 9

/-- Default initial window size (2^16 - 1 = 65535). -/
def defaultInitialWindowSize : UInt32 := 65535

/-- Maximum window size (2^31 - 1). -/
def maxWindowSize : UInt32 := 2147483647

/-- Minimum allowed max frame size (2^14 = 16384). -/
def minMaxFrameSize : UInt32 := 16384

/-- Maximum allowed max frame size (2^24 - 1 = 16777215). -/
def maxMaxFrameSize : UInt32 := 16777215

end Network.HTTP2
