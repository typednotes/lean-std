/-
  Hale.Http2.Network.HTTP2.Frame.Decode — HTTP/2 frame decoding

  Parses HTTP/2 frames from wire format as defined in RFC 9113 Section 4.

  ## Design

  Decoding functions return `Option` on parse failure rather than panicking.
  All multi-byte integers are read in big-endian (network) byte order.

  ## Guarantees

  - `decodeFrameHeader` requires exactly 9 bytes of input
  - `decodeUInt32BE` requires at least `offset + 4` bytes
  - `decodePriority` requires at least `offset + 5` bytes
  - `decodePadding` validates pad length against payload size
  - `validateFrameSize` checks RFC 9113 frame size constraints

  ## Haskell equivalent
  `Network.HTTP2.Frame.Decode` (https://hackage.haskell.org/package/http2)
-/
import Hale.Http2.Network.HTTP2.Frame.Types

namespace Network.HTTP2

-- ── Wire decoding helpers ──────────────────────────────

/-- Decode a 16-bit unsigned integer from big-endian bytes at the given offset.
    $$\text{decodeUInt16BE}(bs, i) = bs[i] \times 256 + bs[i+1]$$
    Returns `none` if there are fewer than `offset + 2` bytes. -/
def decodeUInt16BE (bs : ByteArray) (offset : Nat := 0) : Option UInt16 :=
  if offset + 2 > bs.size then none
  else
    let b0 := bs[offset]!
    let b1 := bs[offset + 1]!
    some ((b0.toUInt16 <<< 8) ||| b1.toUInt16)

/-- Decode a 32-bit unsigned integer from big-endian bytes at the given offset.
    $$\text{decodeUInt32BE}(bs, i) = \sum_{k=0}^{3} bs[i+k] \times 256^{3-k}$$
    Returns `none` if there are fewer than `offset + 4` bytes. -/
def decodeUInt32BE (bs : ByteArray) (offset : Nat := 0) : Option UInt32 :=
  if offset + 4 > bs.size then none
  else
    let b0 := bs[offset]!
    let b1 := bs[offset + 1]!
    let b2 := bs[offset + 2]!
    let b3 := bs[offset + 3]!
    some ((b0.toUInt32 <<< 24) ||| (b1.toUInt32 <<< 16) ||| (b2.toUInt32 <<< 8) ||| b3.toUInt32)

-- ── Frame header decoding ──────────────────────────────

/-- Decode an HTTP/2 frame header from its 9-byte wire format.

    Wire format: `[Length (24)] [Type (8)] [Flags (8)] [R (1)] [Stream ID (31)]`

    $$\text{decodeFrameHeader} : \text{ByteArray} \to \text{Option}(\text{FrameHeader})$$

    Returns `none` if the input has fewer than 9 bytes. -/
def decodeFrameHeader (bs : ByteArray) (offset : Nat := 0) : Option FrameHeader :=
  if offset + 9 > bs.size then none
  else
    -- Length (24 bits)
    let len : UInt32 :=
      (bs[offset]!).toUInt32 <<< 16 |||
      (bs[offset + 1]!).toUInt32 <<< 8 |||
      (bs[offset + 2]!).toUInt32
    -- Type (8 bits)
    let ft := FrameType.fromUInt8 (bs[offset + 3]!)
    -- Flags (8 bits)
    let flags := bs[offset + 4]!
    -- Stream ID (31 bits, mask off reserved bit)
    let sid : UInt32 :=
      (bs[offset + 5]!.toUInt32 &&& 0x7F) <<< 24 |||
      bs[offset + 6]!.toUInt32 <<< 16 |||
      bs[offset + 7]!.toUInt32 <<< 8 |||
      bs[offset + 8]!.toUInt32
    some { payloadLength := len, frameType := ft, flags := flags, streamId := sid }

-- ── Settings decoding ──────────────────────────────────

/-- Decode a single settings parameter (6 bytes: 2 key + 4 value) from the given offset.
    $$\text{decodeSettingsParam} : \text{ByteArray} \to \text{Nat} \to \text{Option}(\text{SettingsKeyId} \times \text{UInt32})$$ -/
def decodeSettingsParam (bs : ByteArray) (offset : Nat := 0) : Option (SettingsKeyId × UInt32) := do
  let key ← decodeUInt16BE bs offset
  let value ← decodeUInt32BE bs (offset + 2)
  some (SettingsKeyId.fromUInt16 key, value)

/-- Decode all settings parameters from a SETTINGS frame payload.
    $$\text{decodeSettingsPayload} : \text{ByteArray} \to \text{Option}(\text{List}(\text{SettingsKeyId} \times \text{UInt32}))$$
    Returns `none` if the payload length is not a multiple of 6. -/
def decodeSettingsPayload (bs : ByteArray) : Option (List (SettingsKeyId × UInt32)) :=
  if bs.size % 6 != 0 then none
  else
    let count := bs.size / 6
    let rec go (i : Nat) (acc : List (SettingsKeyId × UInt32)) (fuel : Nat) :
        Option (List (SettingsKeyId × UInt32)) :=
      match fuel with
      | 0 => some acc.reverse
      | fuel' + 1 =>
        match decodeSettingsParam bs (i * 6) with
        | none => none
        | some param => go (i + 1) (param :: acc) fuel'
    go 0 [] count

/-- Apply decoded settings parameters to a `Settings` structure.
    $$\text{applySettings} : \text{Settings} \to \text{List}(\text{SettingsKeyId} \times \text{UInt32}) \to \text{Settings}$$ -/
def applySettings (s : Settings) (params : List (SettingsKeyId × UInt32)) : Settings :=
  params.foldl (fun s (k, v) =>
    match k with
    | .headerTableSize => { s with headerTableSize := v.toNat }
    | .enablePush => { s with enablePush := v != 0 }
    | .maxConcurrentStreams => { s with maxConcurrentStreams := some v.toNat }
    | .initialWindowSize => { s with initialWindowSize := v }
    | .maxFrameSize => { s with maxFrameSize := v }
    | .maxHeaderListSize => { s with maxHeaderListSize := some v.toNat }
    | .unknown _ => s
  ) s

-- ── GOAWAY decoding ────────────────────────────────────

/-- Decode a GOAWAY frame payload.
    $$\text{decodeGoaway} : \text{ByteArray} \to \text{Option}(\text{StreamId} \times \text{ErrorCode} \times \text{ByteArray})$$
    Returns (lastStreamId, errorCode, debugData). -/
def decodeGoaway (bs : ByteArray) : Option (StreamId × ErrorCode × ByteArray) :=
  if bs.size < 8 then none
  else do
    let lastStream ← decodeUInt32BE bs 0
    let errCode ← decodeUInt32BE bs 4
    let debugData := bs.extract 8 bs.size
    some (lastStream &&& 0x7FFFFFFF, ErrorCode.fromUInt32 errCode, debugData)

-- ── WINDOW_UPDATE decoding ─────────────────────────────

/-- Decode a WINDOW_UPDATE frame payload.
    $$\text{decodeWindowUpdate} : \text{ByteArray} \to \text{Option}(\text{UInt32})$$
    Returns the window size increment (31 bits). -/
def decodeWindowUpdate (bs : ByteArray) : Option UInt32 :=
  if bs.size < 4 then none
  else do
    let inc ← decodeUInt32BE bs 0
    some (inc &&& 0x7FFFFFFF)

-- ── RST_STREAM decoding ───────────────────────────────

/-- Decode a RST_STREAM frame payload.
    $$\text{decodeRstStream} : \text{ByteArray} \to \text{Option}(\text{ErrorCode})$$ -/
def decodeRstStream (bs : ByteArray) : Option ErrorCode :=
  if bs.size < 4 then none
  else do
    let code ← decodeUInt32BE bs 0
    some (ErrorCode.fromUInt32 code)

-- ── Priority decoding ──────────────────────────────────

/-- Decode priority fields from a byte array at the given offset.
    Parses: 1-bit exclusive flag + 31-bit stream dependency + 8-bit weight.
    Total: 5 bytes.

    $$\text{decodePriority} : \text{ByteArray} \to \text{Nat} \to \text{Option}(\text{Bool} \times \text{StreamId} \times \text{UInt8})$$

    Returns `(exclusive, dependency, weight)` or `none` if fewer than `offset + 5` bytes. -/
def decodePriority (bs : ByteArray) (offset : Nat := 0) : Option (Bool × StreamId × UInt8) :=
  if offset + 5 > bs.size then none
  else do
    let first ← decodeUInt32BE bs offset
    let exclusive := (first &&& 0x80000000) != 0
    let dependency := first &&& 0x7FFFFFFF
    let weight := bs[offset + 4]!
    some (exclusive, dependency, weight)

-- ── Padding decoding ───────────────────────────────────

/-- Decode padding from a padded frame payload. Extracts the actual content
    by reading the pad length byte and removing the padding suffix.

    $$\text{decodePadding} : \text{ByteArray} \to \text{Option}(\text{ByteArray} \times \text{Nat})$$

    Returns `(content, padLen)` or `none` if:
    - The payload is empty (no pad length byte)
    - The pad length exceeds the remaining payload

    RFC 9113 Section 6.1: The total padding + pad length byte must not exceed
    the frame payload length. -/
def decodePadding (bs : ByteArray) : Option (ByteArray × Nat) :=
  if bs.size == 0 then none
  else
    let padLen := bs[0]!.toNat
    -- padLen bytes of padding + 1 byte for the pad length field itself
    if padLen + 1 > bs.size then none
    else
      let content := bs.extract 1 (bs.size - padLen)
      some (content, padLen)

-- ── Frame size validation ──────────────────────────────

/-- Validate frame size constraints per RFC 9113.

    Returns `some errorCode` if the frame violates size constraints, `none` if valid.

    Constraints checked:
    - PING: payload must be exactly 8 bytes (FRAME_SIZE_ERROR)
    - RST_STREAM: payload must be exactly 4 bytes (FRAME_SIZE_ERROR)
    - PRIORITY: payload must be exactly 5 bytes (FRAME_SIZE_ERROR)
    - SETTINGS: payload must be a multiple of 6 bytes (FRAME_SIZE_ERROR)
    - SETTINGS ACK: payload must be 0 bytes (FRAME_SIZE_ERROR)
    - WINDOW_UPDATE: payload must be exactly 4 bytes (FRAME_SIZE_ERROR)
    - Any frame: payload must not exceed maxFrameSize (FRAME_SIZE_ERROR)

    $$\text{validateFrameSize} : \text{FrameHeader} \to \text{Settings} \to \text{Option}(\text{ErrorCode})$$ -/
def validateFrameSize (h : FrameHeader) (s : Settings) : Option ErrorCode :=
  let len := h.payloadLength
  -- Check max frame size for all frame types
  if len > s.maxFrameSize then some .frameSizeError
  else match h.frameType with
  | .ping =>
    if len != 8 then some .frameSizeError else none
  | .rstStream =>
    if len != 4 then some .frameSizeError else none
  | .priority =>
    if len != 5 then some .frameSizeError else none
  | .settings =>
    if FrameFlags.test h.flags FrameFlags.ack then
      if len != 0 then some .frameSizeError else none
    else
      if len.toNat % 6 != 0 then some .frameSizeError else none
  | .windowUpdate =>
    if len != 4 then some .frameSizeError else none
  | _ => none

end Network.HTTP2
