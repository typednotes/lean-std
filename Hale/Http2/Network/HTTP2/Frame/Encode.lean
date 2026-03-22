/-
  Hale.Http2.Network.HTTP2.Frame.Encode — HTTP/2 frame encoding

  Serialises HTTP/2 frames to wire format as defined in RFC 9113 Section 4.

  ## Design

  Encoding functions produce `ByteArray` values in network byte order (big-endian).
  The frame header is always exactly 9 bytes. Payload encoding is frame-type-specific.

  ## Guarantees

  - `encodeFrameHeader` always produces exactly 9 bytes
  - `encodeFrame` produces header ++ payload
  - `encodePriority` produces exactly 5 bytes
  - Padding adds exactly `padLen + 1` bytes of overhead

  ## Haskell equivalent
  `Network.HTTP2.Frame.Encode` (https://hackage.haskell.org/package/http2)
-/
import Hale.Http2.Network.HTTP2.Frame.Types

namespace Network.HTTP2

-- ── Wire encoding helpers ──────────────────────────────

/-- Encode a 16-bit unsigned integer in big-endian (network) byte order.
    $$\text{encodeUInt16BE}(n) = [\lfloor n/256 \rfloor, n \bmod 256]$$ -/
@[inline] def encodeUInt16BE (n : UInt16) : ByteArray :=
  ByteArray.empty.push (n >>> 8).toUInt8 |>.push n.toUInt8

/-- Encode a 32-bit unsigned integer in big-endian (network) byte order.
    $$\text{encodeUInt32BE}(n) = [b_3, b_2, b_1, b_0]$$ -/
@[inline] def encodeUInt32BE (n : UInt32) : ByteArray :=
  ByteArray.empty
    |>.push (n >>> 24).toUInt8
    |>.push (n >>> 16).toUInt8
    |>.push (n >>> 8).toUInt8
    |>.push n.toUInt8

-- ── Frame header encoding ──────────────────────────────

/-- Encode an HTTP/2 frame header to its 9-byte wire format.

    Wire format: `[Length (24)] [Type (8)] [Flags (8)] [R (1)] [Stream ID (31)]`

    $$\text{encodeFrameHeader} : \text{FrameHeader} \to \text{ByteArray}$$

    The result is always exactly 9 bytes. -/
def encodeFrameHeader (h : FrameHeader) : ByteArray :=
  let sid := h.streamId &&& 0x7FFFFFFF
  ByteArray.empty
    -- Length (24 bits, big-endian)
    |>.push (h.payloadLength >>> 16).toUInt8
    |>.push (h.payloadLength >>> 8).toUInt8
    |>.push h.payloadLength.toUInt8
    -- Type (8 bits)
    |>.push h.frameType.toUInt8
    -- Flags (8 bits)
    |>.push h.flags
    -- Stream ID (31 bits, high bit reserved/zero)
    |>.push (sid >>> 24).toUInt8
    |>.push (sid >>> 16).toUInt8
    |>.push (sid >>> 8).toUInt8
    |>.push sid.toUInt8

/-- Encode a complete HTTP/2 frame (header + payload).
    $$\text{encodeFrame} : \text{Frame} \to \text{ByteArray}$$ -/
def encodeFrame (f : Frame) : ByteArray :=
  encodeFrameHeader f.header ++ f.payload

-- ── Settings encoding ──────────────────────────────────

/-- Encode a single settings parameter (6 bytes: 2 for key + 4 for value).
    $$\text{encodeSettingsParam}(k, v) = \text{encodeUInt16BE}(k) \mathbin{+\!\!+} \text{encodeUInt32BE}(v)$$ -/
def encodeSettingsParam (key : SettingsKeyId) (value : UInt32) : ByteArray :=
  encodeUInt16BE key.toUInt16 ++ encodeUInt32BE value

/-- Encode a SETTINGS frame payload from a list of key-value pairs.
    $$|\text{result}| = 6 \times |\text{params}|$$ -/
def encodeSettingsPayload (params : List (SettingsKeyId × UInt32)) : ByteArray :=
  params.foldl (fun acc (k, v) => acc ++ encodeSettingsParam k v) ByteArray.empty

/-- Build a complete SETTINGS frame.
    $$\text{buildSettingsFrame} : \text{List}(\text{SettingsKeyId} \times \text{UInt32}) \to \text{Bool} \to \text{Frame}$$ -/
def buildSettingsFrame (params : List (SettingsKeyId × UInt32)) (isAck : Bool := false) : Frame :=
  let payload := if isAck then ByteArray.empty else encodeSettingsPayload params
  let flags := if isAck then FrameFlags.ack else FrameFlags.none
  { header := {
      payloadLength := payload.size.toUInt32
      frameType := .settings
      flags := flags
      streamId := 0
    }
    payload := payload }

-- ── PING encoding ──────────────────────────────────────

/-- Build a PING frame. Payload must be exactly 8 bytes.
    $$\text{buildPingFrame} : \text{ByteArray} \to \text{Bool} \to \text{Frame}$$ -/
def buildPingFrame (opaqueData : ByteArray) (isAck : Bool := false) : Frame :=
  let flags := if isAck then FrameFlags.ack else FrameFlags.none
  { header := {
      payloadLength := opaqueData.size.toUInt32
      frameType := .ping
      flags := flags
      streamId := 0
    }
    payload := opaqueData }

-- ── GOAWAY encoding ────────────────────────────────────

/-- Build a GOAWAY frame.
    $$\text{buildGoawayFrame} : \text{StreamId} \to \text{ErrorCode} \to \text{ByteArray} \to \text{Frame}$$ -/
def buildGoawayFrame (lastStreamId : StreamId) (errorCode : ErrorCode)
    (debugData : ByteArray := ByteArray.empty) : Frame :=
  let payload := encodeUInt32BE (lastStreamId &&& 0x7FFFFFFF) ++ encodeUInt32BE errorCode.toUInt32 ++ debugData
  { header := {
      payloadLength := payload.size.toUInt32
      frameType := .goaway
      flags := FrameFlags.none
      streamId := 0
    }
    payload := payload }

-- ── WINDOW_UPDATE encoding ─────────────────────────────

/-- Build a WINDOW_UPDATE frame.
    $$\text{buildWindowUpdateFrame} : \text{StreamId} \to \text{UInt32} \to \text{Frame}$$ -/
def buildWindowUpdateFrame (streamId : StreamId) (increment : UInt32) : Frame :=
  let payload := encodeUInt32BE (increment &&& 0x7FFFFFFF)
  { header := {
      payloadLength := 4
      frameType := .windowUpdate
      flags := FrameFlags.none
      streamId := streamId
    }
    payload := payload }

-- ── RST_STREAM encoding ───────────────────────────────

/-- Build a RST_STREAM frame.
    $$\text{buildRstStreamFrame} : \text{StreamId} \to \text{ErrorCode} \to \text{Frame}$$ -/
def buildRstStreamFrame (streamId : StreamId) (errorCode : ErrorCode) : Frame :=
  let payload := encodeUInt32BE errorCode.toUInt32
  { header := {
      payloadLength := 4
      frameType := .rstStream
      flags := FrameFlags.none
      streamId := streamId
    }
    payload := payload }

-- ── HEADERS encoding ───────────────────────────────────

/-- Build a HEADERS frame.
    $$\text{buildHeadersFrame} : \text{StreamId} \to \text{ByteArray} \to \text{Bool} \to \text{Bool} \to \text{Frame}$$ -/
def buildHeadersFrame (streamId : StreamId) (headerBlock : ByteArray)
    (endStream : Bool := false) (endHeaders : Bool := true) : Frame :=
  let flags := FrameFlags.none
  let flags := if endStream then FrameFlags.set flags FrameFlags.endStream else flags
  let flags := if endHeaders then FrameFlags.set flags FrameFlags.endHeaders else flags
  { header := {
      payloadLength := headerBlock.size.toUInt32
      frameType := .headers
      flags := flags
      streamId := streamId
    }
    payload := headerBlock }

-- ── DATA encoding ──────────────────────────────────────

/-- Build a DATA frame.
    $$\text{buildDataFrame} : \text{StreamId} \to \text{ByteArray} \to \text{Bool} \to \text{Frame}$$ -/
def buildDataFrame (streamId : StreamId) (payload : ByteArray)
    (endStream : Bool := false) : Frame :=
  let flags := if endStream then FrameFlags.endStream else FrameFlags.none
  { header := {
      payloadLength := payload.size.toUInt32
      frameType := .data
      flags := flags
      streamId := streamId
    }
    payload := payload }

-- ── Priority encoding ──────────────────────────────────

/-- Encode priority fields: 1-bit exclusive flag + 31-bit stream dependency + 8-bit weight.
    Total: exactly 5 bytes.

    Wire format: `[E (1)] [Stream Dependency (31)] [Weight (8)]`

    $$\text{encodePriority}(\text{excl}, \text{dep}, \text{weight}) : \text{ByteArray}_{5}$$ -/
def encodePriority (exclusive : Bool) (dependency : StreamId) (weight : UInt8) : ByteArray :=
  let dep := dependency &&& 0x7FFFFFFF
  let first := if exclusive then dep ||| 0x80000000 else dep
  encodeUInt32BE first |>.push weight

-- ── Padding encoding ───────────────────────────────────

/-- Apply padding to a payload. Prepends a 1-byte pad length field and appends
    zero-filled padding bytes.

    $$\text{encodePadding}(\text{payload}, \text{padLen}) =
      [\text{padLen}] \mathbin{+\!\!+} \text{payload} \mathbin{+\!\!+} [0]^{\text{padLen}}$$

    The padLen must be in [0, 255]. Total overhead is `padLen + 1` bytes. -/
def encodePadding (payload : ByteArray) (padLen : Nat) : ByteArray :=
  let padLenByte := (min padLen 255).toUInt8
  let result := ByteArray.empty.push padLenByte ++ payload
  -- Append padding zeros
  (List.range padLen).foldl (fun (acc : ByteArray) _ => acc.push 0) result

-- ── CONTINUATION encoding ──────────────────────────────

/-- Build a CONTINUATION frame.
    $$\text{buildContinuationFrame} : \text{StreamId} \to \text{ByteArray} \to \text{Bool} \to \text{Frame}$$ -/
def buildContinuationFrame (streamId : StreamId) (headerBlock : ByteArray)
    (endHeaders : Bool := false) : Frame :=
  let flags := if endHeaders then FrameFlags.endHeaders else FrameFlags.none
  { header := {
      payloadLength := headerBlock.size.toUInt32
      frameType := .continuation
      flags := flags
      streamId := streamId
    }
    payload := headerBlock }

-- ── Header block splitting ─────────────────────────────

/-- Split a large header block into chunks of at most `maxSize` bytes.
    Used when a header block exceeds the maximum frame payload size and must
    be sent across HEADERS + CONTINUATION frames.

    $$\text{splitHeaderBlock}(\text{block}, \text{maxSize}) = [\text{chunk}_1, \ldots, \text{chunk}_n]$$
    where $|\text{chunk}_i| \leq \text{maxSize}$ and $\bigoplus_i \text{chunk}_i = \text{block}$. -/
def splitHeaderBlock (block : ByteArray) (maxSize : Nat) : List ByteArray :=
  if maxSize == 0 then [block]
  else
    let rec go (offset : Nat) (acc : List ByteArray) (fuel : Nat) : List ByteArray :=
      match fuel with
      | 0 => acc.reverse
      | fuel' + 1 =>
        if offset >= block.size then acc.reverse
        else
          let remaining := block.size - offset
          let chunkSize := min remaining maxSize
          let chunk := block.extract offset (offset + chunkSize)
          go (offset + chunkSize) (chunk :: acc) fuel'
    go 0 [] (block.size / maxSize + 1)

end Network.HTTP2
