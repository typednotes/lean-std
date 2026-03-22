import Hale.Http3
import Tests.Harness

open Network.HTTP3 Tests

/-
  Coverage:
  - Proofs in source: FrameType.roundtrip_*, H3Error.roundtrip_*
  - Tested here: FrameType fromId/toId, variable-length integer encode/decode,
    Frame encode/decode roundtrip, H3Settings encode/decode, H3Error toCode/fromCode
  - Not covered: Huffman encoding (not implemented)
-/

namespace TestH3Frame

/-- Helper: check varint roundtrip for a given value. -/
private def checkVarIntRoundtrip (name : String) (val : UInt64) : TestResult :=
  let encoded := encodeVarInt val
  match decodeVarInt encoded with
  | some (decoded, consumed) =>
    check name (decoded == val && consumed == encoded.size)
  | none => check name false

def tests : List TestResult :=
  [ -- FrameType roundtrips (proof-covered)
    proofCovered "FrameType.roundtrip_data" "FrameType.roundtrip_data"
  , proofCovered "FrameType.roundtrip_headers" "FrameType.roundtrip_headers"
  , proofCovered "FrameType.roundtrip_settings" "FrameType.roundtrip_settings"
  , proofCovered "FrameType.roundtrip_goaway" "FrameType.roundtrip_goaway"

  -- FrameType toId/fromId
  , check "FrameType data toId"
      (FrameType.data.toId == 0x0)
  , check "FrameType headers toId"
      (FrameType.headers.toId == 0x1)
  , check "FrameType settings toId"
      (FrameType.settings.toId == 0x4)
  , check "FrameType goaway toId"
      (FrameType.goaway.toId == 0x7)
  , check "FrameType unknown preserves id"
      ((FrameType.unknown 0xFF).toId == 0xFF)
  , check "FrameType toString data"
      (toString FrameType.data == "DATA")
  , check "FrameType toString headers"
      (toString FrameType.headers == "HEADERS")

  -- Variable-length integer encoding (1 byte)
  , checkVarIntRoundtrip "varInt roundtrip 0" 0
  , checkVarIntRoundtrip "varInt roundtrip 1" 1
  , checkVarIntRoundtrip "varInt roundtrip 63" 63
  , check "varInt 0 encodes to 1 byte"
      ((encodeVarInt 0).size == 1)
  , check "varInt 63 encodes to 1 byte"
      ((encodeVarInt 63).size == 1)

  -- Variable-length integer encoding (2 bytes)
  , checkVarIntRoundtrip "varInt roundtrip 64" 64
  , checkVarIntRoundtrip "varInt roundtrip 16383" 16383
  , check "varInt 64 encodes to 2 bytes"
      ((encodeVarInt 64).size == 2)
  , check "varInt 16383 encodes to 2 bytes"
      ((encodeVarInt 16383).size == 2)

  -- Variable-length integer encoding (4 bytes)
  , checkVarIntRoundtrip "varInt roundtrip 16384" 16384
  , checkVarIntRoundtrip "varInt roundtrip 1073741823" 1073741823
  , check "varInt 16384 encodes to 4 bytes"
      ((encodeVarInt 16384).size == 4)

  -- Variable-length integer encoding (8 bytes)
  , checkVarIntRoundtrip "varInt roundtrip 1073741824" 1073741824
  , check "varInt 1073741824 encodes to 8 bytes"
      ((encodeVarInt 1073741824).size == 8)

  -- decodeVarInt edge cases
  , check "decodeVarInt empty returns none"
      (decodeVarInt ByteArray.empty == none)
  , check "decodeVarInt truncated 2-byte returns none"
      (decodeVarInt (ByteArray.mk #[0x40]) == none)

  -- Frame encode/decode roundtrip
  , check "Frame roundtrip DATA" (
      let f := Frame.mk .data (ByteArray.mk #[0x48, 0x65, 0x6C])
      match Frame.decode f.encode with
      | some (decoded, _) => decoded.frameType == .data && decoded.payload == f.payload
      | none => false)
  , check "Frame roundtrip HEADERS" (
      let f := Frame.mk .headers (ByteArray.mk #[0x00, 0x00, 0xC0 + 17])
      match Frame.decode f.encode with
      | some (decoded, _) => decoded.frameType == .headers && decoded.payload == f.payload
      | none => false)
  , check "Frame roundtrip empty payload" (
      let f := Frame.mk .settings ByteArray.empty
      match Frame.decode f.encode with
      | some (decoded, _) => decoded.frameType == .settings && decoded.payload.size == 0
      | none => false)

  -- H3Settings
  , check "H3Settings default encode is empty"
      (H3Settings.default.encode.size == 0)
  , check "H3Settings roundtrip with values" (
      let s := { H3Settings.default with qpackMaxTableCapacity := 4096, qpackBlockedStreams := 100 }
      match H3Settings.decode s.encode with
      | some decoded => decoded.qpackMaxTableCapacity == 4096 && decoded.qpackBlockedStreams == 100
      | none => false)

  -- H3Error roundtrips (proof-covered)
  , proofCovered "H3Error.roundtrip_noError" "H3Error.roundtrip_noError"
  , proofCovered "H3Error.roundtrip_internalError" "H3Error.roundtrip_internalError"
  , proofCovered "H3Error.roundtrip_versionFallback" "H3Error.roundtrip_versionFallback"

  -- H3Error toCode/fromCode
  , check "H3Error.noError code" (H3Error.noError.toCode == 0x100)
  , check "H3Error.generalProtocolError code" (H3Error.generalProtocolError.toCode == 0x101)
  , check "H3Error.versionFallback code" (H3Error.versionFallback.toCode == 0x110)
  , check "H3Error fromCode roundtrip noError"
      (H3Error.fromCode 0x100 == .noError)
  , check "H3Error fromCode unknown"
      (H3Error.fromCode 0x999 == .unknown 0x999)
  ]

end TestH3Frame
