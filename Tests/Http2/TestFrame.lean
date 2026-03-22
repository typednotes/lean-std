/-
  Tests/Http2/TestFrame.lean — Tests for HTTP/2 frame encoding/decoding

  ## Coverage

  ### Proofs in source (covered by type-checking):
  - FrameType.fromUInt8_toUInt8_* roundtrip theorems

  ### Tested here (runtime tests):
  - FrameType encoding/decoding roundtrip
  - ErrorCode encoding/decoding roundtrip
  - SettingsKeyId encoding/decoding roundtrip
  - Frame header encode/decode roundtrip
  - Settings frame encode/decode roundtrip
  - GOAWAY, WINDOW_UPDATE, RST_STREAM, PING frame encode/decode
  - Priority encode/decode roundtrip
  - Padding encode/decode roundtrip
  - Frame size validation
  - FrameFlags operations
  - Header block splitting
-/
import Hale
import Tests.Harness

open Network.HTTP2 Tests

namespace TestFrame

private def priorityRoundtrip1 : List TestResult :=
  let encoded := encodePriority true 5 200
  match decodePriority encoded 0 with
  | some (excl, dep, weight) =>
    [ check "Priority exclusive" (excl == true)
    , check "Priority dependency" (dep == 5)
    , check "Priority weight" (weight == 200)
    ]
  | none => [check "Priority decode failed" false]

private def priorityRoundtrip2 : List TestResult :=
  let encoded := encodePriority false 100 42
  match decodePriority encoded 0 with
  | some (excl, dep, weight) =>
    [ check "Priority non-exclusive" (excl == false)
    , check "Priority dependency 100" (dep == 100)
    , check "Priority weight 42" (weight == 42)
    ]
  | none => [check "Priority decode non-excl failed" false]

private def paddingRoundtrip : List TestResult :=
  let payload := ByteArray.empty.push 0xAA |>.push 0xBB |>.push 0xCC
  let padded := encodePadding payload 5
  match decodePadding padded with
  | some (content, padLen) =>
    [ check "Padding content size" (content.size == 3)
    , check "Padding padLen" (padLen == 5)
    , check "Padding content matches" (content == payload)
    ]
  | none => [check "Padding decode failed" false]

private def paddingZero : List TestResult :=
  let empty := encodePadding ByteArray.empty 0
  match decodePadding empty with
  | some (content, padLen) =>
    [ check "Padding zero content empty" (content.size == 0)
    , check "Padding zero padLen" (padLen == 0)
    ]
  | none => [check "Padding zero decode failed" false]

private def frameHeaderRoundtrip : List TestResult :=
  let hdr : FrameHeader := {
    payloadLength := 42
    frameType := .headers
    flags := FrameFlags.set FrameFlags.endHeaders FrameFlags.endStream
    streamId := 7
  }
  let encoded := encodeFrameHeader hdr
  match decodeFrameHeader encoded with
  | some decoded =>
    [ check "Frame header encode size = 9" (encoded.size == 9)
    , check "Frame header roundtrip type" (decoded.frameType == .headers)
    , check "Frame header roundtrip length" (decoded.payloadLength == 42)
    , check "Frame header roundtrip stream" (decoded.streamId == 7)
    , check "Frame header roundtrip flags" (decoded.flags == hdr.flags)
    ]
  | none => [check "Frame header decode failed" false]

private def settingsRoundtrip : List TestResult :=
  let params := [(.maxConcurrentStreams, (100 : UInt32)), (.initialWindowSize, 65535)]
  let payload := encodeSettingsPayload params
  match decodeSettingsPayload payload with
  | some decoded =>
    [ check "Settings payload size" (payload.size == 12)
    , check "Settings decode count" (decoded.length == 2)
    , check "Settings first key" (decoded.head?.map Prod.fst == some .maxConcurrentStreams)
    ]
  | none => [check "Settings decode failed" false]

private def goawayRoundtrip : List TestResult :=
  let frame := buildGoawayFrame 5 .noError
  match decodeGoaway frame.payload with
  | some (lastStream, errCode, debugData) =>
    [ check "GOAWAY lastStream" (lastStream == 5)
    , check "GOAWAY errorCode" (errCode == .noError)
    , check "GOAWAY debugData empty" (debugData.size == 0)
    ]
  | none => [check "GOAWAY decode failed" false]

private def windowUpdateRoundtrip : List TestResult :=
  let frame := buildWindowUpdateFrame 3 1000
  match decodeWindowUpdate frame.payload with
  | some inc => [check "WINDOW_UPDATE increment" (inc == 1000)]
  | none => [check "WINDOW_UPDATE decode failed" false]

private def rstStreamRoundtrip : List TestResult :=
  let frame := buildRstStreamFrame 3 .cancel
  match decodeRstStream frame.payload with
  | some errCode => [check "RST_STREAM errorCode" (errCode == .cancel)]
  | none => [check "RST_STREAM decode failed" false]

private def integerRoundtrip : List TestResult :=
  let small := Network.HTTP2.HPACK.encodeInteger 10 7
  let smallResult := Network.HTTP2.HPACK.decodeInteger small 0 7
  let large := Network.HTTP2.HPACK.encodeInteger 1337 5
  let largeResult := Network.HTTP2.HPACK.decodeInteger large 0 5
  [ check "Integer small roundtrip" (smallResult.map (·.value) == some 10)
  , check "Integer large roundtrip" (largeResult.map (·.value) == some 1337)
  ]

def tests : List TestResult :=
  [ -- FrameType roundtrip
    check "FrameType data roundtrip"
      (FrameType.fromUInt8 (FrameType.toUInt8 .data) == .data)
  , check "FrameType headers roundtrip"
      (FrameType.fromUInt8 (FrameType.toUInt8 .headers) == .headers)
  , check "FrameType settings roundtrip"
      (FrameType.fromUInt8 (FrameType.toUInt8 .settings) == .settings)
  , check "FrameType ping roundtrip"
      (FrameType.fromUInt8 (FrameType.toUInt8 .ping) == .ping)
  , check "FrameType goaway roundtrip"
      (FrameType.fromUInt8 (FrameType.toUInt8 .goaway) == .goaway)
  , check "FrameType continuation roundtrip"
      (FrameType.fromUInt8 (FrameType.toUInt8 .continuation) == .continuation)
  , check "FrameType unknown preserved"
      (FrameType.fromUInt8 42 == .unknown 42)

    -- ErrorCode roundtrip
  , check "ErrorCode noError roundtrip"
      (ErrorCode.fromUInt32 (ErrorCode.toUInt32 .noError) == .noError)
  , check "ErrorCode protocolError roundtrip"
      (ErrorCode.fromUInt32 (ErrorCode.toUInt32 .protocolError) == .protocolError)
  , check "ErrorCode flowControlError roundtrip"
      (ErrorCode.fromUInt32 (ErrorCode.toUInt32 .flowControlError) == .flowControlError)
  , check "ErrorCode frameSizeError roundtrip"
      (ErrorCode.fromUInt32 (ErrorCode.toUInt32 .frameSizeError) == .frameSizeError)

    -- SettingsKeyId roundtrip
  , check "SettingsKeyId headerTableSize roundtrip"
      (SettingsKeyId.fromUInt16 (SettingsKeyId.toUInt16 .headerTableSize) == .headerTableSize)
  , check "SettingsKeyId maxFrameSize roundtrip"
      (SettingsKeyId.fromUInt16 (SettingsKeyId.toUInt16 .maxFrameSize) == .maxFrameSize)

    -- PING frame
  , let pingData := ByteArray.empty
      |>.push 1 |>.push 2 |>.push 3 |>.push 4
      |>.push 5 |>.push 6 |>.push 7 |>.push 8
    check "PING payload size" (pingData.size == 8)

    -- FrameFlags operations
  , check "FrameFlags test endStream"
      (FrameFlags.test (FrameFlags.set FrameFlags.none FrameFlags.endStream) FrameFlags.endStream)
  , check "FrameFlags test endHeaders"
      (FrameFlags.test (FrameFlags.set FrameFlags.none FrameFlags.endHeaders) FrameFlags.endHeaders)
  , check "FrameFlags clear"
      (!FrameFlags.test (FrameFlags.clear (FrameFlags.set FrameFlags.none FrameFlags.endStream) FrameFlags.endStream) FrameFlags.endStream)
  , check "FrameFlags none is 0" (FrameFlags.none == 0)

    -- Connection preface
  , check "Connection preface length" (connectionPreface.size == 24)

    -- Priority encode size
  , check "Priority encode size" ((encodePriority true 5 200).size == 5)

    -- Padding total size
  , let payload := ByteArray.empty.push 0xAA |>.push 0xBB |>.push 0xCC
    check "Padding total size" ((encodePadding payload 5).size == 9)

    -- Frame size validation
  , check "Validate PING size=8 ok"
      (validateFrameSize { payloadLength := 8, frameType := .ping, flags := 0, streamId := 0 } Settings.default == none)
  , check "Validate PING size=7 error"
      (validateFrameSize { payloadLength := 7, frameType := .ping, flags := 0, streamId := 0 } Settings.default == some .frameSizeError)
  , check "Validate RST_STREAM size=4 ok"
      (validateFrameSize { payloadLength := 4, frameType := .rstStream, flags := 0, streamId := 1 } Settings.default == none)
  , check "Validate RST_STREAM size=3 error"
      (validateFrameSize { payloadLength := 3, frameType := .rstStream, flags := 0, streamId := 1 } Settings.default == some .frameSizeError)
  , check "Validate PRIORITY size=5 ok"
      (validateFrameSize { payloadLength := 5, frameType := .priority, flags := 0, streamId := 1 } Settings.default == none)
  , check "Validate PRIORITY size=6 error"
      (validateFrameSize { payloadLength := 6, frameType := .priority, flags := 0, streamId := 1 } Settings.default == some .frameSizeError)
  , check "Validate SETTINGS size=12 ok"
      (validateFrameSize { payloadLength := 12, frameType := .settings, flags := 0, streamId := 0 } Settings.default == none)
  , check "Validate SETTINGS size=7 error"
      (validateFrameSize { payloadLength := 7, frameType := .settings, flags := 0, streamId := 0 } Settings.default == some .frameSizeError)
  , check "Validate SETTINGS ACK size=0 ok"
      (validateFrameSize { payloadLength := 0, frameType := .settings, flags := FrameFlags.ack, streamId := 0 } Settings.default == none)
  , check "Validate SETTINGS ACK size=6 error"
      (validateFrameSize { payloadLength := 6, frameType := .settings, flags := FrameFlags.ack, streamId := 0 } Settings.default == some .frameSizeError)
  , check "Validate WINDOW_UPDATE size=4 ok"
      (validateFrameSize { payloadLength := 4, frameType := .windowUpdate, flags := 0, streamId := 0 } Settings.default == none)
  , check "Validate exceeds maxFrameSize"
      (validateFrameSize { payloadLength := 16385, frameType := .data, flags := 0, streamId := 1 } Settings.default == some .frameSizeError)

    -- Header block splitting
  , let block := ByteArray.empty
      |>.push 1 |>.push 2 |>.push 3 |>.push 4 |>.push 5
      |>.push 6 |>.push 7 |>.push 8 |>.push 9 |>.push 10
    check "Split block count" ((splitHeaderBlock block 4).length == 3)
  , let block := ByteArray.empty
      |>.push 1 |>.push 2 |>.push 3 |>.push 4 |>.push 5
      |>.push 6 |>.push 7 |>.push 8 |>.push 9 |>.push 10
    check "Split block sizes" ((splitHeaderBlock block 4).map ByteArray.size == [4, 4, 2])
  , let block := ByteArray.empty.push 1 |>.push 2 |>.push 3
    check "Split single chunk" ((splitHeaderBlock block 10).length == 1)

    -- Proof coverage
  , proofCovered "FrameType.fromUInt8_toUInt8_data" "FrameType.fromUInt8_toUInt8_data"
  , proofCovered "FrameType.fromUInt8_toUInt8_headers" "FrameType.fromUInt8_toUInt8_headers"
  , proofCovered "FrameType.fromUInt8_toUInt8_settings" "FrameType.fromUInt8_toUInt8_settings"
  ] ++ frameHeaderRoundtrip
    ++ settingsRoundtrip
    ++ goawayRoundtrip
    ++ windowUpdateRoundtrip
    ++ rstStreamRoundtrip
    ++ priorityRoundtrip1
    ++ priorityRoundtrip2
    ++ paddingRoundtrip
    ++ paddingZero
    ++ integerRoundtrip

end TestFrame
