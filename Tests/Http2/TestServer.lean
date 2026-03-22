/-
  Tests/Http2/TestServer.lean — Tests for HTTP/2 server functionality

  ## Coverage

  ### Tested here (runtime tests):
  - Settings exchange (processSettings)
  - HEADERS + CONTINUATION assembly (HeaderBlockState)
  - GOAWAY handling
  - Connection state initialization
  - Stream ID validation
  - Flow control window operations
  - ConnectionError / StreamError construction

  ### Not yet covered:
  - Full runHTTP2Connection (requires IO mocking)
-/
import Hale
import Tests.Harness

open Network.HTTP2 Network.HTTP2.HPACK Tests

namespace TestServer

def tests : IO (List TestResult) := do
  let mut results : List TestResult := []

  -- ConnectionState initialization
  let state := ConnectionState.initial
  results := results ++ [
    check "Initial state default settings" (state.localSettings == Settings.default),
    check "Initial state no goaway" (state.goawayReceived == false),
    check "Initial state idle header block" (state.headerBlockState == .idle),
    check "Initial state no peer settings" (state.peerSettingsReceived == false)
  ]

  -- HeaderBlockState tests
  results := results ++ [
    check "HeaderBlockState idle not assembling" (!HeaderBlockState.idle.isAssembling),
    check "HeaderBlockState assembling"
      (HeaderBlockState.assembling 1 ByteArray.empty).isAssembling,
    check "HeaderBlockState idle streamId?" (HeaderBlockState.idle.streamId? == none),
    check "HeaderBlockState assembling streamId?"
      ((HeaderBlockState.assembling 5 ByteArray.empty).streamId? == some 5)
  ]

  -- HeaderBlockState append
  let hbs := HeaderBlockState.assembling 3 ByteArray.empty
  let fragment := ByteArray.empty.push 0xAA |>.push 0xBB
  match hbs.appendFragment fragment with
  | some newHBS =>
    results := results ++ [
      check "HeaderBlockState append succeeds" true,
      check "HeaderBlockState append still assembling" newHBS.isAssembling
    ]
    match newHBS.complete with
    | some (sid, block) =>
      results := results ++ [
        check "HeaderBlockState complete streamId" (sid == 3),
        check "HeaderBlockState complete block size" (block.size == 2)
      ]
    | none =>
      results := results ++ [check "HeaderBlockState complete failed" false]
  | none =>
    results := results ++ [check "HeaderBlockState append failed" false]

  -- HeaderBlockState append to idle fails
  results := results ++ [
    check "HeaderBlockState append idle fails"
      (HeaderBlockState.idle.appendFragment ByteArray.empty == none)
  ]

  -- Stream ID validation
  results := results ++ [
    check "isClientStream odd" (isClientStream 1),
    check "isClientStream 3" (isClientStream 3),
    check "isClientStream even" (!isClientStream 2),
    check "isClientStream 0" (!isClientStream 0),
    check "isServerStream 2" (isServerStream 2),
    check "isServerStream 4" (isServerStream 4),
    check "isServerStream odd" (!isServerStream 1),
    check "isServerStream 0" (!isServerStream 0),
    check "isConnectionStream 0" (isConnectionStream 0),
    check "isConnectionStream 1" (!isConnectionStream 1),
    check "validateStreamId monotonic" (validateStreamId 3 1),
    check "validateStreamId not monotonic" (!validateStreamId 1 3),
    check "validateStreamId equal" (!validateStreamId 1 1)
  ]

  -- StreamTable operations
  let table := StreamTable.empty
  results := results ++ [
    check "StreamTable empty no streams" (table.streams.size == 0),
    check "StreamTable empty lastClient 0" (table.lastClientStreamId == 0)
  ]

  -- Open a client stream
  match table.openClientStream 1 65535 with
  | some table' =>
    results := results ++ [
      check "StreamTable open stream 1" true,
      check "StreamTable lastClientStreamId" (table'.lastClientStreamId == 1),
      check "StreamTable lookup stream 1" (table'.lookup 1 |>.isSome)
    ]
    -- Try to open stream 3
    match table'.openClientStream 3 65535 with
    | some table'' =>
      results := results ++ [
        check "StreamTable open stream 3" true,
        check "StreamTable active count" (table''.activeStreamCount == 2)
      ]
    | none =>
      results := results ++ [check "StreamTable open stream 3 failed" false]
    -- Try to open non-monotonic stream (should fail)
    results := results ++ [
      check "StreamTable reject non-monotonic"
        (table'.openClientStream 1 65535).isNone
    ]
    -- Try to open even stream (should fail)
    results := results ++ [
      check "StreamTable reject even stream"
        (table'.openClientStream 2 65535).isNone
    ]
  | none =>
    results := results ++ [check "StreamTable open stream 1 failed" false]

  -- Flow control
  let window := FlowWindow.default
  results := results ++ [
    checkEq "FlowWindow default size" 65535 window.size,
    checkEq "FlowWindow available" 65535 window.available
  ]

  -- FlowWindow increment
  match window.increment 1000 with
  | .ok w =>
    results := results ++ [
      checkEq "FlowWindow increment" 66535 w.size
    ]
  | .error _ =>
    results := results ++ [check "FlowWindow increment failed" false]

  -- FlowWindow increment 0 is error
  match window.increment 0 with
  | .error _ =>
    results := results ++ [check "FlowWindow increment 0 error" true]
  | .ok _ =>
    results := results ++ [check "FlowWindow increment 0 should error" false]

  -- FlowWindow consume
  let w := window.consume 100
  results := results ++ [
    checkEq "FlowWindow consume" 65435 w.size
  ]

  -- ConnectionError / StreamError
  let connErr : ConnectionError := { errorCode := .protocolError, message := "test" }
  let streamErr : StreamError := { streamId := 5, errorCode := .cancel, message := "cancelled" }
  results := results ++ [
    check "ConnectionError errorCode" (connErr.errorCode == .protocolError),
    check "StreamError streamId" (streamErr.streamId == 5),
    check "StreamError errorCode" (streamErr.errorCode == .cancel)
  ]

  -- Settings processing (simulated)
  let settingsPayload := encodeSettingsPayload [
    (.maxConcurrentStreams, 100),
    (.initialWindowSize, 32768)
  ]
  match decodeSettingsPayload settingsPayload with
  | some params =>
    let newSettings := applySettings Settings.default params
    results := results ++ [
      check "Settings apply maxConcurrentStreams" (newSettings.maxConcurrentStreams == some 100),
      check "Settings apply initialWindowSize" (newSettings.initialWindowSize == 32768)
    ]
  | none =>
    results := results ++ [check "Settings decode failed" false]

  -- GOAWAY frame construction and decode
  let goawayFrame := buildGoawayFrame 7 .protocolError "test error".toUTF8
  match decodeGoaway goawayFrame.payload with
  | some (lastStream, errCode, debugData) =>
    results := results ++ [
      check "GOAWAY lastStream" (lastStream == 7),
      check "GOAWAY errCode" (errCode == .protocolError),
      check "GOAWAY debugData non-empty" (debugData.size > 0)
    ]
  | none =>
    results := results ++ [check "GOAWAY decode failed" false]

  -- CONTINUATION frame construction
  let contFrame := buildContinuationFrame 5 (ByteArray.empty.push 1 |>.push 2) (endHeaders := true)
  results := results ++ [
    check "CONTINUATION type" (contFrame.header.frameType == .continuation),
    check "CONTINUATION streamId" (contFrame.header.streamId == 5),
    check "CONTINUATION endHeaders flag"
      (FrameFlags.test contFrame.header.flags FrameFlags.endHeaders),
    check "CONTINUATION payload size" (contFrame.payload.size == 2)
  ]

  -- Header block splitting and HEADERS + CONTINUATION assembly
  let dt := DynamicTable.empty 4096
  let headers : List (String × String) := [(":method", "GET"), (":path", "/"), (":scheme", "https"), ("x-custom-header", "some-long-value-that-wont-be-indexed")]
  let (headerBlock, _) := encodeHeaders dt headers
  let chunks := splitHeaderBlock headerBlock 3  -- Very small max to force splitting
  results := results ++ [
    check "Split forces multiple chunks" (chunks.length > 1),
    -- Reassemble: concatenate all chunks
    let reassembled := chunks.foldl (· ++ ·) ByteArray.empty
    check "Reassembled matches original" (reassembled == headerBlock)
  ]

  return results

end TestServer
