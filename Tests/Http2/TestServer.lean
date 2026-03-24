/-
  Tests/Http2/TestServer.lean — Tests for HTTP/2 server functionality
-/
import Hale
import Tests.Harness

open Network.HTTP2 Network.HTTP2.HPACK Tests

namespace TestServer

private def sid (n : UInt32) : StreamId := StreamId.fromWire n

def tests : IO (List TestResult) := do
  let mut results : List TestResult := []

  let state := ConnectionState.initial
  results := results ++ [
    check "Initial state default settings" (state.localSettings == Settings.default),
    check "Initial state no goaway" (state.goawayReceived == false),
    check "Initial state idle header block" (state.headerBlockState == .idle),
    check "Initial state no peer settings" (state.peerSettingsReceived == false)
  ]

  results := results ++ [
    check "HeaderBlockState idle not assembling" (!HeaderBlockState.idle.isAssembling),
    check "HeaderBlockState assembling"
      (HeaderBlockState.assembling (sid 1) ByteArray.empty).isAssembling,
    check "HeaderBlockState idle streamId?" (HeaderBlockState.idle.streamId? == none),
    check "HeaderBlockState assembling streamId?"
      ((HeaderBlockState.assembling (sid 5) ByteArray.empty).streamId? == some (sid 5))
  ]

  let hbs := HeaderBlockState.assembling (sid 3) ByteArray.empty
  let fragment := ByteArray.empty.push 0xAA |>.push 0xBB
  match hbs.appendFragment fragment with
  | some newHBS =>
    results := results ++ [
      check "HeaderBlockState append succeeds" true,
      check "HeaderBlockState append still assembling" newHBS.isAssembling
    ]
    match newHBS.complete with
    | some (completeSid, block) =>
      results := results ++ [
        check "HeaderBlockState complete streamId" (completeSid == sid 3),
        check "HeaderBlockState complete block size" (block.size == 2)
      ]
    | none =>
      results := results ++ [check "HeaderBlockState complete failed" false]
  | none =>
    results := results ++ [check "HeaderBlockState append failed" false]

  results := results ++ [
    check "HeaderBlockState append idle fails"
      (HeaderBlockState.idle.appendFragment ByteArray.empty == none)
  ]

  results := results ++ [
    check "isClientStream odd" (isClientStream (sid 1)),
    check "isClientStream 3" (isClientStream (sid 3)),
    check "isClientStream even" (!isClientStream (sid 2)),
    check "isClientStream 0" (!isClientStream (sid 0)),
    check "isServerStream 2" (isServerStream (sid 2)),
    check "isServerStream 4" (isServerStream (sid 4)),
    check "isServerStream odd" (!isServerStream (sid 1)),
    check "isServerStream 0" (!isServerStream (sid 0)),
    check "isConnectionStream 0" (isConnectionStream (sid 0)),
    check "isConnectionStream 1" (!isConnectionStream (sid 1)),
    check "validateStreamId monotonic" (validateStreamId (sid 3) (sid 1)),
    check "validateStreamId not monotonic" (!validateStreamId (sid 1) (sid 3)),
    check "validateStreamId equal" (!validateStreamId (sid 1) (sid 1))
  ]

  let table := StreamTable.empty
  results := results ++ [
    check "StreamTable empty no streams" (table.streams.size == 0),
    check "StreamTable empty lastClient 0" (table.lastClientStreamId == sid 0)
  ]

  match table.openClientStream (sid 1) 65535 with
  | some table' =>
    results := results ++ [
      check "StreamTable open stream 1" true,
      check "StreamTable lastClientStreamId" (table'.lastClientStreamId == sid 1),
      check "StreamTable lookup stream 1" (table'.lookup (sid 1) |>.isSome)
    ]
    match table'.openClientStream (sid 3) 65535 with
    | some table'' =>
      results := results ++ [
        check "StreamTable open stream 3" true,
        check "StreamTable active count" (table''.activeStreamCount == 2)
      ]
    | none =>
      results := results ++ [check "StreamTable open stream 3 failed" false]
    results := results ++ [
      check "StreamTable reject non-monotonic"
        (table'.openClientStream (sid 1) 65535).isNone,
      check "StreamTable reject even stream"
        (table'.openClientStream (sid 2) 65535).isNone
    ]
  | none =>
    results := results ++ [check "StreamTable open stream 1 failed" false]

  let window := FlowWindow.default
  results := results ++ [
    checkEq "FlowWindow default size" 65535 window.size,
    checkEq "FlowWindow available" 65535 window.available
  ]

  match window.increment 1000 with
  | .ok w => results := results ++ [checkEq "FlowWindow increment" 66535 w.size]
  | .error _ => results := results ++ [check "FlowWindow increment failed" false]

  match window.increment 0 with
  | .error _ => results := results ++ [check "FlowWindow increment 0 error" true]
  | .ok _ => results := results ++ [check "FlowWindow increment 0 should error" false]

  let w := window.consume 100
  results := results ++ [checkEq "FlowWindow consume" 65435 w.size]

  let connErr : ConnectionError := { errorCode := .protocolError, message := "test" }
  let streamErr : StreamError := { streamId := sid 5, errorCode := .cancel, message := "cancelled" }
  results := results ++ [
    check "ConnectionError errorCode" (connErr.errorCode == .protocolError),
    check "StreamError streamId" (streamErr.streamId == sid 5),
    check "StreamError errorCode" (streamErr.errorCode == .cancel)
  ]

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

  let goawayFrame := buildGoawayFrame (sid 7) .protocolError "test error".toUTF8
  match decodeGoaway goawayFrame.payload with
  | some (lastStream, errCode, debugData) =>
    results := results ++ [
      check "GOAWAY lastStream" (lastStream.val == 7),
      check "GOAWAY errCode" (errCode == .protocolError),
      check "GOAWAY debugData non-empty" (debugData.size > 0)
    ]
  | none =>
    results := results ++ [check "GOAWAY decode failed" false]

  let contFrame := buildContinuationFrame (sid 5) (ByteArray.empty.push 1 |>.push 2) (endHeaders := true)
  results := results ++ [
    check "CONTINUATION type" (contFrame.header.frameType == .continuation),
    check "CONTINUATION streamId" (contFrame.header.streamId == sid 5),
    check "CONTINUATION endHeaders flag"
      (FrameFlags.test contFrame.header.flags FrameFlags.endHeaders),
    check "CONTINUATION payload size" (contFrame.payload.size == 2)
  ]

  let dt := DynamicTable.empty 4096
  let headers : List (String × String) := [(":method", "GET"), (":path", "/"), (":scheme", "https"), ("x-custom-header", "some-long-value-that-wont-be-indexed")]
  let (headerBlock, _) := encodeHeaders dt headers
  let chunks := splitHeaderBlock headerBlock 3
  results := results ++ [
    check "Split forces multiple chunks" (chunks.length > 1),
    let reassembled := chunks.foldl (· ++ ·) ByteArray.empty
    check "Reassembled matches original" (reassembled == headerBlock)
  ]

  return results

end TestServer
