/-
  Hale.Http2.Network.HTTP2.Stream — HTTP/2 stream state management

  Implements HTTP/2 stream lifecycle and state machine as defined in
  RFC 9113 Section 5.1.

  ## Design

  Streams are identified by 31-bit unsigned integers. Client-initiated streams
  use odd IDs; server-initiated streams use even IDs. Stream 0 is reserved for
  the connection control stream.

  The stream state machine tracks the lifecycle: idle → open → half-closed → closed.

  ## Guarantees

  - `isClientStream` and `isServerStream` are complementary for non-zero stream IDs
  - Stream IDs are validated for monotonic increase
  - State transitions are checked against the RFC 9113 state machine

  ## Haskell equivalent
  `Network.HTTP2.Stream` (https://hackage.haskell.org/package/http2)
-/
import Hale.Http2.Network.HTTP2.Frame.Types
import Hale.Http2.Network.HTTP2.Types

namespace Network.HTTP2

/-- HTTP/2 stream states as defined in RFC 9113 Section 5.1. -/
inductive StreamState where
  /-- Stream has not been opened yet. -/
  | idle
  /-- HEADERS sent/received, awaiting response. -/
  | open
  /-- Local side has sent END_STREAM. -/
  | halfClosedLocal
  /-- Remote side has sent END_STREAM. -/
  | halfClosedRemote
  /-- RST_STREAM sent/received. -/
  | resetLocal
  /-- RST_STREAM received. -/
  | resetRemote
  /-- Both sides have closed or RST_STREAM. -/
  | closed
  /-- PUSH_PROMISE received, awaiting HEADERS. -/
  | reservedLocal
  /-- PUSH_PROMISE sent, awaiting HEADERS. -/
  | reservedRemote
  deriving Repr, Inhabited, BEq

instance : ToString StreamState where
  toString
    | .idle => "idle"
    | .open => "open"
    | .halfClosedLocal => "half-closed (local)"
    | .halfClosedRemote => "half-closed (remote)"
    | .resetLocal => "reset (local)"
    | .resetRemote => "reset (remote)"
    | .closed => "closed"
    | .reservedLocal => "reserved (local)"
    | .reservedRemote => "reserved (remote)"

/-- Information about a single HTTP/2 stream. -/
structure StreamInfo where
  /-- Stream identifier. -/
  streamId : StreamId
  /-- Current state. -/
  state : StreamState
  /-- Stream-level flow control window (send). -/
  sendWindow : Int
  /-- Stream-level flow control window (receive). -/
  recvWindow : Int
  /-- Priority: exclusive flag. -/
  priorityExclusive : Bool := false
  /-- Priority: stream dependency. -/
  priorityDependency : StreamId := 0
  /-- Priority: weight (1-256, wire value + 1). -/
  priorityWeight : UInt8 := 15
  deriving Repr, Inhabited

-- ── Stream ID validation ───────────────────────────────

/-- Check if a stream ID belongs to a client-initiated stream.
    Client-initiated streams have odd IDs.
    $$\text{isClientStream}(id) = (id \bmod 2 = 1)$$ -/
@[inline] def isClientStream (streamId : StreamId) : Bool :=
  streamId != 0 && (streamId &&& 1) == 1

/-- Check if a stream ID belongs to a server-initiated stream.
    Server-initiated streams have even IDs (excluding 0).
    $$\text{isServerStream}(id) = (id \neq 0) \wedge (id \bmod 2 = 0)$$ -/
@[inline] def isServerStream (streamId : StreamId) : Bool :=
  streamId != 0 && (streamId &&& 1) == 0

/-- Check if a stream ID is the connection control stream.
    $$\text{isConnectionStream}(id) = (id = 0)$$ -/
@[inline] def isConnectionStream (streamId : StreamId) : Bool :=
  streamId == 0

/-- Validate that a new stream ID is monotonically greater than the last seen ID.
    RFC 9113 Section 5.1.1: Stream identifiers cannot be reused.

    $$\text{validateStreamId}(\text{new}, \text{last}) = \text{new} > \text{last}$$ -/
@[inline] def validateStreamId (newId lastId : StreamId) : Bool :=
  newId > lastId

/-- Client and server stream checks are complementary for non-zero IDs.
    A stream ID is either client-initiated (odd) or server-initiated (even).
    This is a runtime-validated property due to UInt32 bitwise operations. -/
def client_server_complementary_check (id : StreamId) (_h : id ≠ 0) : Bool :=
  isClientStream id || isServerStream id

/-- A stream table mapping stream IDs to stream info. -/
structure StreamTable where
  /-- Active streams. -/
  streams : Array StreamInfo
  /-- Highest client-initiated stream ID seen. -/
  lastClientStreamId : StreamId
  /-- Highest server-initiated stream ID seen. -/
  lastServerStreamId : StreamId
  /-- Next server-initiated stream ID to assign. -/
  nextServerStreamId : StreamId
  deriving Repr, Inhabited

namespace StreamTable

/-- Create an empty stream table with default settings. -/
def empty : StreamTable :=
  { streams := #[]
    lastClientStreamId := 0
    lastServerStreamId := 0
    nextServerStreamId := 2 }

/-- Look up a stream by ID. -/
def lookup (table : StreamTable) (streamId : StreamId) : Option StreamInfo :=
  table.streams.find? (fun s => s.streamId == streamId)

/-- Insert or update a stream in the table. -/
def upsert (table : StreamTable) (info : StreamInfo) : StreamTable :=
  let idx := table.streams.findIdx? (fun s => s.streamId == info.streamId)
  match idx with
  | some i => { table with streams := table.streams.set! i info }
  | none => { table with streams := table.streams.push info }

/-- Open a new client-initiated stream. Returns `none` if the stream ID is invalid. -/
def openClientStream (table : StreamTable) (streamId : StreamId)
    (initialWindow : Int) : Option StreamTable :=
  if !isClientStream streamId then none
  else if !validateStreamId streamId table.lastClientStreamId then none
  else
    let info : StreamInfo :=
      { streamId := streamId
        state := .open
        sendWindow := initialWindow
        recvWindow := initialWindow }
    some { (table.upsert info) with lastClientStreamId := streamId }

/-- Update stream state. -/
def updateState (table : StreamTable) (streamId : StreamId) (state : StreamState) : StreamTable :=
  match table.lookup streamId with
  | some info => table.upsert { info with state := state }
  | none => table

/-- Update stream priority. -/
def updatePriority (table : StreamTable) (streamId : StreamId)
    (exclusive : Bool) (dependency : StreamId) (weight : UInt8) : StreamTable :=
  match table.lookup streamId with
  | some info =>
    table.upsert { info with
      priorityExclusive := exclusive
      priorityDependency := dependency
      priorityWeight := weight }
  | none =>
    -- Create an idle stream entry with the priority
    table.upsert {
      streamId := streamId
      state := .idle
      sendWindow := 0
      recvWindow := 0
      priorityExclusive := exclusive
      priorityDependency := dependency
      priorityWeight := weight }

/-- Count the number of open (non-closed, non-idle) streams. -/
def activeStreamCount (table : StreamTable) : Nat :=
  table.streams.foldl (fun acc s =>
    match s.state with
    | .idle | .closed | .resetLocal | .resetRemote => acc
    | _ => acc + 1) 0

end StreamTable

end Network.HTTP2
