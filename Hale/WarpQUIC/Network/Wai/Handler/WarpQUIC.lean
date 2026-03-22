/-
  Hale.WarpQUIC.Network.Wai.Handler.WarpQUIC -- WAI handler over HTTP/3 / QUIC

  Bridges the WAI Application interface with HTTP/3 over QUIC transport.
  Analogous to `warp` for HTTP/1.1+HTTP/2 over TCP, but uses QUIC/HTTP/3.

  ## Design

  `run` creates a QUIC server, accepts connections, processes HTTP/3 streams,
  and dispatches WAI `Request`/`Response` objects to the application.

  The flow is:
  1. Build QUIC `ServerConfig` from `Settings`
  2. Run a QUIC server accept loop
  3. For each connection: open HTTP/3 control stream, send SETTINGS
  4. For each request stream: decode QPACK headers -> build WAI `Request` -> call app -> encode response

  ## Guarantees

  - TLS is mandatory (QUIC always uses TLS 1.3)
  - `Settings.certFile` and `Settings.keyFile` are required (not Option)
  - Server socket cleanup follows try/finally pattern

  ## Haskell equivalent
  `Network.Wai.Handler.WarpQUIC` from the `warp-quic` package
-/

import Hale.QUIC
import Hale.Http3

namespace Network.Wai.Handler.WarpQUIC

open Network.QUIC
open Network.HTTP3

/-- Settings for the WarpQUIC server.
    $$\text{Settings} = \{ \text{port} : \text{UInt16},\; \text{certFile} : \text{String},\; \ldots \}$$ -/
structure Settings where
  /-- Port to listen on. Default: 443 (HTTPS). -/
  port : UInt16 := 443
  /-- Host to bind to. Default: all interfaces. -/
  host : String := "0.0.0.0"
  /-- Path to TLS certificate file. Required for QUIC. -/
  certFile : String
  /-- Path to TLS private key file. Required for QUIC. -/
  keyFile : String
  /-- Maximum concurrent HTTP/3 request streams per connection. Default: 100. -/
  maxConcurrentStreams : Nat := 100
  /-- QPACK maximum dynamic table capacity. Default: 4096. -/
  qpackMaxTableCapacity : Nat := 4096
  /-- QPACK maximum blocked streams. Default: 100. -/
  qpackBlockedStreams : Nat := 100
  /-- Server name for the `server` response header. -/
  serverName : String := "Hale/WarpQUIC"
  /-- Called just before the server starts its accept loop. -/
  beforeMainLoop : IO Unit := pure ()

/-- Default settings (requires cert and key paths).
    $$\text{defaultSettings}(c, k) = \text{Settings}\{ \text{certFile} := c,\; \text{keyFile} := k \}$$ -/
def defaultSettings (certFile keyFile : String) : Settings :=
  { certFile, keyFile }

/-- Build a QUIC `ServerConfig` from WarpQUIC `Settings`.
    $$\text{toQUICConfig} : \text{Settings} \to \text{ServerConfig}$$ -/
def toQUICConfig (settings : Settings) : ServerConfig :=
  { tlsConfig := {
      certFile := some settings.certFile
      keyFile := some settings.keyFile
      alpn := ["h3"]
    }
    transportParams := {
      initialMaxStreamsBidi := settings.maxConcurrentStreams
      initialMaxStreamsUni := settings.maxConcurrentStreams
    }
    host := settings.host
    port := settings.port
  }

/-- Build HTTP/3 settings from WarpQUIC settings.
    $$\text{toH3Settings} : \text{Settings} \to \text{H3Settings}$$ -/
def toH3Settings (settings : Settings) : H3Settings :=
  { qpackMaxTableCapacity := settings.qpackMaxTableCapacity
    qpackBlockedStreams := settings.qpackBlockedStreams
  }

/-- Convert an HTTP/3 request to a simplified WAI-compatible representation.
    In a full implementation, this would build a `Network.Wai.Request`.
    $$\text{h3RequestToHeaders} : \text{H3Request} \to \text{List}(\text{String} \times \text{String})$$ -/
def h3RequestToHeaders (req : H3Request) : List (String × String) :=
  [(":method", req.method),
   (":path", req.path),
   (":scheme", req.scheme),
   (":authority", req.authority)] ++ req.headers

/-- Handle a single QUIC connection by processing HTTP/3 streams.
    $$\text{handleConnection} : \text{Settings} \to \text{Connection} \to \text{IO}(\text{Unit})$$
    Currently delegates to `HTTP3.handleConnection`. -/
def handleConnection (settings : Settings) (conn : Connection)
    (handler : H3Handler) : IO Unit := do
  let h3settings := toH3Settings settings
  Network.HTTP3.handleConnection conn h3settings handler

/-- Run a WAI-like application over HTTP/3 / QUIC.
    $$\text{runH3} : \text{Settings} \to \text{H3Handler} \to \text{IO}(\text{Unit})$$
    This is the main entry point for the WarpQUIC server.
    Creates a QUIC server and dispatches HTTP/3 requests to the handler. -/
def runH3 (settings : Settings) (handler : H3Handler) : IO Unit := do
  let quicConfig := toQUICConfig settings
  settings.beforeMainLoop
  Server.run quicConfig fun conn => do
    handleConnection settings conn handler

/-- Run a QUIC server with a custom QUIC config and an HTTP/3 handler.
    $$\text{runQUIC} : \text{ServerConfig} \to \text{H3Handler} \to \text{IO}(\text{Unit})$$ -/
def runQUIC (quicConfig : ServerConfig) (handler : H3Handler) : IO Unit := do
  Server.run quicConfig fun conn => do
    let h3settings := H3Settings.default
    Network.HTTP3.handleConnection conn h3settings handler

end Network.Wai.Handler.WarpQUIC
