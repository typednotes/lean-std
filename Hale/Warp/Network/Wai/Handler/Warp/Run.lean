/-
  Hale.Warp.Network.Wai.Handler.Warp.Run — Accept loop and connection handling

  Ports Haskell's Warp server run loop. Binds a TCP socket, accepts connections,
  and spawns a task per connection to handle HTTP requests.

  ## Design

  - `runSettings` is the main entry point: creates a listening socket,
    runs `settingsBeforeMainLoop`, and enters the accept loop.
  - `acceptLoop` accepts connections and spawns `IO.asTask` for each.
  - `runConnection` handles a single connection: parses the request,
    invokes the application, and sends the response.

  ## Guarantees

  - Server socket is cleaned up via `try/finally`
  - Client sockets are closed after handling via `try/finally`
  - Exception in one connection does not crash the server
-/

import Hale.WAI
import Hale.HttpTypes
import Hale.Network
import Hale.Warp.Network.Wai.Handler.Warp.Settings
import Hale.Warp.Network.Wai.Handler.Warp.Request
import Hale.Warp.Network.Wai.Handler.Warp.Response

namespace Network.Wai.Handler.Warp

open Network.Wai
open Network.Socket

/-- Handle a single HTTP connection.
    Parses the request, invokes the application, and sends the response.
    The client socket is closed after handling.
    $$\text{runConnection} : \text{Socket} \to \text{SockAddr} \to \text{Settings} \to \text{Application} \to \text{IO}(\text{Unit})$$ -/
def runConnection (clientSock : Socket) (remoteAddr : SockAddr)
    (settings : Settings) (app : Application) : IO Unit := do
  try
    let reqOpt ← parseRequest clientSock remoteAddr
    match reqOpt with
    | none => pure ()  -- Malformed request or connection closed
    | some req =>
      let _received ← app req fun resp => do
        sendResponse clientSock settings req resp
      pure ()
  catch e =>
    -- Invoke the exception handler with the remote address
    settings.settingsOnException (some remoteAddr)
    -- Log to stderr as a fallback
    IO.eprintln s!"Warp: connection error from {remoteAddr}: {e}"
  finally
    Network.Socket.close clientSock

/-- Accept loop: continuously accepts connections and spawns tasks.
    Each accepted connection is handled in a separate task via `IO.asTask`.
    $$\text{acceptLoop} : \text{Socket} \to \text{Settings} \to \text{Application} \to \text{IO}(\text{Unit})$$ -/
partial def acceptLoop (serverSock : Socket) (settings : Settings)
    (app : Application) : IO Unit := do
  let (clientSock, remoteAddr) ← Network.Socket.accept serverSock
  -- Spawn connection handler as a background task
  let _task ← IO.asTask (runConnection clientSock remoteAddr settings app)
  -- Continue accepting
  acceptLoop serverSock settings app

/-- Run a WAI application with the given settings.
    Creates a TCP server socket, runs the before-main-loop callback,
    and enters the accept loop.
    $$\text{runSettings} : \text{Settings} \to \text{Application} \to \text{IO}(\text{Unit})$$ -/
def runSettings (settings : Settings) (app : Application) : IO Unit := do
  let serverSock ← Network.Socket.listenTCP
    settings.settingsHost settings.settingsPort settings.settingsBacklog
  try
    settings.settingsBeforeMainLoop
    acceptLoop serverSock settings app
  finally
    Network.Socket.close serverSock

end Network.Wai.Handler.Warp
