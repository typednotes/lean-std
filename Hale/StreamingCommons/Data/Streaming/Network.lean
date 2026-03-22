/-
  Hale.StreamingCommons.Data.Streaming.Network — Streaming network utilities

  Thin wrappers around the Network socket API for common patterns.
  Mirrors Haskell's `Data.Streaming.Network`.
-/

import Hale.Network

namespace Data.Streaming.Network

open Network.Socket

/-- Application data for a connected client. -/
structure AppData where
  /-- Read data from the connection. -/
  appRead : IO ByteArray
  /-- Write data to the connection. -/
  appWrite : ByteArray → IO Unit
  /-- The client's address. -/
  appSockAddr : SockAddr
  /-- Close the connection. -/
  appClose : IO Unit

/-- Bind a TCP server socket to a port.
    $$\text{bindPortTCP} : \text{UInt16} \to \text{String} \to \text{IO}(\text{Socket})$$ -/
def bindPortTCP (port : UInt16) (host : String := "0.0.0.0") : IO Socket :=
  listenTCP host port

/-- Connect to a remote TCP server.
    $$\text{getSocketTCP} : \text{String} \to \text{UInt16} \to \text{IO}(\text{Socket} \times \text{SockAddr})$$ -/
def getSocketTCP (host : String) (port : UInt16) : IO (Socket × SockAddr) := do
  let s ← socket .inet .stream
  connect s ⟨host, port⟩
  pure (s, ⟨host, port⟩)

/-- Accept a connection, retrying on transient errors.
    $$\text{acceptSafe} : \text{Socket} \to \text{IO}(\text{Socket} \times \text{SockAddr})$$ -/
partial def acceptSafe (serverSock : Socket) : IO (Socket × SockAddr) := do
  try
    accept serverSock
  catch _ =>
    IO.sleep 10
    acceptSafe serverSock

/-- Create AppData from a connected socket. -/
def mkAppData (clientSock : Socket) (addr : SockAddr) : AppData :=
  { appRead := recv clientSock 4096
  , appWrite := fun data => do let _ ← send clientSock data; pure ()
  , appSockAddr := addr
  , appClose := close clientSock }

/-- Run a TCP server: accept connections and handle each in a new task.
    $$\text{runTCPServer} : \text{UInt16} \to (\text{AppData} \to \text{IO}(\text{Unit})) \to \text{IO}(\text{Unit})$$ -/
def runTCPServer (port : UInt16) (handler : AppData → IO Unit) : IO Unit := do
  let server ← bindPortTCP port
  try
    while true do
      let (client, addr) ← acceptSafe server
      let appData := mkAppData client addr
      let _task ← IO.asTask do
        try handler appData catch _ => pure ()
        appData.appClose
  finally
    close server

end Data.Streaming.Network
