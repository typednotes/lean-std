import Hale
import Tests.Harness

open Network.Socket Tests

/-
  Coverage:
  - Proofs: None (IO + FFI based)
  - Tested: socket creation, bind, listen, connect, accept, send, recv, close (loopback)
  - Not covered: IPv6, UDP, getAddrInfo, non-blocking mode
-/

namespace TestSocket

def tests : IO (List TestResult) := do
  -- Test: create a TCP server, connect a client, exchange data on loopback
  let server ← socket .inet .stream
  setReuseAddr server
  bind server ⟨"127.0.0.1", 9876⟩
  listen server 5

  -- Connect client in a separate task
  let clientTask ← IO.asTask do
    let client ← socket .inet .stream
    connect client ⟨"127.0.0.1", 9876⟩
    let _ ← Network.Socket.send client "hello".toUTF8
    let response ← Network.Socket.recv client 1024
    close client
    pure (String.fromUTF8! response)

  -- Accept on server
  let (conn, _remoteAddr) ← accept server
  let data ← Network.Socket.recv conn 1024
  let received := String.fromUTF8! data
  let _ ← Network.Socket.send conn "world".toUTF8
  close conn
  close server

  let clientResponse ← IO.ofExcept clientTask.get

  pure [
    checkEq "server received" "hello" received
  , checkEq "client received" "world" clientResponse
  ]

end TestSocket
