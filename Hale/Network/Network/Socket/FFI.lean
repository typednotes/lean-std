/-
  Hale.Network.Network.Socket.FFI — C FFI bindings for POSIX sockets

  Low-level extern declarations mapping to the C shim in `ffi/network.c`.
  These are not intended for direct use; see `Network.Socket` for the safe API.

  ## Encoding conventions

  - Pairs are nested: `(A × B × C)` = `(A × (B × C))` = `ctor(0,2,0)[a, ctor(0,2,0)[b, c]]`
  - Lists use `ctor(1,2,0)` for cons, `box(0)` for nil
  - USize values are boxed/unboxed with `lean_box`/`lean_unbox`
  - All functions return `IO` (lean_io_result_mk_ok / lean_io_result_mk_error)
-/

import Hale.Network.Network.Socket.Types

namespace Network.Socket.FFI

open Network.Socket

-- ── Socket creation and management ──

/-- Create a socket.
    $$\text{socketCreate} : \text{UInt8} \to \text{UInt8} \to \text{IO}(\text{USize})$$ -/
@[extern "hale_socket_create"]
opaque socketCreate (domain : UInt8) (socktype : UInt8) : IO USize

/-- Close a socket.
    $$\text{socketClose} : \text{USize} \to \text{IO}(\text{Unit})$$ -/
@[extern "hale_socket_close"]
opaque socketClose (fd : USize) : IO Unit

/-- Bind a socket to an address (IPv4/IPv6 via getaddrinfo).
    $$\text{socketBind} : \text{USize} \to \text{String} \to \text{UInt16} \to \text{IO}(\text{Unit})$$ -/
@[extern "hale_socket_bind"]
opaque socketBind (fd : USize) (host : @& String) (port : UInt16) : IO Unit

/-- Listen for connections.
    $$\text{socketListen} : \text{USize} \to \text{USize} \to \text{IO}(\text{Unit})$$ -/
@[extern "hale_socket_listen"]
opaque socketListen (fd : USize) (backlog : USize) : IO Unit

/-- Accept a connection. Returns nested pair `(client_fd, (remote_host, remote_port))`.
    $$\text{socketAccept} : \text{USize} \to \text{IO}(\text{USize} \times (\text{String} \times \text{USize}))$$

    **Fix**: The C code now returns `ctor(0,2,0)[fd, ctor(0,2,0)[host, port]]`
    matching Lean's nested pair encoding, rather than the flat 3-tuple that caused segfaults. -/
@[extern "hale_socket_accept"]
opaque socketAccept (fd : USize) : IO (USize × String × USize)

/-- Connect to a remote address (IPv4/IPv6 via getaddrinfo).
    $$\text{socketConnect} : \text{USize} \to \text{String} \to \text{UInt16} \to \text{IO}(\text{Unit})$$ -/
@[extern "hale_socket_connect"]
opaque socketConnect (fd : USize) (host : @& String) (port : UInt16) : IO Unit

-- ── Send / Recv (TCP) ──

/-- Send data. Returns bytes sent.
    $$\text{socketSend} : \text{USize} \to \text{ByteArray} \to \text{IO}(\text{USize})$$ -/
@[extern "hale_socket_send"]
opaque socketSend (fd : USize) (data : @& ByteArray) : IO USize

/-- Receive data. Returns received bytes.
    $$\text{socketRecv} : \text{USize} \to \text{USize} \to \text{IO}(\text{ByteArray})$$ -/
@[extern "hale_socket_recv"]
opaque socketRecv (fd : USize) (maxlen : USize) : IO ByteArray

-- ── UDP: sendto / recvfrom ──

/-- Send data to a specific address (UDP).
    $$\text{socketSendTo} : \text{USize} \to \text{ByteArray} \to \text{String} \to \text{UInt16} \to \text{IO}(\text{USize})$$ -/
@[extern "hale_socket_sendto"]
opaque socketSendTo (fd : USize) (data : @& ByteArray) (host : @& String) (port : UInt16) : IO USize

/-- Receive data with sender address (UDP).
    Returns `(data, (host, port))`.
    $$\text{socketRecvFrom} : \text{USize} \to \text{USize} \to \text{IO}(\text{ByteArray} \times (\text{String} \times \text{USize}))$$ -/
@[extern "hale_socket_recvfrom"]
opaque socketRecvFrom (fd : USize) (maxlen : USize) : IO (ByteArray × String × USize)

-- ── Socket options ──

/-- Set SO_REUSEADDR option.
    $$\text{setReuseAddr} : \text{USize} \to \text{UInt8} \to \text{IO}(\text{Unit})$$ -/
@[extern "hale_socket_set_reuseaddr"]
opaque setReuseAddr (fd : USize) (enable : UInt8) : IO Unit

/-- Set TCP_NODELAY option.
    $$\text{setNoDelay} : \text{USize} \to \text{UInt8} \to \text{IO}(\text{Unit})$$ -/
@[extern "hale_socket_set_nodelay"]
opaque setNoDelay (fd : USize) (enable : UInt8) : IO Unit

/-- Set non-blocking mode.
    $$\text{setNonBlocking} : \text{USize} \to \text{UInt8} \to \text{IO}(\text{Unit})$$ -/
@[extern "hale_socket_set_nonblocking"]
opaque setNonBlocking (fd : USize) (enable : UInt8) : IO Unit

/-- Set SO_KEEPALIVE option.
    $$\text{setKeepAlive} : \text{USize} \to \text{UInt8} \to \text{IO}(\text{Unit})$$ -/
@[extern "hale_socket_set_keepalive"]
opaque setKeepAlive (fd : USize) (enable : UInt8) : IO Unit

/-- Set SO_LINGER option.
    $$\text{setLinger} : \text{USize} \to \text{UInt8} \to \text{USize} \to \text{IO}(\text{Unit})$$ -/
@[extern "hale_socket_set_linger"]
opaque setLinger (fd : USize) (enable : UInt8) (seconds : USize) : IO Unit

/-- Set SO_RCVBUF size.
    $$\text{setRecvBuf} : \text{USize} \to \text{USize} \to \text{IO}(\text{Unit})$$ -/
@[extern "hale_socket_set_recvbuf"]
opaque setRecvBuf (fd : USize) (size : USize) : IO Unit

/-- Set SO_SNDBUF size.
    $$\text{setSendBuf} : \text{USize} \to \text{USize} \to \text{IO}(\text{Unit})$$ -/
@[extern "hale_socket_set_sendbuf"]
opaque setSendBuf (fd : USize) (size : USize) : IO Unit

/-- Shutdown a socket (read, write, or both).
    $$\text{socketShutdown} : \text{USize} \to \text{UInt8} \to \text{IO}(\text{Unit})$$ -/
@[extern "hale_socket_shutdown"]
opaque socketShutdown (fd : USize) (how : UInt8) : IO Unit

/-- Get peer address. Returns `(host, port)`.
    $$\text{getPeerName} : \text{USize} \to \text{IO}(\text{String} \times \text{USize})$$ -/
@[extern "hale_socket_getpeername"]
opaque getPeerName (fd : USize) : IO (String × USize)

/-- Get local address. Returns `(host, port)`.
    $$\text{getSockName} : \text{USize} \to \text{IO}(\text{String} \times \text{USize})$$ -/
@[extern "hale_socket_getsockname"]
opaque getSockName (fd : USize) : IO (String × USize)

-- ── DNS resolution ──

/-- Resolve a hostname. Returns list of `(family, (host, port))`.
    $$\text{getAddrInfo} : \text{String} \to \text{String} \to \text{IO}(\text{List}(\text{USize} \times (\text{String} \times \text{USize})))$$ -/
@[extern "hale_getaddrinfo"]
opaque getAddrInfo (node : @& String) (service : @& String) : IO (List (USize × String × USize))

-- ── Event multiplexing (kqueue/epoll) ──

/-- Create an event loop (kqueue on macOS, epoll on Linux).
    $$\text{eventLoopCreate} : \text{IO}(\text{USize})$$ -/
@[extern "hale_event_loop_create"]
opaque eventLoopCreate : IO USize

/-- Register interest in events for a socket fd.
    $$\text{eventLoopAdd} : \text{USize} \to \text{USize} \to \text{USize} \to \text{IO}(\text{Unit})$$ -/
@[extern "hale_event_loop_add"]
opaque eventLoopAdd (loopFd : USize) (socketFd : USize) (events : USize) : IO Unit

/-- Unregister a socket fd from the event loop.
    $$\text{eventLoopDel} : \text{USize} \to \text{USize} \to \text{IO}(\text{Unit})$$ -/
@[extern "hale_event_loop_del"]
opaque eventLoopDel (loopFd : USize) (socketFd : USize) : IO Unit

/-- Wait for events. Returns `List (fd × events)`.
    timeout is in milliseconds; pass a very large value for indefinite blocking.
    $$\text{eventLoopWait} : \text{USize} \to \text{USize} \to \text{IO}(\text{List}(\text{USize} \times \text{USize}))$$ -/
@[extern "hale_event_loop_wait"]
opaque eventLoopWait (loopFd : USize) (timeoutMs : USize) : IO (List (USize × USize))

/-- Close the event loop fd.
    $$\text{eventLoopClose} : \text{USize} \to \text{IO}(\text{Unit})$$ -/
@[extern "hale_event_loop_close"]
opaque eventLoopClose (loopFd : USize) : IO Unit

end Network.Socket.FFI
