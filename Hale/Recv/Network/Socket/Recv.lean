/-
  Hale.Recv.Network.Socket.Recv — Socket recv returning ByteArray

  Thin wrapper around Network.Socket.recv matching Haskell's `recv` package API.
-/

import Hale.Network

namespace Network.Socket.Recv

open Network.Socket

/-- Receive up to `maxlen` bytes from a socket. Returns empty ByteArray on EOF.
    $$\text{recv} : \text{Socket} \to \mathbb{N} \to \text{IO}(\text{ByteArray})$$ -/
@[inline] def recv (sock : Socket) (maxlen : Nat := 4096) : IO ByteArray :=
  Network.Socket.recv sock maxlen

/-- Receive data as a String (UTF-8 decoded). -/
@[inline] def recvString (sock : Socket) (maxlen : Nat := 4096) : IO String := do
  let data ← recv sock maxlen
  pure (String.fromUTF8! data)

end Network.Socket.Recv
