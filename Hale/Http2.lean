/-
  Hale.Http2 — HTTP/2 protocol implementation for Lean 4

  Re-exports all HTTP/2 sub-modules. Inspired by Haskell's `http2` package,
  with a maximalist approach to typing.

  ## Modules

  - Frame/Types: Frame type definitions, error codes, settings
  - Frame/Encode: Frame serialisation to wire format
  - Frame/Decode: Frame parsing from wire format
  - HPACK/Table: Static and dynamic header compression tables
  - HPACK/Huffman: Huffman coding for HPACK
  - HPACK/Encode: HPACK header encoding
  - HPACK/Decode: HPACK header decoding
  - Types: Connection/stream error types, header block assembly state
  - Stream: Stream state machine and validation
  - FlowControl: Connection and stream flow control windows
  - Server: HTTP/2 server connection handler
-/

import Hale.Http2.Network.HTTP2.Frame.Types
import Hale.Http2.Network.HTTP2.Frame.Encode
import Hale.Http2.Network.HTTP2.Frame.Decode
import Hale.Http2.Network.HTTP2.HPACK.Table
import Hale.Http2.Network.HTTP2.HPACK.Huffman
import Hale.Http2.Network.HTTP2.HPACK.Encode
import Hale.Http2.Network.HTTP2.HPACK.Decode
import Hale.Http2.Network.HTTP2.Types
import Hale.Http2.Network.HTTP2.Stream
import Hale.Http2.Network.HTTP2.FlowControl
import Hale.Http2.Network.HTTP2.Server
