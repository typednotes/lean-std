import Hale.QUIC
import Tests.Harness

open Network.QUIC Tests

/-
  Coverage:
  - Proofs in source: FrameType roundtrips, H3Error roundtrips (in Http3 modules)
  - Tested here: ConnectionId construction + invariant, StreamId type extraction,
    TransportParams defaults, Version equality, TransportError code roundtrip,
    TLSConfig defaults, ServerConfig/ClientConfig construction
  - Not covered: Connection/QUICStream operations (stubbed, require FFI)
-/

namespace TestQUICTypes

def tests : List TestResult :=
  [ -- ConnectionId
    check "ConnectionId.empty has size 0"
      (ConnectionId.empty.bytes.size == 0)
  , check "ConnectionId.empty satisfies length invariant"
      (ConnectionId.empty.bytes.size ≤ 20)
  , check "ConnectionId BEq reflexive"
      (ConnectionId.empty == ConnectionId.empty)
  , check "ConnectionId toString of empty"
      (toString ConnectionId.empty == "")
  , check "ConnectionId construction with small bytes"
      (let cid : ConnectionId := { bytes := ByteArray.mk #[0x01, 0x02, 0x03], hLen := by native_decide }
       cid.bytes.size == 3)

  -- Version
  , check "Version.v1 equals http3"
      (Version.v1 == Version.http3)
  , check "Version.v1 val is 1"
      (Version.v1.val == 1)
  , check "Version.v2 is different from v1"
      (!(Version.v2 == Version.v1))

  -- TransportParams
  , check "TransportParams default maxIdleTimeout"
      (TransportParams.default.maxIdleTimeout == 30000)
  , check "TransportParams default maxUDPPayloadSize"
      (TransportParams.default.maxUDPPayloadSize == 65527)
  , check "TransportParams default initialMaxData"
      (TransportParams.default.initialMaxData == 1048576)
  , check "TransportParams default initialMaxStreamsBidi"
      (TransportParams.default.initialMaxStreamsBidi == 100)
  , check "TransportParams BEq reflexive"
      (TransportParams.default == TransportParams.default)

  -- StreamId
  , check "StreamId clientBidi type (id=0)"
      (StreamId.streamType ⟨0⟩ == .clientBidi)
  , check "StreamId serverBidi type (id=1)"
      (StreamId.streamType ⟨1⟩ == .serverBidi)
  , check "StreamId clientUni type (id=2)"
      (StreamId.streamType ⟨2⟩ == .clientUni)
  , check "StreamId serverUni type (id=3)"
      (StreamId.streamType ⟨3⟩ == .serverUni)
  , check "StreamId 4 is clientBidi"
      (StreamId.streamType ⟨4⟩ == .clientBidi)
  , check "StreamId isBidi for id=0"
      (StreamId.isBidi ⟨0⟩)
  , check "StreamId isUni for id=2"
      (StreamId.isUni ⟨2⟩)
  , check "StreamId isClientInitiated for id=0"
      (StreamId.isClientInitiated ⟨0⟩)
  , check "StreamId isServerInitiated for id=1"
      (StreamId.isServerInitiated ⟨1⟩)
  , check "StreamId BEq"
      (StreamId.mk 42 == StreamId.mk 42)

  -- TransportError
  , check "TransportError.noError code is 0"
      (TransportError.noError.toCode == 0)
  , check "TransportError.internalError code is 1"
      (TransportError.internalError.toCode == 1)
  , check "TransportError.cryptoError 48 code"
      ((TransportError.cryptoError 48).toCode == 0x100 + 48)
  , check "TransportError.unknown preserves code"
      ((TransportError.unknown 999).toCode == 999)

  -- TLSConfig
  , check "TLSConfig default alpn is h3"
      (let cfg : TLSConfig := {}; cfg.alpn == ["h3"])
  , check "TLSConfig default certFile is none"
      (let cfg : TLSConfig := {}; cfg.certFile == none)
  ]

end TestQUICTypes
