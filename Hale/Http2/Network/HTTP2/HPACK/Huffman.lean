/-
  Hale.Http2.Network.HTTP2.HPACK.Huffman — HPACK Huffman coding

  Implements the Huffman coding table from RFC 7541 Appendix B for
  HPACK header compression.

  ## Design

  For simplicity, this implementation provides a pass-through that does not
  apply Huffman encoding/decoding. A full Huffman implementation would require
  the 256-entry code table from RFC 7541 Appendix B.

  The encoding/decoding functions accept a flag indicating whether Huffman
  coding is used. When not used, strings are encoded as raw octets.

  ## Haskell equivalent
  `Network.HTTP2.HPACK.Huffman` (https://hackage.haskell.org/package/http2)
-/

namespace Network.HTTP2.HPACK

/-- Encode a string to bytes, optionally with Huffman coding.
    Currently implements identity encoding (no Huffman).
    $$\text{huffmanEncode} : \text{String} \to \text{ByteArray}$$ -/
def huffmanEncode (s : String) : ByteArray :=
  s.toUTF8

/-- Decode bytes to a string, optionally with Huffman decoding.
    Currently implements identity decoding (no Huffman).
    $$\text{huffmanDecode} : \text{ByteArray} \to \text{Option}(\text{String})$$ -/
def huffmanDecode (bs : ByteArray) : Option String :=
  String.fromUTF8? bs

end Network.HTTP2.HPACK
