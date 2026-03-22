/-
  Hale.Http3.Network.HTTP3.QPACK.Encode -- QPACK header encoding

  Encodes HTTP header fields using QPACK (RFC 9204).
  This implementation uses static-table-only encoding (no dynamic table updates).

  ## Design

  QPACK encoding supports three representations:
  1. Indexed field line (static table) -- most compact
  2. Literal field line with name reference (static table)
  3. Literal field line with literal name -- least compact

  This implementation does not use the dynamic table or Huffman encoding,
  making it stateless and suitable for simple HTTP/3 usage.

  ## Guarantees

  - Encoding is deterministic for the same input
  - Static table lookups are used when possible for compactness
  - Encoded output is valid QPACK per RFC 9204

  ## Haskell equivalent
  QPACK encoding from the `http3` package
-/

import Hale.Http3.Network.HTTP3.QPACK.Table

namespace Network.HTTP3.QPACK

/-- Encode a QPACK integer with the given prefix bit width (RFC 9204 Section 4.1.1).
    $$\text{encodeQInt}(n, v) = \text{prefix-encoded integer}$$
    The first byte is OR'd with `firstByteMask` to set prefix bits. -/
def encodeQInt (prefixBits : Nat) (value : Nat) (firstByteMask : UInt8 := 0) : ByteArray := Id.run do
  let maxPrefix := (1 <<< prefixBits) - 1
  if value < maxPrefix then
    return ByteArray.mk #[firstByteMask ||| value.toUInt8]
  else
    let firstByte := firstByteMask ||| maxPrefix.toUInt8
    let mut buf := ByteArray.mk #[firstByte]
    let mut remaining := value - maxPrefix
    while remaining >= 128 do
      buf := buf.push ((remaining % 128 + 128).toUInt8)
      remaining := remaining / 128
    buf := buf.push remaining.toUInt8
    return buf

/-- Encode a string literal without Huffman encoding (RFC 9204 Section 4.1.2).
    The first bit indicates Huffman encoding (0 = no Huffman).
    $$\text{encodeStringLiteral}(s) = \text{0 bit} \| \text{length} \| \text{bytes}$$ -/
def encodeStringLiteral (s : String) : ByteArray :=
  let bytes := s.toUTF8
  let lenEnc := encodeQInt 7 bytes.size 0x00  -- H=0 (no Huffman)
  lenEnc ++ bytes

/-- Encode a list of header fields using QPACK (static-table-only mode).
    Returns the encoded header block (request stream portion).
    The required insert count and delta base are both 0 (static-only mode).
    $$\text{encodeHeaders} : \text{List HeaderField} \to \text{ByteArray}$$ -/
def encodeHeaders (headers : List HeaderField) : ByteArray := Id.run do
  -- Encoded Field Section Prefix: Required Insert Count = 0, Delta Base = 0
  let mut buf := ByteArray.mk #[0x00, 0x00]
  for (name, value) in headers do
    match staticFind name value with
    | some (idx, true) =>
      -- Indexed Field Line (static): 1xxxxxxx with T=1 (static)
      -- Prefix: 1 (indexed) + 1 (static) = top 2 bits = 0b11, 6-bit index
      buf := buf ++ encodeQInt 6 idx 0xC0
    | some (idx, false) =>
      -- Literal Field Line With Name Reference (static)
      -- Prefix: 0101 (4 bits) + N=0, then 4-bit name index
      buf := buf ++ encodeQInt 4 idx 0x50
      buf := buf ++ encodeStringLiteral value
    | none =>
      -- Literal Field Line With Literal Name
      -- Prefix: 0010 (4 bits) + N=0, then 3-bit name
      buf := buf ++ ByteArray.mk #[0x20]
      buf := buf ++ encodeStringLiteral name
      buf := buf ++ encodeStringLiteral value
  return buf

end Network.HTTP3.QPACK
