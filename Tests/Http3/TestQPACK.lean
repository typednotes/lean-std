import Hale.Http3
import Tests.Harness

open Network.HTTP3.QPACK Tests

/-
  Coverage:
  - Proofs in source: (none -- QPACK is all runtime byte manipulation)
  - Tested here: Static table lookup, static table find, QPACK integer
    encode/decode, string literal encode, header encode/decode roundtrip
  - Not covered: Huffman encoding, dynamic table (not implemented)
-/

namespace TestQPACK

/-- Helper: check QPACK integer roundtrip. -/
private def checkQIntRoundtrip (name : String) (pfxBits : Nat) (val : Nat) : TestResult :=
  let encoded := encodeQInt pfxBits val
  match decodeQInt pfxBits encoded 0 with
  | some (decoded, consumed) =>
    check name (decoded == val && consumed == encoded.size)
  | none => check name false

def tests : List TestResult :=
  [ -- Static table
    check "staticTableSize is 99"
      (staticTableSize == 99)
  , check "staticLookup 0 is :authority"
      (staticLookup 0 == some (":authority", ""))
  , check "staticLookup 1 is :path /"
      (staticLookup 1 == some (":path", "/"))
  , check "staticLookup 17 is :method GET"
      (staticLookup 17 == some (":method", "GET"))
  , check "staticLookup 25 is :status 200"
      (staticLookup 25 == some (":status", "200"))
  , check "staticLookup 98 is x-frame-options sameorigin"
      (staticLookup 98 == some ("x-frame-options", "sameorigin"))
  , check "staticLookup 99 is none (out of range)"
      (staticLookup 99 == none)

  -- Static find
  , check "staticFind :method GET is exact match at 17"
      (staticFind ":method" "GET" == some (17, true))
  , check "staticFind :status 200 is exact match at 25"
      (staticFind ":status" "200" == some (25, true))
  , check "staticFind :method PATCH is name-only match"
      (match staticFind ":method" "PATCH" with
       | some (_, false) => true
       | _ => false)
  , check "staticFind nonexistent returns none"
      (staticFind "x-custom" "value" == none)

  -- QPACK integer encode/decode
  , checkQIntRoundtrip "qint 6-bit prefix val=0" 6 0
  , checkQIntRoundtrip "qint 6-bit prefix val=62" 6 62
  , checkQIntRoundtrip "qint 6-bit prefix val=63" 6 63
  , checkQIntRoundtrip "qint 6-bit prefix val=100" 6 100
  , checkQIntRoundtrip "qint 4-bit prefix val=0" 4 0
  , checkQIntRoundtrip "qint 4-bit prefix val=14" 4 14
  , checkQIntRoundtrip "qint 4-bit prefix val=15" 4 15
  , checkQIntRoundtrip "qint 4-bit prefix val=200" 4 200
  , checkQIntRoundtrip "qint 7-bit prefix val=126" 7 126
  , checkQIntRoundtrip "qint 7-bit prefix val=127" 7 127
  , checkQIntRoundtrip "qint 7-bit prefix val=1000" 7 1000

  -- String literal
  , check "encodeStringLiteral empty" (
      let enc := encodeStringLiteral ""
      enc.size == 1 && enc.get! 0 == 0x00)
  , check "decodeStringLiteral roundtrip" (
      let enc := encodeStringLiteral "hello"
      match decodeStringLiteral enc 0 with
      | some (s, consumed) => s == "hello" && consumed == enc.size
      | none => false)

  -- Header encode/decode roundtrip (static-only)
  , check "encode/decode roundtrip: static exact match" (
      let headers := [(":method", "GET"), (":path", "/"), (":scheme", "https")]
      let encoded := encodeHeaders headers
      match decodeHeaders encoded with
      | some decoded => decoded == headers
      | none => false)
  , check "encode/decode roundtrip: static name ref + literal value" (
      let headers := [(":status", "201")]
      let encoded := encodeHeaders headers
      match decodeHeaders encoded with
      | some decoded => decoded == headers
      | none => false)
  , check "encode/decode roundtrip: literal name + value" (
      let headers := [("x-custom-header", "custom-value")]
      let encoded := encodeHeaders headers
      match decodeHeaders encoded with
      | some decoded => decoded == headers
      | none => false)
  , check "encode/decode roundtrip: mixed" (
      let headers := [(":method", "GET"), (":path", "/"), ("x-custom", "val")]
      let encoded := encodeHeaders headers
      match decodeHeaders encoded with
      | some decoded => decoded == headers
      | none => false)
  , check "decode empty header block" (
      let encoded := encodeHeaders []
      match decodeHeaders encoded with
      | some decoded => decoded == []
      | none => false)
  ]

end TestQPACK
