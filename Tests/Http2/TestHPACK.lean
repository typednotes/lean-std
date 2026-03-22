/-
  Tests/Http2/TestHPACK.lean — Tests for HPACK header compression

  ## Coverage

  ### Tested here (runtime tests):
  - Static table lookups
  - Dynamic table insert/eviction/resize
  - Integer encoding/decoding roundtrip
  - String encoding/decoding roundtrip
  - Header list encode/decode roundtrip
  - Combined table lookup (findInTables)
  - Index lookup across static + dynamic

  ### Not yet covered:
  - Huffman encoding/decoding (pass-through implementation)
-/
import Hale
import Tests.Harness

open Network.HTTP2 Network.HTTP2.HPACK Tests

namespace TestHPACK

private def integerRoundtrips : List TestResult :=
  let tests := [(10, 7), (1337, 5), (0, 7), (127, 7), (128, 7), (255, 8)]
  tests.map fun (val, pfx) =>
    let encoded := encodeInteger val pfx
    match decodeInteger encoded 0 pfx with
    | some result => check s!"Integer roundtrip {val}/{pfx}" (result.value == val)
    | none => check s!"Integer roundtrip {val}/{pfx} decode failed" false

private def stringRoundtrips : List TestResult :=
  let strs := ["hello", "", "content-type", "/index.html"]
  strs.map fun s =>
    let encoded := encodeString s
    match decodeString encoded 0 with
    | some result => check s!"String roundtrip \"{s}\"" (result.value == s)
    | none => check s!"String roundtrip \"{s}\" decode failed" false

private def headerRoundtrip : List TestResult :=
  let dt := DynamicTable.empty 4096
  let headers : List HeaderField := [(":method", "GET"), (":path", "/"), (":scheme", "https"), (":authority", "example.com")]
  let (encoded, _dt') := encodeHeaders dt headers
  match decodeHeaders (DynamicTable.empty 4096) encoded with
  | some (decoded, _) =>
    [ check "Header roundtrip count" (decoded.length == 4)
    , check "Header roundtrip first" (decoded.head? == some (":method", "GET"))
    , check "Header roundtrip last" (decoded.getLast? == some (":authority", "example.com"))
    ]
  | none => [check "Header decode failed" false]

def tests : List TestResult :=
  [ -- Static table
    check "Static table size" (staticTableSize == 61)
  , check "Static table index 1" (staticLookup 1 == some (":authority", ""))
  , check "Static table index 2" (staticLookup 2 == some (":method", "GET"))
  , check "Static table index 4" (staticLookup 4 == some (":path", "/"))
  , check "Static table index 8" (staticLookup 8 == some (":status", "200"))
  , check "Static table index 61" (staticLookup 61 == some ("www-authenticate", ""))
  , check "Static table index 0 none" (staticLookup 0 == none)
  , check "Static table index 62 none" (staticLookup 62 == none)

    -- Dynamic table basics
  , check "Dynamic table empty" ((DynamicTable.empty 4096).size == 0)
  , check "Dynamic table insert" (((DynamicTable.empty 4096).insert "content-type" "text/html").size == 1)
  , check "Dynamic table lookup" (((DynamicTable.empty 4096).insert "content-type" "text/html").lookup 0 == some ("content-type", "text/html"))

    -- Dynamic table eviction
  , let dt := DynamicTable.empty 64  -- Very small
    let dt := dt.insert "a" "b"  -- 34 bytes
    check "Dynamic table small insert" (dt.size == 1)
  , let dt := DynamicTable.empty 64
    let dt := dt.insert "a" "b"  -- 34
    let dt := dt.insert "c" "d"  -- 34, evicts first (68 > 64)
    check "Dynamic table eviction" (dt.size == 1)
  , let dt := DynamicTable.empty 64
    let dt := dt.insert "a" "b"
    let dt := dt.insert "c" "d"
    check "Dynamic table eviction keeps newest" (dt.lookup 0 == some ("c", "d"))

    -- Dynamic table resize
  , let dt := (DynamicTable.empty 4096).insert "content-type" "text/html"
    let dt := dt.resize 0
    check "Dynamic table resize to 0" (dt.size == 0)

    -- Entry size
  , checkEq "Entry size host+example.com" 47 (entrySize "host" "example.com")

    -- Combined table lookup
  , check "findInTables :method GET exact"
      (match findInTables (DynamicTable.empty 4096) ":method" "GET" with
       | some (2, true) => true | _ => false)
  , check "findInTables :method POST exact"
      (match findInTables (DynamicTable.empty 4096) ":method" "POST" with
       | some (3, true) => true | _ => false)
  , check "findInTables unknown name"
      (match findInTables (DynamicTable.empty 4096) "x-custom" "value" with
       | none => true | _ => false)

    -- Index lookup across static + dynamic
  , let dt := (DynamicTable.empty 4096).insert "x-custom" "value"
    check "indexLookup static" (indexLookup dt 2 == some (":method", "GET"))
  , let dt := (DynamicTable.empty 4096).insert "x-custom" "value"
    check "indexLookup dynamic" (indexLookup dt 62 == some ("x-custom", "value"))
  ] ++ integerRoundtrips
    ++ stringRoundtrips
    ++ headerRoundtrip

end TestHPACK
