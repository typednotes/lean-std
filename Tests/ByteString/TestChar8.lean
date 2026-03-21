import Hale
import Tests.Harness

open Data.ByteString Tests

namespace TestChar8

def tests : List TestResult :=
  let bs := Char8.pack "Hello"
  [ checkEq "pack length" 5 bs.len
  , checkEq "unpack" "Hello" (Char8.unpack bs)
  , checkEq "pack/unpack roundtrip" "Hello" (Char8.unpack (Char8.pack "Hello"))
  -- Head (Option-based for runtime values)
  , checkEq "head?" (some 'H') (Char8.head? bs)
  , checkEq "head? empty" none (Char8.head? ByteString.empty)
  -- Map
  , checkEq "map (+1)" "Ifmmp"
    (Char8.unpack (Char8.map (fun c => Char.ofNat (c.toNat + 1)) bs))
  -- Elem
  , check "elem 'l'" (Char8.elem 'l' bs)
  , check "not elem 'z'" (!(Char8.elem 'z' bs))
  -- Filter
  , checkEq "filter vowels" "eo"
    (Char8.unpack (Char8.filter (fun c => "aeiou".toList.contains c) bs))
  -- Lines
  , let multiline := Char8.pack "line1\nline2\nline3"
    checkEq "lines count" 3 (Char8.lines multiline).length
  -- Words
  , let sentence := Char8.pack "hello world foo"
    checkEq "words count" 3 (Char8.words sentence).length
  ]

end TestChar8
