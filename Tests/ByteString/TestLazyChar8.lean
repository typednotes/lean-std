import Hale
import Tests.Harness

open Data.ByteString.Lazy Tests

/-
  Coverage:
  - Tested: pack/unpack, cons, head?, map, filter, foldl, foldr, elem
  - Not covered: None
-/

namespace TestLazyChar8

def tests : List TestResult :=
  let lbs := Char8.pack "Hello"
  [ -- pack/unpack roundtrip
    checkEq "pack/unpack roundtrip" "Hello" (Char8.unpack (Char8.pack "Hello"))
  -- cons
  , checkEq "cons" "xHello" (Char8.unpack (Char8.cons 'x' lbs))
  -- head?
  , checkEq "head?" (some 'H') (Char8.head? lbs)
  , checkEq "head? empty" (none : Option Char) (Char8.head? (Char8.pack ""))
  -- map
  , checkEq "map (+1)" "Ifmmp"
      (Char8.unpack (Char8.map (fun c => Char.ofNat (c.toNat + 1)) lbs))
  -- filter
  , checkEq "filter vowels" "eo"
      (Char8.unpack (Char8.filter (fun c => "aeiou".toList.contains c) lbs))
  -- foldl (count characters)
  , checkEq "foldl length" 5 (Char8.foldl (fun acc _ => acc + 1) 0 lbs)
  -- foldr (build string)
  , checkEq "foldr length" 5
      (Char8.foldr (fun _ acc => acc + 1) 0 lbs)
  -- elem
  , check "elem 'l'" (Char8.elem 'l' lbs)
  , check "not elem 'z'" (!(Char8.elem 'z' lbs))
  ]
end TestLazyChar8
