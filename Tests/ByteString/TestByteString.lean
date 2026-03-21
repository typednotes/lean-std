import Hale
import Tests.Harness

open Data.ByteString Tests

namespace TestByteString

def tests : List TestResult :=
  let bs := ByteString.pack [72, 101, 108, 108, 111]  -- "Hello"
  let empty := ByteString.empty
  [ -- Construction
    check "empty is null" empty.null
  , check "singleton not null" (!(ByteString.singleton 42).null)
  , checkEq "pack length" 5 bs.len
  , checkEq "unpack roundtrip" [72, 101, 108, 108, 111] bs.unpack
  -- Head/tail (Option-based for runtime values)
  , checkEq "head?" (some 72) bs.head?
  , checkEq "head? empty" none empty.head?
  , check "uncons some" bs.uncons.isSome
  , check "uncons empty" empty.uncons.isNone
  , checkEq "tail length" 4
    (match bs.uncons with | some (_, t) => t.len | none => 0)
  -- O(1) slicing
  , checkEq "take 3 length" 3 (bs.take 3).len
  , checkEq "drop 2 length" 3 (bs.drop 2).len
  , checkEq "take 3 unpack" [72, 101, 108] (bs.take 3).unpack
  , checkEq "drop 2 unpack" [108, 108, 111] (bs.drop 2).unpack
  , checkEq "splitAt fst" [72, 101] (bs.splitAt 2).1.unpack
  , checkEq "splitAt snd" [108, 108, 111] (bs.splitAt 2).2.unpack
  -- Transform
  , checkEq "map (+1)" [73, 102, 109, 109, 112] (bs.map (· + 1)).unpack
  , checkEq "reverse" [111, 108, 108, 101, 72] bs.reverse.unpack
  , checkEq "cons" [42, 72, 101, 108, 108, 111] (ByteString.cons 42 bs).unpack
  , checkEq "snoc" [72, 101, 108, 108, 111, 42] (bs.snoc 42).unpack
  -- Append
  , checkEq "append length" 10 (bs ++ bs).len
  , checkEq "empty append" 5 (empty ++ bs).len
  -- Folds
  , checkEq "foldl sum" 500 (bs.foldl (fun acc w => acc + w.toNat) 0)
  , check "any (== 108)" (bs.any (· == 108))
  , check "all (> 50)" (bs.all (· > 50))
  , check "not all (> 110)" (!(bs.all (· > 110)))
  , checkEq "count 108" 2 (bs.count 108)
  -- Search
  , check "elem 108" (bs.elem 108)
  , check "notElem 42" (bs.notElem 42)
  , checkEq "elemIndex 108" (some 2) (bs.elemIndex 108)
  , checkEq "elemIndices 108" [2, 3] (bs.elemIndices 108)
  , checkEq "findIndex (> 105)" (some 2) (bs.findIndex (· > 105))
  -- Filter/partition
  , checkEq "filter (> 105)" [108, 108, 111] (bs.filter (· > 105)).unpack
  -- Prefix/suffix
  , check "isPrefixOf" (ByteString.isPrefixOf (ByteString.pack [72, 101]) bs)
  , check "isSuffixOf" (ByteString.isSuffixOf (ByteString.pack [108, 111]) bs)
  , check "isInfixOf" (ByteString.isInfixOf (ByteString.pack [101, 108]) bs)
  -- Replicate
  , checkEq "replicate" [42, 42, 42] (ByteString.replicate 3 42).unpack
  -- Intercalate
  , checkEq "intercalate" [1, 0, 2, 0, 3]
    (ByteString.intercalate (ByteString.singleton 0)
      [ByteString.singleton 1, ByteString.singleton 2, ByteString.singleton 3]).unpack
  -- Intersperse
  , checkEq "intersperse" [72, 0, 101, 0, 108, 0, 108, 0, 111]
    (bs.intersperse 0).unpack
  -- BEq
  , check "beq same" (bs == ByteString.pack [72, 101, 108, 108, 111])
  , check "beq diff" (!(bs == ByteString.pack [72, 101]))
  -- Unsnoc
  , check "unsnoc some" bs.unsnoc.isSome
  , check "unsnoc empty" empty.unsnoc.isNone
  -- Copy
  , checkEq "copy" bs.unpack bs.copy.unpack
  -- Scans
  , checkEq "scanl" [0, 72, 173, 25, 133, 244]
    (bs.scanl (· + ·) 0).unpack
  -- foldl1/foldr1 (need proof of non-empty)
  , checkEq "foldl1 max" 111 (bs.foldl1 (fun a b => if a > b then a else b) (by native_decide))
  , checkEq "foldr1 min" 72 (bs.foldr1 (fun a b => if a < b then a else b) (by native_decide))
  -- mapAccumL
  , check "mapAccumL state" ((bs.mapAccumL (fun acc w => (acc + w.toNat, w + 1)) 0).1 == 500)
  , checkEq "mapAccumL mapped" [73, 102, 109, 109, 112]
      (bs.mapAccumL (fun acc w => (acc + w.toNat, w + 1)) 0).2.unpack
  -- mapAccumR
  , check "mapAccumR state" ((bs.mapAccumR (fun acc w => (acc + w.toNat, w)) 0).1 == 500)
  -- concatMap
  , checkEq "concatMap" [72, 72, 101, 101, 108, 108, 108, 108, 111, 111]
      (bs.concatMap (fun w => ByteString.pack [w, w])).unpack
  -- maximum/minimum
  , checkEq "maximum" 111 (bs.maximum (by native_decide))
  , checkEq "minimum" 72 (bs.minimum (by native_decide))
  -- takeWhile/dropWhile/span
  , checkEq "takeWhile" [72, 101] (bs.takeWhile (· < 108)).unpack
  , checkEq "dropWhile" [108, 108, 111] (bs.dropWhile (· < 108)).unpack
  , let (tw, dw) := bs.span (· < 108)
    check "span" (tw.unpack == [72, 101] && dw.unpack == [108, 108, 111])
  -- group/groupBy
  , checkEq "group count" 4 (bs.group).length
  , checkEq "groupBy count" 4 (bs.groupBy (· == ·)).length
  -- inits/tails
  , checkEq "inits count" 6 (bs.inits).length
  , checkEq "tails count" 6 (bs.tails).length
  -- stripPrefix/stripSuffix
  , check "stripPrefix some" (ByteString.stripPrefix (ByteString.pack [72, 101]) bs).isSome
  , check "stripPrefix none" (ByteString.stripPrefix (ByteString.pack [99, 99]) bs).isNone
  , check "stripSuffix some" (ByteString.stripSuffix (ByteString.pack [108, 111]) bs).isSome
  , check "stripSuffix none" (ByteString.stripSuffix (ByteString.pack [99, 99]) bs).isNone
  -- transpose
  , checkEq "transpose" 3
      (ByteString.transpose [ByteString.pack [1, 2, 3], ByteString.pack [4, 5, 6]]).length
  -- scanr/scanl1/scanr1
  , checkEq "scanl1 length" 5 (bs.scanl1 (· + ·) (by native_decide)).len
  , checkEq "scanr length" 6 (bs.scanr (· + ·) 0).len
  , checkEq "scanr1 length" 5 (bs.scanr1 (· + ·) (by native_decide)).len
  -- partition content
  , let (yes, no) := bs.partition (· > 105)
    check "partition" (yes.unpack == [108, 108, 111] && no.unpack == [72, 101])
  ]

end TestByteString
