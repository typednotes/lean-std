import Hale
import Tests.Harness

open Data.ByteString Data.ByteString.Lazy Tests

namespace TestLazy

def tests : List TestResult :=
  let bs1 := ByteString.pack [1, 2, 3]
  let bs2 := ByteString.pack [4, 5, 6]
  let lbs := LazyByteString.fromChunks [bs1, bs2]
  [ check "empty is null" LazyByteString.empty.null
  , check "fromChunks not null" (!lbs.null)
  , checkEq "length" 6 lbs.length
  , checkEq "toStrict unpack" [1, 2, 3, 4, 5, 6] lbs.toStrict.unpack
  , checkEq "toChunks count" 2 lbs.toChunks.length
  -- fromStrict / toStrict roundtrip
  , let bs := ByteString.pack [10, 20, 30]
    checkEq "fromStrict/toStrict" bs.unpack
      (LazyByteString.fromStrict bs).toStrict.unpack
  -- Append
  , let lbs2 := LazyByteString.fromChunks [ByteString.pack [7, 8]]
    checkEq "append" [1, 2, 3, 4, 5, 6, 7, 8] (lbs ++ lbs2).toStrict.unpack
  -- Folds
  , checkEq "foldl sum" 21 (lbs.foldl (fun acc w => acc + w.toNat) 0)
  -- Map
  , checkEq "map (*2)" [2, 4, 6, 8, 10, 12] (lbs.map (· * 2)).toStrict.unpack
  -- Take/Drop
  , checkEq "take 4" [1, 2, 3, 4] (lbs.take 4).toStrict.unpack
  , checkEq "drop 2" [3, 4, 5, 6] (lbs.drop 2).toStrict.unpack
  -- Filter
  , checkEq "filter even" [2, 4, 6]
    (lbs.filter (fun w => w % 2 == 0)).toStrict.unpack
  -- Any/All/Elem
  , check "any (== 3)" (lbs.any (· == 3))
  , check "all (< 10)" (lbs.all (· < 10))
  , check "elem 5" (lbs.elem 5)
  -- Pack/Unpack
  , checkEq "pack/unpack" [42, 43, 44] (LazyByteString.pack [42, 43, 44]).unpack
  -- cons/snoc
  , checkEq "cons" [0, 1, 2, 3, 4, 5, 6] (LazyByteString.cons 0 lbs).toStrict.unpack
  , checkEq "snoc" [1, 2, 3, 4, 5, 6, 7] (lbs.snoc 7).toStrict.unpack
  -- head? content
  , checkEq "head?" (some 1) lbs.head?
  , checkEq "head? empty" (none : Option UInt8) LazyByteString.empty.head?
  -- uncons
  , check "uncons some" lbs.uncons.isSome
  , check "uncons empty" LazyByteString.empty.uncons.isNone
  -- reverse
  , checkEq "reverse" [6, 5, 4, 3, 2, 1] lbs.reverse.toStrict.unpack
  -- splitAt content
  , checkEq "splitAt fst" [1, 2, 3] (lbs.splitAt 3).1.toStrict.unpack
  , checkEq "splitAt snd" [4, 5, 6] (lbs.splitAt 3).2.toStrict.unpack
  -- foldr
  , checkEq "foldr" [1, 2, 3, 4, 5, 6]
      (lbs.foldr (fun w acc => w :: acc) [])
  -- foldlChunks/foldrChunks
  , checkEq "foldlChunks count" 2
      (lbs.foldlChunks (fun acc _ => acc + 1) 0)
  , checkEq "foldrChunks count" 2
      (lbs.foldrChunks (fun _ acc => acc + 1) 0)
  ]

end TestLazy
