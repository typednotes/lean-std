import Hale
import Tests.Harness

open Data.ByteString Tests

namespace TestShort

def tests : List TestResult :=
  let sbs := ShortByteString.pack [1, 2, 3, 4, 5]
  [ check "empty is null" ShortByteString.empty.null
  , checkEq "pack length" 5 sbs.length
  , checkEq "unpack roundtrip" [1, 2, 3, 4, 5] sbs.unpack
  , checkEq "index 0" 1 (sbs.index 0 (by native_decide))
  , checkEq "index 4" 5 (sbs.index 4 (by native_decide))
  -- Conversion roundtrip
  , let bs := ShortByteString.fromShort sbs
    checkEq "fromShort length" 5 bs.len
  , let bs := ByteString.pack [10, 20, 30]
    let sbs' := ShortByteString.toShort bs
    checkEq "toShort length" 3 sbs'.length
  , let bs := ByteString.pack [10, 20, 30]
    checkEq "toShort/fromShort roundtrip" bs.unpack
      (ShortByteString.fromShort (ShortByteString.toShort bs)).unpack
  -- BEq
  , check "ShortByteString BEq same" (sbs == ShortByteString.pack [1, 2, 3, 4, 5])
  , check "ShortByteString BEq diff" (!(sbs == ShortByteString.pack [1, 2, 3]))
  -- Empty edge cases
  , checkEq "empty unpack" ([] : List UInt8) ShortByteString.empty.unpack
  , checkEq "empty length" 0 ShortByteString.empty.length
  ]

end TestShort
