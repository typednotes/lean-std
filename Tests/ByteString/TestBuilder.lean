import Hale
import Tests.Harness

open Data.ByteString Tests

namespace TestBuilder

def tests : List TestResult :=
  [ -- Empty builder
    checkEq "empty builder" ([] : List UInt8)
      Builder.empty.toStrictByteString.unpack
  -- Singleton
  , checkEq "singleton" [42]
      (Builder.singleton 42).toStrictByteString.unpack
  -- Append
  , checkEq "append" [1, 2, 3]
      (Builder.toStrictByteString
        (Builder.singleton 1 ++ Builder.singleton 2 ++ Builder.singleton 3)).unpack
  -- byteString
  , let bs := ByteString.pack [10, 20, 30]
    checkEq "byteString" [10, 20, 30]
      (Builder.byteString bs).toStrictByteString.unpack
  -- word8
  , checkEq "word8" [255]
      (Builder.word8 255).toStrictByteString.unpack
  -- word16BE
  , checkEq "word16BE 0x0102" [1, 2]
      (Builder.word16BE 0x0102).toStrictByteString.unpack
  -- word16LE
  , checkEq "word16LE 0x0102" [2, 1]
      (Builder.word16LE 0x0102).toStrictByteString.unpack
  -- word32BE
  , checkEq "word32BE 0x01020304" [1, 2, 3, 4]
      (Builder.word32BE 0x01020304).toStrictByteString.unpack
  -- word32LE
  , checkEq "word32LE 0x01020304" [4, 3, 2, 1]
      (Builder.word32LE 0x01020304).toStrictByteString.unpack
  -- stringUtf8
  , checkEq "stringUtf8 'Hi'" [72, 105]
      (Builder.stringUtf8 "Hi").toStrictByteString.unpack
  -- intDec
  , checkEq "intDec 42" [52, 50]  -- '4', '2'
      (Builder.intDec 42).toStrictByteString.unpack
  -- Concatenation of multiple builders
  , checkEq "multi concat" [1, 0, 2, 0, 3]
      (Builder.toStrictByteString
        (Builder.word8 1 ++ Builder.word8 0 ++ Builder.word8 2
          ++ Builder.word8 0 ++ Builder.word8 3)).unpack
  ]

end TestBuilder
