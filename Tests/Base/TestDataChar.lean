import Hale.Base.Data.Char
import Tests.Harness

open Data Tests

namespace TestDataChar

def tests : List TestResult :=
  [ -- isSpace
    check "Char' isSpace space" (Char'.isSpace ' ')
  , check "Char' isSpace tab" (Char'.isSpace '\t')
  , check "Char' isSpace not a" (!Char'.isSpace 'a')
  -- isAlphaNum
  , check "Char' isAlphaNum letter" (Char'.isAlphaNum 'Z')
  , check "Char' isAlphaNum digit" (Char'.isAlphaNum '5')
  , check "Char' isAlphaNum not !" (!Char'.isAlphaNum '!')
  -- isAscii
  , check "Char' isAscii 'A'" (Char'.isAscii 'A')
  , check "Char' isAscii NUL" (Char'.isAscii (Char.ofNat 0))
  -- isLatin1
  , check "Char' isLatin1 0xFF" (Char'.isLatin1 (Char.ofNat 255))
  -- isControl
  , check "Char' isControl NUL" (Char'.isControl (Char.ofNat 0))
  , check "Char' isControl DEL" (Char'.isControl (Char.ofNat 127))
  , check "Char' isControl not a" (!Char'.isControl 'a')
  -- isPrint
  , check "Char' isPrint 'a'" (Char'.isPrint 'a')
  , check "Char' isPrint not NUL" (!Char'.isPrint (Char.ofNat 0))
  -- isHexDigit
  , check "Char' isHexDigit '0'" (Char'.isHexDigit '0')
  , check "Char' isHexDigit '9'" (Char'.isHexDigit '9')
  , check "Char' isHexDigit 'a'" (Char'.isHexDigit 'a')
  , check "Char' isHexDigit 'F'" (Char'.isHexDigit 'F')
  , check "Char' isHexDigit not 'g'" (!Char'.isHexDigit 'g')
  -- isOctDigit
  , check "Char' isOctDigit '0'" (Char'.isOctDigit '0')
  , check "Char' isOctDigit '7'" (Char'.isOctDigit '7')
  , check "Char' isOctDigit not '8'" (!Char'.isOctDigit '8')
  -- isAsciiUpper / isAsciiLower
  , check "Char' isAsciiUpper 'A'" (Char'.isAsciiUpper 'A')
  , check "Char' isAsciiUpper 'Z'" (Char'.isAsciiUpper 'Z')
  , check "Char' isAsciiUpper not 'a'" (!Char'.isAsciiUpper 'a')
  , check "Char' isAsciiLower 'a'" (Char'.isAsciiLower 'a')
  , check "Char' isAsciiLower 'z'" (Char'.isAsciiLower 'z')
  , check "Char' isAsciiLower not 'A'" (!Char'.isAsciiLower 'A')
  -- isPunctuation
  , check "Char' isPunctuation '!'" (Char'.isPunctuation '!')
  , check "Char' isPunctuation not 'a'" (!Char'.isPunctuation 'a')
  -- ord / chr roundtrip
  , checkEq "Char' ord 'A'" 65 (Char'.ord 'A')
  , checkEq "Char' chr 65" 'A' (Char'.chr 65)
  , checkEq "Char' ord (chr 48)" 48 (Char'.ord (Char'.chr 48))
  -- digitToInt
  , checkEq "Char' digitToInt '0'" (some 0) (Char'.digitToInt '0')
  , checkEq "Char' digitToInt '9'" (some 9) (Char'.digitToInt '9')
  , checkEq "Char' digitToInt 'a'" (some 10) (Char'.digitToInt 'a')
  , checkEq "Char' digitToInt 'F'" (some 15) (Char'.digitToInt 'F')
  , checkEq "Char' digitToInt 'g'" (none : Option Nat) (Char'.digitToInt 'g')
  -- intToDigit
  , checkEq "Char' intToDigit 0" (some '0') (Char'.intToDigit 0)
  , checkEq "Char' intToDigit 9" (some '9') (Char'.intToDigit 9)
  , checkEq "Char' intToDigit 10" (some 'a') (Char'.intToDigit 10)
  , checkEq "Char' intToDigit 15" (some 'f') (Char'.intToDigit 15)
  , checkEq "Char' intToDigit 16" (none : Option Char) (Char'.intToDigit 16)
  -- Proof coverage
  , proofCovered "Char'.isAlphaNum_iff" "Hale.Base.Data.Char"
  , proofCovered "Char'.isSpace_eq_isWhitespace" "Hale.Base.Data.Char"
  , proofCovered "Char'.ord_eq_toNat" "Hale.Base.Data.Char"
  ]

end TestDataChar
