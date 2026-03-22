import Hale.Word8
import Tests.Harness

open Data.Word8 Tests

/-
  Coverage:
  - Proofs: toLower_idempotent, toUpper_idempotent, isUpper_toLower, isLower_toUpper (in source)
  - Tested: isUpper, isLower, isAlpha, isDigit, isAlphaNum, isSpace, isControl, isPrint,
            isHexDigit, isOctDigit, isAscii, toLower, toUpper, constants
  - Not covered: None
-/

namespace TestWord8

def tests : List TestResult :=
  [ -- Classification: isUpper
    check "isUpper 'A'" (isUpper 65)
  , check "isUpper 'Z'" (isUpper 90)
  , check "!isUpper 'a'" (!isUpper 97)
  , check "!isUpper '0'" (!isUpper 48)
  -- Classification: isLower
  , check "isLower 'a'" (isLower 97)
  , check "isLower 'z'" (isLower 122)
  , check "!isLower 'A'" (!isLower 65)
  , check "!isLower '0'" (!isLower 48)
  -- Classification: isAlpha
  , check "isAlpha 'Z'" (isAlpha 90)
  , check "isAlpha 'a'" (isAlpha 97)
  , check "!isAlpha '0'" (!isAlpha 48)
  -- Classification: isDigit
  , check "isDigit '0'" (isDigit 48)
  , check "isDigit '9'" (isDigit 57)
  , check "!isDigit 'A'" (!isDigit 65)
  -- Classification: isAlphaNum
  , check "isAlphaNum 'A'" (isAlphaNum 65)
  , check "isAlphaNum '5'" (isAlphaNum 53)
  , check "!isAlphaNum ' '" (!isAlphaNum 32)
  -- Classification: isSpace
  , check "isSpace ' '" (isSpace 32)
  , check "isSpace '\\t'" (isSpace 9)
  , check "isSpace '\\n'" (isSpace 10)
  , check "isSpace '\\r'" (isSpace 13)
  , check "isSpace '\\v'" (isSpace 11)
  , check "isSpace '\\f'" (isSpace 12)
  , check "!isSpace 'A'" (!isSpace 65)
  -- Classification: isControl
  , check "isControl NUL" (isControl 0)
  , check "isControl DEL" (isControl 127)
  , check "!isControl ' '" (!isControl 32)
  -- Classification: isPrint
  , check "isPrint ' '" (isPrint 32)
  , check "isPrint '~'" (isPrint 126)
  , check "!isPrint NUL" (!isPrint 0)
  , check "!isPrint DEL" (!isPrint 127)
  -- Classification: isHexDigit
  , check "isHexDigit '0'" (isHexDigit 48)
  , check "isHexDigit 'a'" (isHexDigit 97)
  , check "isHexDigit 'F'" (isHexDigit 70)
  , check "!isHexDigit 'G'" (!isHexDigit 71)
  -- Classification: isOctDigit
  , check "isOctDigit '0'" (isOctDigit 48)
  , check "isOctDigit '7'" (isOctDigit 55)
  , check "!isOctDigit '8'" (!isOctDigit 56)
  -- Classification: isAscii
  , check "isAscii 0" (isAscii 0)
  , check "isAscii 127" (isAscii 127)
  , check "!isAscii 128" (!isAscii 128)
  -- Conversion: toLower
  , checkEq "toLower 'A'" 97 (toLower 65)
  , checkEq "toLower 'Z'" 122 (toLower 90)
  , checkEq "toLower 'a'" 97 (toLower 97)
  , checkEq "toLower '0'" 48 (toLower 48)
  -- Conversion: toUpper
  , checkEq "toUpper 'a'" 65 (toUpper 97)
  , checkEq "toUpper 'z'" 90 (toUpper 122)
  , checkEq "toUpper 'A'" 65 (toUpper 65)
  , checkEq "toUpper '0'" 48 (toUpper 48)
  -- Constants
  , checkEq "_nul" 0 _nul
  , checkEq "_tab" 9 _tab
  , checkEq "_lf" 10 _lf
  , checkEq "_vt" 11 _vt
  , checkEq "_ff" 12 _ff
  , checkEq "_cr" 13 _cr
  , checkEq "_space" 32 _space
  , checkEq "_exclam" 33 _exclam
  , checkEq "_quotedbl" 34 _quotedbl
  , checkEq "_numbersign" 35 _numbersign
  , checkEq "_dollar" 36 _dollar
  , checkEq "_percent" 37 _percent
  , checkEq "_ampersand" 38 _ampersand
  , checkEq "_quotesingle" 39 _quotesingle
  , checkEq "_parenleft" 40 _parenleft
  , checkEq "_parenright" 41 _parenright
  , checkEq "_asterisk" 42 _asterisk
  , checkEq "_plus" 43 _plus
  , checkEq "_comma" 44 _comma
  , checkEq "_hyphen" 45 _hyphen
  , checkEq "_period" 46 _period
  , checkEq "_slash" 47 _slash
  , checkEq "_0" 48 _0
  , checkEq "_9" 57 _9
  , checkEq "_colon" 58 _colon
  , checkEq "_semicolon" 59 _semicolon
  , checkEq "_less" 60 _less
  , checkEq "_equal" 61 _equal
  , checkEq "_greater" 62 _greater
  , checkEq "_question" 63 _question
  , checkEq "_at" 64 _at
  , checkEq "_A" 65 _A
  , checkEq "_Z" 90 _Z
  , checkEq "_bracketleft" 91 _bracketleft
  , checkEq "_backslash" 92 _backslash
  , checkEq "_bracketright" 93 _bracketright
  , checkEq "_circum" 94 _circum
  , checkEq "_underscore" 95 _underscore
  , checkEq "_grave" 96 _grave
  , checkEq "_a" 97 _a
  , checkEq "_z" 122 _z
  , checkEq "_braceleft" 123 _braceleft
  , checkEq "_bar" 124 _bar
  , checkEq "_braceright" 125 _braceright
  , checkEq "_tilde" 126 _tilde
  , checkEq "_del" 127 _del
  -- Proof coverage
  , proofCovered "toLower idempotent" "Data.Word8.toLower_idempotent"
  , proofCovered "toUpper idempotent" "Data.Word8.toUpper_idempotent"
  , proofCovered "isUpper -> isLower after toLower" "Data.Word8.isUpper_toLower"
  , proofCovered "isLower -> isUpper after toUpper" "Data.Word8.isLower_toUpper"
  ]

end TestWord8
