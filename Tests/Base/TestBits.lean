import Hale.Base.Data.Bits
import Tests.Harness

open Data Tests

namespace TestBits

def tests : List TestResult :=
  [ -- UInt8 basic operations
    checkEq "Bits UInt8 and" (0x0A : UInt8) (Bits.and 0x0F 0x1A)
  , checkEq "Bits UInt8 or" (0x1F : UInt8) (Bits.or 0x0F 0x1A)
  , checkEq "Bits UInt8 xor" (0x15 : UInt8) (Bits.xor 0x0F 0x1A)
  , checkEq "Bits UInt8 complement 0" (0xFF : UInt8) (Bits.complement (0x00 : UInt8))
  -- shifts
  , checkEq "Bits UInt8 shiftL" (0x14 : UInt8) (Bits.shiftL (0x05 : UInt8) 2)
  , checkEq "Bits UInt8 shiftR" (0x02 : UInt8) (Bits.shiftR (0x0A : UInt8) 2)
  -- testBit
  , check "Bits UInt8 testBit 0 of 1" (Bits.testBit (1 : UInt8) 0)
  , check "Bits UInt8 testBit 1 of 2" (Bits.testBit (2 : UInt8) 1)
  , check "Bits UInt8 testBit 0 of 2 false" (!Bits.testBit (2 : UInt8) 0)
  -- bit
  , checkEq "Bits UInt8 bit 0" (1 : UInt8) (Bits.bit 0)
  , checkEq "Bits UInt8 bit 3" (8 : UInt8) (Bits.bit 3)
  , checkEq "Bits UInt8 bit 7" (128 : UInt8) (Bits.bit 7)
  -- popCount
  , checkEq "Bits UInt8 popCount 0" 0 (Bits.popCount (0 : UInt8))
  , checkEq "Bits UInt8 popCount 0xFF" 8 (Bits.popCount (0xFF : UInt8))
  , checkEq "Bits UInt8 popCount 0x0F" 4 (Bits.popCount (0x0F : UInt8))
  , checkEq "Bits UInt8 popCount 1" 1 (Bits.popCount (1 : UInt8))
  -- zeroBits
  , checkEq "Bits UInt8 zeroBits" (0 : UInt8) (Bits.zeroBits)
  -- bitSizeMaybe
  , checkEq "Bits UInt8 bitSizeMaybe" (some 8) (Bits.bitSizeMaybe (α := UInt8))
  -- Identity: x AND (complement x) = zeroBits
  , checkEq "Bits UInt8 x and complement x = 0" (0 : UInt8)
      (Bits.and (0xAB : UInt8) (Bits.complement 0xAB))
  -- Identity: x OR zeroBits = x
  , checkEq "Bits UInt8 x or zeroBits = x" (0xAB : UInt8)
      (Bits.or (0xAB : UInt8) (Bits.zeroBits))
  -- FiniteBits
  , checkEq "FiniteBits UInt8 finiteBitSize" 8 (FiniteBits.finiteBitSize (α := UInt8))
  , checkEq "FiniteBits UInt8 countLeadingZeros 1" 7 (FiniteBits.countLeadingZeros (1 : UInt8))
  , checkEq "FiniteBits UInt8 countTrailingZeros 2" 1 (FiniteBits.countTrailingZeros (2 : UInt8))
  , checkEq "FiniteBits UInt8 countLeadingZeros 0" 8 (FiniteBits.countLeadingZeros (0 : UInt8))
  , checkEq "FiniteBits UInt8 countTrailingZeros 0" 8 (FiniteBits.countTrailingZeros (0 : UInt8))
  -- UInt32 spot check
  , checkEq "Bits UInt32 bitSizeMaybe" (some 32) (Bits.bitSizeMaybe (α := UInt32))
  , checkEq "Bits UInt32 popCount 0xFFFF" 16 (Bits.popCount (0xFFFF : UInt32))
  -- UInt64 spot check
  , checkEq "Bits UInt64 bitSizeMaybe" (some 64) (Bits.bitSizeMaybe (α := UInt64))
  -- Derived: setBit, clearBit, complementBit
  , checkEq "Bits setBit UInt8" (3 : UInt8) (Bits.setBit (1 : UInt8) 1)
  , checkEq "Bits clearBit UInt8" (1 : UInt8) (Bits.clearBit (3 : UInt8) 1)
  , checkEq "Bits complementBit UInt8" (3 : UInt8) (Bits.complementBit (1 : UInt8) 1)
  ]

end TestBits
