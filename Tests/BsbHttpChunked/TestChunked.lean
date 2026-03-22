import Hale
import Tests.Harness

open Network.HTTP.Chunked Tests

/-
  Coverage:
  - Proofs: None
  - Tested: chunkedTransferEncoding, chunkedTransferTerminator, encodeChunked
  - Not covered: None
-/

namespace TestChunked

private def strToBytes (s : String) : ByteArray := s.toUTF8
private def bytesToStr (b : ByteArray) : String := String.fromUTF8! b

def tests : List TestResult :=
  let hello := strToBytes "Hello"
  let world := strToBytes "World!"
  let chunk1 := chunkedTransferEncoding hello
  let term := chunkedTransferTerminator
  [ -- Single chunk encoding
    checkEq "chunk Hello" "5\r\nHello\r\n" (bytesToStr chunk1)
  -- Terminator
  , checkEq "terminator" "0\r\n\r\n" (bytesToStr term)
  -- Empty data produces empty output
  , checkEq "chunk empty" 0 (chunkedTransferEncoding ByteArray.empty).size
  -- Full encoding
  , let full := encodeChunked [hello, world]
    checkEq "encodeChunked" "5\r\nHello\r\n6\r\nWorld!\r\n0\r\n\r\n" (bytesToStr full)
  -- Hex encoding for larger sizes
  , let big := (List.replicate 255 (65 : UInt8)).toByteArray  -- 255 bytes of 'A'
    let encoded := chunkedTransferEncoding big
    -- Should start with "ff\r\n"
    check "hex 255 = ff" (bytesToStr encoded |>.startsWith "ff\r\n")
  ]

end TestChunked
