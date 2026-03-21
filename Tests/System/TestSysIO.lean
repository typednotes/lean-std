import Hale.Base.System.IO
import Tests.Harness

open System.SysIO Tests

namespace TestSysIO

def tests : IO (List TestResult) := do
  let mut results : List TestResult := []

  -- stdin/stdout/stderr exist (they return streams)
  results := results ++ [← checkIO "stdin returns a stream" do
    let _ ← stdin
    pure true]

  results := results ++ [← checkIO "stdout returns a stream" do
    let _ ← stdout
    pure true]

  results := results ++ [← checkIO "stderr returns a stream" do
    let _ ← stderr
    pure true]

  -- hPutStr / hGetContents roundtrip via temp file
  results := results ++ [← checkEqIO "hPutStr/hGetContents roundtrip" "hello world" do
    let tmpPath : System.FilePath := "/tmp/hale_test_sysio.txt"
    -- Write
    withFile tmpPath .writeMode fun h => do
      hPutStr h "hello world"
    -- Read back
    withFile tmpPath .readMode fun h => do
      hGetContents h]

  -- hPutStrLn adds newline
  results := results ++ [← checkEqIO "hPutStrLn adds newline" "line1\nline2\n" do
    let tmpPath : System.FilePath := "/tmp/hale_test_sysio_ln.txt"
    withFile tmpPath .writeMode fun h => do
      hPutStrLn h "line1"
      hPutStrLn h "line2"
    withFile tmpPath .readMode fun h => do
      hGetContents h]

  -- hGetLine reads one line
  results := results ++ [← checkEqIO "hGetLine reads one line" "first\n" do
    let tmpPath : System.FilePath := "/tmp/hale_test_sysio_getline.txt"
    withFile tmpPath .writeMode fun h => do
      hPutStrLn h "first"
      hPutStrLn h "second"
    withFile tmpPath .readMode fun h => do
      hGetLine h]

  -- withFile with append mode
  results := results ++ [← checkEqIO "withFile append mode" "AB" do
    let tmpPath : System.FilePath := "/tmp/hale_test_sysio_append.txt"
    withFile tmpPath .writeMode fun h => hPutStr h "A"
    withFile tmpPath .appendMode fun h => hPutStr h "B"
    withFile tmpPath .readMode fun h => hGetContents h]

  -- IOMode BEq
  results := results ++ [check "IOMode BEq same" (IOMode.readMode == IOMode.readMode)]
  results := results ++ [check "IOMode BEq diff" (IOMode.readMode != IOMode.writeMode)]

  -- toFSMode mapping (verified by pattern matching)
  results := results ++ [check "toFSMode readMode" (match toFSMode .readMode with | .read => true | _ => false)]
  results := results ++ [check "toFSMode writeMode" (match toFSMode .writeMode with | .write => true | _ => false)]
  results := results ++ [check "toFSMode appendMode" (match toFSMode .appendMode with | .append => true | _ => false)]
  results := results ++ [check "toFSMode readWriteMode" (match toFSMode .readWriteMode with | .readWrite => true | _ => false)]

  pure results

end TestSysIO
