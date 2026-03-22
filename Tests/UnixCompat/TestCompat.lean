import Hale
import Tests.Harness

open System.Posix Tests

/-
  Coverage:
  - Proofs: None (IO-based)
  - Tested: getFileStatus, fileExist
  - Not covered: closeFd (needs real fd)
-/

namespace TestCompat

def tests : IO (List TestResult) := do
  -- Create temp file
  let path := "/tmp/hale-compat-test.txt"
  IO.FS.writeFile path "test content"
  let exists1 ← fileExist path
  let status ← getFileStatus path
  let exists2 ← fileExist "/tmp/nonexistent-hale-test-file"
  pure [
    check "file exists" exists1
  , check "file not exists" (!exists2)
  , check "is regular file" status.isRegularFile
  , check "not directory" (!status.isDirectory)
  , check "size > 0" (status.size > 0)
  ]

end TestCompat
