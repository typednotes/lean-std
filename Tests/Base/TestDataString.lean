import Hale
import Tests.Harness

open Data.DataString Tests

/-
  Coverage:
  - Proofs: unwords_nil, unwords_singleton
  - Tested: IsString, lines, words, unlines, unwords
  - Not covered: None
-/

namespace TestDataString

def tests : List TestResult :=
  [ -- IsString
    checkEq "IsString String" "hello" (IsString.fromString "hello" : String)
  -- lines
  , checkEq "lines" ["a", "b", "c"] (lines "a\nb\nc")
  , checkEq "lines empty" [""] (lines "")
  -- words
  , checkEq "words" ["hello", "world"] (words "hello world")
  , checkEq "words leading space" ["hello"] (words "  hello  ")
  , checkEq "words empty" ([] : List String) (words "")
  -- unlines
  , checkEq "unlines" "a\nb\n" (unlines ["a", "b"])
  -- unwords
  , checkEq "unwords" "hello world" (unwords ["hello", "world"])
  , checkEq "unwords empty" "" (unwords [])
  -- Proof coverage
  , proofCovered "unwords_nil" "Hale.Base.Data.String"
  ]
end TestDataString
