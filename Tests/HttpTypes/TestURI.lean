import Hale
import Tests.Harness

open Network.HTTP.Types Tests

namespace TestURI

def tests : List TestResult :=
  [ -- parseQuery
    checkEq "parseQuery empty" ([] : Query) (parseQuery "")
  , checkEq "parseQuery simple" [("a", some "1"), ("b", some "2")] (parseQuery "?a=1&b=2")
  , checkEq "parseQuery no ?" [("a", some "1")] (parseQuery "a=1")
  , checkEq "parseQuery no value" [("key", none)] (parseQuery "key")
  -- renderQuery
  , checkEq "renderQuery" "?a=1&b=2" (renderQuery [("a", some "1"), ("b", some "2")])
  , checkEq "renderQuery empty" "" (renderQuery [])
  -- urlEncode
  , checkEq "urlEncode simple" "hello" (urlEncode "hello")
  , check "urlEncode space" (urlEncode "hello world" |>.contains '%')
  -- urlDecode
  , checkEq "urlDecode %20" "hello world" (urlDecode "hello%20world")
  , checkEq "urlDecode +" "hello world" (urlDecode "hello+world")
  -- roundtrip
  , checkEq "urlDecode . urlEncode" "test" (urlDecode (urlEncode "test"))
  ]

end TestURI
