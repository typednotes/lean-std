import Hale
import Tests.Harness

open Network.HTTP.Types Tests

namespace TestMethod

def tests : List TestResult :=
  [ checkEq "GET toString" "GET" (toString StdMethod.GET)
  , checkEq "POST toString" "POST" (toString StdMethod.POST)
  , checkEq "PATCH toString" "PATCH" (toString StdMethod.PATCH)
  , check "parseMethod GET" (parseMethod "GET" == .standard .GET)
  , check "parseMethod POST" (parseMethod "POST" == .standard .POST)
  , check "parseMethod custom" (parseMethod "PURGE" == .custom "PURGE")
  , checkEq "renderMethod GET" "GET" (renderMethod (.standard .GET))
  , checkEq "renderMethod custom" "PURGE" (renderMethod (.custom "PURGE"))
  ]

end TestMethod
