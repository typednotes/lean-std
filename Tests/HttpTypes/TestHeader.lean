import Hale
import Tests.Harness

open Network.HTTP.Types Tests

namespace TestHeader

def tests : List TestResult :=
  [ checkEq "hContentType original" "Content-Type" hContentType.original
  , check "header name case-insensitive" (hContentType == Data.CI.mk' "content-type")
  , check "hHost eq" (hHost == Data.CI.mk' "HOST")
  , checkEq "hAccept original" "Accept" hAccept.original
  ]

end TestHeader
