import Hale
import Tests.Harness

open Network.HTTP.Types Tests

namespace TestStatus

def tests : List TestResult :=
  [ checkEq "200 code" 200 status200.statusCode
  , checkEq "200 message" "OK" status200.statusMessage
  , checkEq "404 code" 404 status404.statusCode
  , check "200 == 200" (status200 == ok200)
  , check "200 isSuccessful" status200.isSuccessful
  , check "301 isRedirection" status301.isRedirection
  , check "400 isClientError" status400.isClientError
  , check "500 isServerError" status500.isServerError
  , check "100 isInformational" status100.isInformational
  , check "200 not client error" (!status200.isClientError)
  , checkEq "toString 200" "200 OK" (toString status200)
  , checkEq "toString 404" "404 Not Found" (toString status404)
  ]

end TestStatus
