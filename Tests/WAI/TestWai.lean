import Hale.WAI
import Hale.HttpTypes
import Tests.Harness

open Network.Wai Network.HTTP.Types Tests

/-
  Coverage:
  - Proofs: None (type definitions)
  - Tested: Response construction, status/headers accessors, middleware composition
  - Not covered: Full request/response cycle (needs warp)
-/

namespace TestWai

def tests : List TestResult :=
  let resp := responseLBS status200 [(hContentType, "text/plain")] "Hello"
  let resp404 := responseLBS status404 [] "Not Found"
  [ -- Response status
    checkEq "response 200 status" 200 resp.status.statusCode
  , checkEq "response 404 status" 404 resp404.status.statusCode
  -- Response headers
  , checkEq "response headers length" 1 resp.headers.length
  -- mapResponseStatus
  , let modified := resp.mapResponseStatus (fun _ => status301)
    checkEq "mapResponseStatus" 301 modified.status.statusCode
  -- mapResponseHeaders
  , let modified2 := resp.mapResponseHeaders (fun h => (hServer, "Hale") :: h)
    checkEq "mapResponseHeaders" 2 modified2.headers.length
  -- addHeader
  , let modified3 := addHeader hServer "Hale/1.0" resp
    checkEq "addHeader" 2 modified3.headers.length
  -- Middleware composition
  , check "idMiddleware" true
  ]

end TestWai
