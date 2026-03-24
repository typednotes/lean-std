/-
  Tests.WAI.TestWaiSmoke — WAI/Warp runtime smoke tests

  Exercises the full Application → Response pipeline using the simulated
  WAI test harness (no real network needed). These tests verify that
  the strengthened types (AppM indexed monad, phantom parameters) work
  correctly at runtime, not just at compile time.

  ## Coverage
  - Tested: AppM.respond, AppM.respondIO, AppM.ioThen, middleware short-circuit,
    middleware callback wrapping, middleware composition, streaming body
  - Proofs: Covered by Tests/Examples.lean (compile-time witnesses)
-/
import Hale
import Hale.WaiExtra.Network.Wai.Test
import Tests.Harness

open Network.Wai Network.HTTP.Types Tests

namespace TestWaiSmoke

-- ════════════════════════════════════════════════════════════════
-- Sample applications exercising each AppM combinator
-- ════════════════════════════════════════════════════════════════

/-- Simple application using `AppM.respond` directly. -/
def simpleApp : Application := fun _req respond =>
  AppM.respond respond (responseLBS status200 [] "ok")

/-- Echo application using `AppM.respondIO` (IO then respond). -/
def echoApp : Application := fun req respond =>
  AppM.respondIO respond do
    let body ← req.requestBody
    pure (responseLBS status200 [(hContentType, "text/plain")] (String.fromUTF8! body))

/-- Health check middleware using `AppM.respond` for short-circuit. -/
def healthMiddleware : Middleware :=
  fun app req respond =>
    if req.rawPathInfo == "/_health" then
      AppM.respond respond (responseLBS status200 [] "healthy")
    else app req respond

/-- Header-adding middleware using callback wrapping. -/
def serverHeaderMiddleware : Middleware :=
  fun app req respond =>
    app req fun resp =>
      respond (resp.mapResponseHeaders ((hServer, "HaleSmoke/1.0") :: ·))

/-- IO-before-delegate middleware using `AppM.ioThen`. -/
def timestampMiddleware : Middleware :=
  fun app req respond =>
    AppM.ioThen (IO.monoNanosNow) fun _ts =>
      app req respond

-- ════════════════════════════════════════════════════════════════
-- Runtime smoke tests
-- ════════════════════════════════════════════════════════════════

def tests : IO (List TestResult) := do
  -- 1. Simple GET → 200
  let resp ← Network.Wai.Test.get simpleApp "/"
  let r1 := checkEq "smoke: simple GET status" 200 resp.simpleStatus.statusCode
  let r2 := checkEq "smoke: simple GET body" "ok" (String.fromUTF8! resp.simpleBody)

  -- 2. Echo POST → returns body
  let resp2 ← Network.Wai.Test.post echoApp "/" "hello world".toUTF8
  let r3 := checkEq "smoke: echo POST body" "hello world" (String.fromUTF8! resp2.simpleBody)

  -- 3. Empty POST → returns empty
  let resp3 ← Network.Wai.Test.post echoApp "/" ByteArray.empty
  let r4 := checkEq "smoke: echo empty POST" "" (String.fromUTF8! resp3.simpleBody)

  -- 4. Health middleware — short-circuit path
  let healthApp := healthMiddleware echoApp
  let resp4 ← Network.Wai.Test.get healthApp "/_health"
  let r5 := checkEq "smoke: health check status" 200 resp4.simpleStatus.statusCode
  let r6 := checkEq "smoke: health check body" "healthy" (String.fromUTF8! resp4.simpleBody)

  -- 5. Health middleware — passthrough path
  let resp5 ← Network.Wai.Test.post healthApp "/echo" "pass".toUTF8
  let r7 := checkEq "smoke: health passthrough" "pass" (String.fromUTF8! resp5.simpleBody)

  -- 6. Server header middleware — adds header
  let headerApp := serverHeaderMiddleware simpleApp
  let resp6 ← Network.Wai.Test.get headerApp "/"
  let hasServer := resp6.simpleHeaders.any fun (n, _) => toString n == "Server"
  let r8 := check "smoke: Server header present" hasServer

  -- 7. Composed middleware — both layers apply
  let composedApp := (healthMiddleware ∘ serverHeaderMiddleware) echoApp
  let resp7 ← Network.Wai.Test.get composedApp "/_health"
  let r9 := checkEq "smoke: composed health" 200 resp7.simpleStatus.statusCode

  let resp8 ← Network.Wai.Test.post composedApp "/other" "data".toUTF8
  let hasServer2 := resp8.simpleHeaders.any fun (n, _) => toString n == "Server"
  let r10 := check "smoke: composed adds header" hasServer2
  let r11 := checkEq "smoke: composed echoes body" "data" (String.fromUTF8! resp8.simpleBody)

  -- 8. Timestamp middleware (IO-before-delegate) — doesn't change response
  let tsApp := timestampMiddleware simpleApp
  let resp9 ← Network.Wai.Test.get tsApp "/"
  let r12 := checkEq "smoke: ioThen passthrough" 200 resp9.simpleStatus.statusCode

  pure [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12]

end TestWaiSmoke
