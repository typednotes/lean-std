import Hale.WarpQUIC
import Tests.Harness

open Network.Wai.Handler.WarpQUIC Tests

/-
  Coverage:
  - Proofs in source: (none -- WarpQUIC is runtime glue)
  - Tested here: Settings construction, toQUICConfig, toH3Settings,
    defaultSettings, h3RequestToHeaders
  - Not covered: run, runH3, runQUIC, handleConnection (require QUIC FFI)
-/

namespace TestWarpQUIC

def tests : List TestResult :=
  [ -- Settings construction
    check "Settings default port is 443"
      ((defaultSettings "/cert.pem" "/key.pem").port == 443)
  , check "Settings default host is 0.0.0.0"
      ((defaultSettings "/cert.pem" "/key.pem").host == "0.0.0.0")
  , check "Settings certFile preserved"
      ((defaultSettings "/cert.pem" "/key.pem").certFile == "/cert.pem")
  , check "Settings keyFile preserved"
      ((defaultSettings "/cert.pem" "/key.pem").keyFile == "/key.pem")
  , check "Settings default maxConcurrentStreams"
      ((defaultSettings "/c" "/k").maxConcurrentStreams == 100)
  , check "Settings default qpackMaxTableCapacity"
      ((defaultSettings "/c" "/k").qpackMaxTableCapacity == 4096)
  , check "Settings default serverName"
      ((defaultSettings "/c" "/k").serverName == "Hale/WarpQUIC")

  -- toQUICConfig
  , check "toQUICConfig port" (
      let cfg := toQUICConfig (defaultSettings "/c.pem" "/k.pem")
      cfg.port == 443)
  , check "toQUICConfig host" (
      let cfg := toQUICConfig (defaultSettings "/c.pem" "/k.pem")
      cfg.host == "0.0.0.0")
  , check "toQUICConfig TLS certFile" (
      let cfg := toQUICConfig (defaultSettings "/c.pem" "/k.pem")
      cfg.tlsConfig.certFile == some "/c.pem")
  , check "toQUICConfig TLS keyFile" (
      let cfg := toQUICConfig (defaultSettings "/c.pem" "/k.pem")
      cfg.tlsConfig.keyFile == some "/k.pem")
  , check "toQUICConfig TLS alpn is h3" (
      let cfg := toQUICConfig (defaultSettings "/c.pem" "/k.pem")
      cfg.tlsConfig.alpn == ["h3"])
  , check "toQUICConfig transport maxStreamsBidi" (
      let s : Settings := { certFile := "/c", keyFile := "/k", maxConcurrentStreams := 50 }
      let cfg := toQUICConfig s
      cfg.transportParams.initialMaxStreamsBidi == 50)

  -- toH3Settings
  , check "toH3Settings qpackMaxTableCapacity" (
      let s : Settings := { certFile := "/c", keyFile := "/k", qpackMaxTableCapacity := 8192 }
      let h3s := toH3Settings s
      h3s.qpackMaxTableCapacity == 8192)
  , check "toH3Settings qpackBlockedStreams" (
      let s : Settings := { certFile := "/c", keyFile := "/k", qpackBlockedStreams := 200 }
      let h3s := toH3Settings s
      h3s.qpackBlockedStreams == 200)

  -- h3RequestToHeaders
  , check "h3RequestToHeaders includes pseudo-headers" (
      let req : Network.HTTP3.H3Request := {
        method := "GET", path := "/index.html",
        scheme := "https", authority := "example.com",
        headers := [("accept", "text/html")],
        readBody := pure ByteArray.empty
      }
      let hdrs := h3RequestToHeaders req
      hdrs.length == 5)
  ]

end TestWarpQUIC
