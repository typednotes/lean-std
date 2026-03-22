/-
  Hale.Warp — Haskell `warp` for Lean 4

  A fast, lightweight HTTP server library. Ports Haskell's `warp` package.

  ## Modules

  - `Network.Wai.Handler.Warp.Settings` — Server configuration
  - `Network.Wai.Handler.Warp.Request` — HTTP request parsing
  - `Network.Wai.Handler.Warp.Response` — HTTP response rendering
  - `Network.Wai.Handler.Warp.Run` — Accept loop and connection handling
  - `Network.Wai.Handler.Warp` — Public API (re-exports + `run`)
-/
import Hale.Warp.Network.Wai.Handler.Warp
