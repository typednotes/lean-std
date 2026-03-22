/-
  Hale.Warp.Network.Wai.Handler.Warp.Settings — Warp server configuration

  Ports Haskell's `Network.Wai.Handler.Warp.Settings` from the `warp` package.

  ## Design

  `Settings` mirrors Haskell's `Settings` record type. All fields have sensible
  defaults so `defaultSettings` (or `{}`) is a valid, production-ready configuration.

  ## Guarantees

  - `settingsPort` is `UInt16`, bounding the port to [0, 65535] by construction
  - `settingsTimeout` and `settingsBacklog` are `Nat`, ensuring non-negativity
-/

import Hale.WAI
import Hale.HttpTypes
import Hale.Network

namespace Network.Wai.Handler.Warp

open Network.HTTP.Types
open Network.Socket (SockAddr)

/-- Warp server settings.
    $$\text{Settings} = \{ \text{port} : \text{UInt16},\; \text{host} : \text{String},\; \ldots \}$$ -/
structure Settings where
  /-- Port to listen on. Default: 3000. -/
  settingsPort : UInt16 := 3000
  /-- Host to bind to. Default: "0.0.0.0" (all interfaces). -/
  settingsHost : String := "0.0.0.0"
  /-- Called when an exception occurs during request handling.
      Receives the remote address if available. -/
  settingsOnException : Option SockAddr → IO Unit := fun _ => pure ()
  /-- Called just before the server starts its accept loop.
      Useful for logging "server started on port X". -/
  settingsBeforeMainLoop : IO Unit := pure ()
  /-- Server name for the `Server` response header. -/
  settingsServerName : String := "Hale/Warp"
  /-- Maximum number of bytes to flush from a request body on connection
      reuse. `none` means no flushing limit. -/
  settingsMaximumBodyFlush : Option Nat := some 8192
  /-- Timeout in seconds for each connection. -/
  settingsTimeout : Nat := 30
  /-- Socket listen backlog. -/
  settingsBacklog : Nat := 128
  /-- Graceful shutdown timeout in seconds. `none` means no graceful shutdown. -/
  settingsGracefulShutdownTimeout : Option Nat := none
  /-- Whether to auto-add the `Date` response header. -/
  settingsAddDateHeader : Bool := true
  /-- Whether to auto-add the `Server` response header. -/
  settingsAddServerHeader : Bool := true

/-- Default settings.
    $$\text{defaultSettings} = \text{Settings}\{\}$$ -/
def defaultSettings : Settings := {}

end Network.Wai.Handler.Warp
