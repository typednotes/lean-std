import Hale
import Tests.Harness

open Data Tests

/-
  Coverage:
  - Proofs: map_id, map_comp, pure_bind, bind_pure, bind_assoc
  - Tested: BEq, Ord, ToString, Functor, Monad
  - Not covered: None
-/

namespace TestProxy

def tests : List TestResult :=
  [ -- BEq (always true)
    check "Proxy BEq" (Proxy.mk == (Proxy.mk : Proxy Nat))
  -- Ord (always eq)
  , check "Proxy Ord" (compare (Proxy.mk : Proxy Nat) Proxy.mk == .eq)
  -- ToString
  , check "Proxy toString" (toString (Proxy.mk : Proxy Nat) == "Proxy")
  -- Functor map
  , check "Proxy map" (toString (Functor.map (· + 1) (Proxy.mk : Proxy Nat) : Proxy Nat) == "Proxy")
  -- Monad pure/bind
  , check "Proxy pure" (toString ((pure 42 : Proxy Nat)) == "Proxy")
  , check "Proxy bind" (toString ((Proxy.mk : Proxy Nat) >>= fun _ => (Proxy.mk : Proxy String)) == "Proxy")
  -- Proof coverage
  , proofCovered "Proxy.map_id" "Hale.Base.Data.Proxy"
  , proofCovered "Proxy.map_comp" "Hale.Base.Data.Proxy"
  , proofCovered "Proxy.pure_bind" "Hale.Base.Data.Proxy"
  , proofCovered "Proxy.bind_pure" "Hale.Base.Data.Proxy"
  , proofCovered "Proxy.bind_assoc" "Hale.Base.Data.Proxy"
  ]
end TestProxy
