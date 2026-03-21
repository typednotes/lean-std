import Hale
import Tests.Harness

open Data.Functor Tests

/-
  Coverage:
  - Proofs: map_id, map_comp, pure_bind, bind_pure, bind_assoc
  - Tested: Construction, map, pure, bind, seq, ToString, BEq
  - Not covered: None
-/

namespace TestIdentity

def tests : List TestResult :=
  [ checkEq "Identity construction" 42 (Identity.mk 42).runIdentity
  , checkEq "Identity map" 43 ((Functor.map (· + 1) (Identity.mk 42) : Identity Nat).runIdentity)
  , checkEq "Identity pure" 5 ((pure 5 : Identity Nat).runIdentity)
  , checkEq "Identity bind" 84 ((Identity.mk 42 >>= fun n => Identity.mk (n * 2) : Identity Nat).runIdentity)
  , checkEq "Identity seq" 10 ((Seq.seq (Identity.mk (· * 2)) (fun () => Identity.mk 5) : Identity Nat).runIdentity)
  -- ToString
  , check "Identity toString" (toString (Identity.mk 42 : Identity Nat) == "42")
  -- BEq
  , check "Identity BEq equal" (Identity.mk 42 == (Identity.mk 42 : Identity Nat))
  , check "Identity BEq not equal" !(Identity.mk 42 == (Identity.mk 99 : Identity Nat))
  -- Proof coverage
  , proofCovered "Identity.map_id" "Hale.Base.Data.Functor.Identity"
  , proofCovered "Identity.map_comp" "Hale.Base.Data.Functor.Identity"
  , proofCovered "Identity.pure_bind" "Hale.Base.Data.Functor.Identity"
  , proofCovered "Identity.bind_pure" "Hale.Base.Data.Functor.Identity"
  , proofCovered "Identity.bind_assoc" "Hale.Base.Data.Functor.Identity"
  ]
end TestIdentity
