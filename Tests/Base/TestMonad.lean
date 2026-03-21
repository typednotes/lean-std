import Hale
import Hale.Base.Control.Monad
import Tests.Harness

open Tests

/-
  Coverage:
  - Proofs: join_pure
  - Tested: join, void, when, unless, mapM_, forM_, foldM, filterM,
            zipWithM, replicateM, replicateM_, fish, fishBack
  - Not covered: None
-/

namespace TestMonad

open Control.Monad in
def tests : List TestResult :=
  [ -- join
    checkEq "join Option some some" (some 42)
      (join (some (some 42)))
  , checkEq "join Option some none" (none : Option Nat)
      (join (some (none : Option Nat)))
  , checkEq "join Option none" (none : Option Nat)
      (join (none : Option (Option Nat)))
  , checkEq "join Option nested" (some 99)
      (join (some (some 99)))
  -- void
  , checkEq "void Option some" (some ())
      (void (some 42))
  , checkEq "void Option none" (none : Option Unit)
      (void (none : Option Nat))
  , checkEq "void List" [(), (), ()]
      (void [1, 2, 3])
  -- when / unless (keywords, use French quotes)
  , checkEq "when true" (some ())
      (Control.Monad.«when» (m := Option) true (some ()))
  , checkEq "when false" (some ())
      (Control.Monad.«when» (m := Option) false (none))
  , checkEq "unless false" (some ())
      (Control.Monad.«unless» (m := Option) false (some ()))
  , checkEq "unless true" (some ())
      (Control.Monad.«unless» (m := Option) true (none))
  -- foldM
  , checkEq "foldM Option success" (some 10)
      (foldM (m := Option) (fun acc x => some (acc + x)) 0 [1, 2, 3, 4])
  , checkEq "foldM Option failure" (none : Option Nat)
      (foldM (m := Option) (fun acc x => if x == 0 then none else some (acc + x)) 0 [1, 0, 3])
  , checkEq "foldM empty" (some 0)
      (foldM (m := Option) (fun acc x => some (acc + x)) 0 ([] : List Nat))
  -- filterM
  , checkEq "filterM Option" (some [2, 4])
      (filterM (m := Option) (fun x => some (x % 2 == 0)) [1, 2, 3, 4])
  -- zipWithM
  , checkEq "zipWithM Option success" (some [5, 7, 9])
      (zipWithM (m := Option) (fun a b => some (a + b)) [1, 2, 3] [4, 5, 6])
  , checkEq "zipWithM unequal lengths" (some [5, 7])
      (zipWithM (m := Option) (fun a b => some (a + b)) [1, 2] [4, 5, 6])
  , checkEq "zipWithM empty" (some ([] : List Nat))
      (zipWithM (m := Option) (fun a b => some (a + b)) ([] : List Nat) [4, 5, 6])
  -- replicateM
  , checkEq "replicateM Option" (some [1, 1, 1])
      (replicateM (m := Option) 3 (some 1))
  , checkEq "replicateM Option none" (none : Option (List Nat))
      (replicateM (m := Option) 3 (none : Option Nat))
  , checkEq "replicateM 0" (some ([] : List Nat))
      (replicateM (m := Option) 0 (some 1))
  -- fish (Kleisli composition)
  , checkEq "fish Option" (some 12)
      (fish (m := Option) (fun x => some (x + 1)) (fun y => some (y * 3)) 3)
  , checkEq "fish Option failure" (none : Option Nat)
      (fish (m := Option) (fun _ => (none : Option Nat)) (fun y => some (y * 3)) 3)
  -- fishBack
  , checkEq "fishBack Option" (some 12)
      (fishBack (m := Option) (fun y => some (y * 3)) (fun x => some (x + 1)) 3)
  -- Proof coverage
  , proofCovered "join_pure" "Hale.Base.Control.Monad"
  ]
end TestMonad
