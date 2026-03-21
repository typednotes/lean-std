import Hale
import Tests.Harness

open Data.Maybe Tests

/-
  Coverage:
  - Proofs: maybe_none, maybe_some, fromMaybe_none, fromMaybe_some,
            catMaybes_nil, mapMaybe_nil, maybeToList_listToMaybe, catMaybes_eq_mapMaybe_id
  - Tested: maybe, fromMaybe, isJust, isNothing, catMaybes, mapMaybe, listToMaybe, maybeToList, fromJust
  - Not covered: None
-/

namespace TestMaybe

def tests : List TestResult :=
  [ -- maybe
    checkEq "maybe none" 0 (maybe 0 (· + 1) (none : Option Nat))
  , checkEq "maybe some" 43 (maybe 0 (· + 1) (some 42))
  -- fromMaybe
  , checkEq "fromMaybe none" 99 (fromMaybe 99 (none : Option Nat))
  , checkEq "fromMaybe some" 42 (fromMaybe 99 (some 42))
  -- isJust / isNothing
  , check "isJust some" (isJust (some 1))
  , check "isJust none" (!isJust (none : Option Nat))
  , check "isNothing none" (isNothing (none : Option Nat))
  , check "isNothing some" (!isNothing (some 1))
  -- catMaybes
  , checkEq "catMaybes" [1, 3] (catMaybes [some 1, none, some 3, none])
  , checkEq "catMaybes empty" ([] : List Nat) (catMaybes [])
  -- mapMaybe
  , checkEq "mapMaybe" [4, 6] (mapMaybe (fun n => if n > 1 then some (n * 2) else none) [1, 2, 3])
  , checkEq "mapMaybe empty" ([] : List Nat) (mapMaybe some [])
  -- listToMaybe
  , checkEq "listToMaybe nonempty" (some 1) (listToMaybe [1, 2, 3])
  , checkEq "listToMaybe empty" (none : Option Nat) (listToMaybe [])
  -- maybeToList
  , checkEq "maybeToList some" [42] (maybeToList (some 42))
  , checkEq "maybeToList none" ([] : List Nat) (maybeToList none)
  -- fromJust
  , checkEq "fromJust" 42 (fromJust (some 42) rfl)
  -- Proof coverage
  , proofCovered "maybe_none" "Hale.Base.Data.Maybe"
  , proofCovered "maybe_some" "Hale.Base.Data.Maybe"
  , proofCovered "fromMaybe_none" "Hale.Base.Data.Maybe"
  , proofCovered "fromMaybe_some" "Hale.Base.Data.Maybe"
  , proofCovered "catMaybes_nil" "Hale.Base.Data.Maybe"
  , proofCovered "mapMaybe_nil" "Hale.Base.Data.Maybe"
  , proofCovered "maybeToList_listToMaybe" "Hale.Base.Data.Maybe"
  , proofCovered "catMaybes_eq_mapMaybe_id" "Hale.Base.Data.Maybe"
  ]
end TestMaybe
