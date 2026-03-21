import Hale
import Tests.Harness

open Data Tests

/-
  Coverage:
  - Proofs: Dual.append_assoc, Endo.append_assoc, First.append_assoc,
            Last.append_assoc, Sum.append_assoc, Product.append_assoc,
            All.append_assoc, Any.append_assoc
  - Tested: Append semantics, identity elements, ToString
  - Not covered: None
-/

namespace TestNewtype

def tests : List TestResult :=
  [ -- Sum
    checkEq "Sum append" (Sum.mk 7) (Sum.mk 3 ++ Sum.mk 4 : Sum Nat)
  , checkEq "Sum identity" (Sum.mk 3) (Sum.mk 3 ++ Sum.mk 0 : Sum Nat)
  , check "Sum toString" (toString (Sum.mk 3 : Sum Nat) == "Sum(3)")
  , -- Product
    checkEq "Product append" (Product.mk 12) (Product.mk 3 ++ Product.mk 4 : Product Nat)
  , checkEq "Product identity" (Product.mk 5) (Product.mk 5 ++ Product.mk 1 : Product Nat)
  , check "Product toString" (toString (Product.mk 5 : Product Nat) == "Product(5)")
  , -- All
    checkEq "All true ++ true" (All.mk true) (All.mk true ++ All.mk true)
  , checkEq "All true ++ false" (All.mk false) (All.mk true ++ All.mk false)
  , checkEq "All identity" (All.mk false) (All.mk false ++ All.mk true)
  , check "All toString" (toString (All.mk true) == "All(true)")
  , -- Any
    checkEq "Any false ++ true" (Any.mk true) (Any.mk false ++ Any.mk true)
  , checkEq "Any false ++ false" (Any.mk false) (Any.mk false ++ Any.mk false)
  , checkEq "Any identity" (Any.mk true) (Any.mk true ++ Any.mk false)
  , check "Any toString" (toString (Any.mk false) == "Any(false)")
  , -- First
    checkEq "First none ++ some" (First.mk (some 42)) (First.mk (none : Option Nat) ++ First.mk (some 42))
  , checkEq "First some ++ some" (First.mk (some 1)) (First.mk (some 1) ++ First.mk (some 2) : First Nat)
  , check "First toString" (toString (First.mk (some 42) : First Nat) == "First((some 42))")
  , -- Last
    checkEq "Last some ++ some" (Last.mk (some 2)) (Last.mk (some 1) ++ Last.mk (some 2) : Last Nat)
  , check "Last toString" (toString (Last.mk (some 1) : Last Nat) == "Last((some 1))")
  , -- Dual
    checkEq "Dual reverses append" (Dual.mk [2, 1]) (Dual.mk [1] ++ Dual.mk [2] : Dual (List Nat))
  , check "Dual toString" (toString (Dual.mk "hello" : Dual String) == "Dual(hello)")
  , -- Endo
    checkEq "Endo composes" 7 ((Endo.mk (· + 1) ++ Endo.mk (· * 2) : Endo Nat).appEndo 3)
  , -- Proof coverage
    proofCovered "Dual.append_assoc" "Hale.Base.Data.Newtype"
  , proofCovered "Endo.append_assoc" "Hale.Base.Data.Newtype"
  , proofCovered "First.append_assoc" "Hale.Base.Data.Newtype"
  , proofCovered "Last.append_assoc" "Hale.Base.Data.Newtype"
  , proofCovered "Sum.append_assoc" "Hale.Base.Data.Newtype"
  , proofCovered "Product.append_assoc" "Hale.Base.Data.Newtype"
  , proofCovered "All.append_assoc" "Hale.Base.Data.Newtype"
  , proofCovered "Any.append_assoc" "Hale.Base.Data.Newtype"
  ]
end TestNewtype
