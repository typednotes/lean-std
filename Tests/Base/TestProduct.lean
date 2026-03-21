import Hale
import Hale.Base.Data.Functor.Product
import Tests.Harness

open Data.Functor Tests

/-
  Coverage:
  - Proofs: Product.map_id, Product.map_comp
  - Tested: Construction, functor map, BEq
  - Not covered: None
-/

namespace TestProduct

def tests : List TestResult :=
  [ -- Construction
    checkEq "Product runProduct fst" [1, 2, 3]
      (Product.mk ([1, 2, 3], some 42) : Product List Option Nat).runProduct.1
  , checkEq "Product runProduct snd" (some 42)
      (Product.mk ([1, 2, 3], some 42) : Product List Option Nat).runProduct.2
  -- Functor map
  , checkEq "Product map fst" [2, 4, 6]
      ((Functor.map (· * 2) (Product.mk ([1, 2, 3], some 5)) : Product List Option Nat).runProduct.1)
  , checkEq "Product map snd" (some 10)
      ((Functor.map (· * 2) (Product.mk ([1, 2, 3], some 5)) : Product List Option Nat).runProduct.2)
  -- BEq
  , check "Product BEq equal"
      (Product.mk ([1, 2], some 3) == (Product.mk ([1, 2], some 3) : Product List Option Nat))
  , check "Product BEq not equal fst"
      !(Product.mk ([1], some 3) == (Product.mk ([1, 2], some 3) : Product List Option Nat))
  , check "Product BEq not equal snd"
      !(Product.mk ([1, 2], some 3) == (Product.mk ([1, 2], some 4) : Product List Option Nat))
  -- Proof coverage
  , proofCovered "Product.map_id" "Hale.Base.Data.Functor.Product"
  , proofCovered "Product.map_comp" "Hale.Base.Data.Functor.Product"
  ]
end TestProduct
