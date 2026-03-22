import Hale.Vault
import Tests.Harness

open Data Tests

/-
  Coverage:
  - Proofs: None (IO-based key allocation)
  - Tested: Key.new, empty, insert, lookup, delete, adjust, size, union
  - Not covered: None
-/

namespace TestVault

def tests : IO (List TestResult) := do
  let k1 ← Key.new (α := Nat)
  let k2 ← Key.new (α := String)
  let k3 ← Key.new (α := Nat)

  let v0 := Vault.empty
  let v1 := v0.insert k1 42
  let v2 := v1.insert k2 "hello"

  pure [
    -- Empty vault
    checkEq "empty size" 0 v0.size
  , check "lookup empty none" (v0.lookup k1 |>.isNone)
    -- Insert and lookup
  , checkEq "lookup k1" (some 42) (v2.lookup k1)
  , checkEq "lookup k2" (some "hello") (v2.lookup k2)
  , check "lookup k3 none" (v2.lookup k3 |>.isNone)
    -- Size
  , checkEq "size after 2 inserts" 2 v2.size
    -- Delete
  , let v3 := v2.delete k1
    check "lookup deleted none" (v3.lookup k1 |>.isNone)
  , checkEq "size after delete" 1 (v2.delete k1).size
    -- Adjust
  , let v4 := v2.adjust (· + 10) k1
    checkEq "adjust k1" (some 52) (v4.lookup k1)
  , let v5 := v2.adjust (· + 10) k3  -- k3 not present
    checkEq "adjust missing" (some 42) (v5.lookup k1)
    -- Union
  , let va := Vault.empty.insert k1 100
    let vb := Vault.empty.insert k2 "world"
    let vu := va.union vb
    checkEq "union k1" (some 100) (vu.lookup k1)
  , let va2 := Vault.empty.insert k1 100
    let vb2 := Vault.empty.insert k2 "world"
    let vu2 := va2.union vb2
    checkEq "union k2" (some "world") (vu2.lookup k2)
    -- Overwrite
  , let v6 : Vault := v2.insert k1 99
    checkEq "overwrite k1" (some 99) (v6.lookup k1)
  ]

end TestVault
