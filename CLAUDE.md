# Std project

This library provides functionalities heavily inspired by haskell popular libraries but with a maximalist stance on typing.

## Typing approach

We'd like our implementations to explicitly state and prove the guarantees they provide.

Ideally the types give information about
- correctness
- useful properties and invariants
- performance guarantees
- algorithmic complexity
- resource usage

when proofs are feasible.

## Lazy vs Strict Evaluation

Haskell is lazily evaluated while Lean is strictly evaluated. When porting Haskell code, be aware that some idioms rely on laziness (e.g., infinite lists, guarded recursion, lazy fields in data types). Use `Stream` or `Thunk` in Lean to emulate Haskell's lazy behavior when necessary.

## Haskell → Lean Module Mapping

Reference: https://hackage.haskell.org/package/base

| Lean Module | Haskell Module |
|---|---|
| `Hale.Base.Data.Void` | `Data.Void` |
| `Hale.Base.Data.Function` | `Data.Function` |
| `Hale.Base.Data.Newtype` | `Data.Monoid` / `Data.Semigroup` |
| `Hale.Base.Data.Bool` | `Data.Bool` |
| `Hale.Base.Data.Maybe` | `Data.Maybe` |
| `Hale.Base.Data.Char` | `Data.Char` |
| `Hale.Base.Data.String` | `Data.String` |
| `Hale.Base.Data.List` | `Data.List` |
| `Hale.Base.Data.Proxy` | `Data.Proxy` |
| `Hale.Base.Data.Unique` | `Data.Unique` |
| `Hale.Base.Data.IORef` | `Data.IORef` |
| `Hale.Base.Data.Bits` | `Data.Bits` |
| `Hale.Base.Data.Ix` | `Data.Ix` |
| `Hale.Base.Data.Bifunctor` | `Data.Bifunctor` |
| `Hale.Base.Data.Functor.Contravariant` | `Data.Functor.Contravariant` |
| `Hale.Base.Data.Functor.Const` | `Data.Functor.Const` |
| `Hale.Base.Data.Functor.Identity` | `Data.Functor.Identity` |
| `Hale.Base.Data.Functor.Compose` | `Data.Functor.Compose` |
| `Hale.Base.Data.Functor.Product` | `Data.Functor.Product` |
| `Hale.Base.Data.Functor.Sum` | `Data.Functor.Sum` |
| `Hale.Base.Control.Category` | `Control.Category` |
| `Hale.Base.Control.Applicative` | `Control.Applicative` |
| `Hale.Base.Control.Monad` | `Control.Monad` |
| `Hale.Base.Control.Exception` | `Control.Exception` |
| `Hale.Base.Data.List.NonEmpty` | `Data.List.NonEmpty` |
| `Hale.Base.Data.Either` | `Data.Either` |
| `Hale.Base.Data.Ord` | `Data.Ord` |
| `Hale.Base.Data.Tuple` | `Data.Tuple` + `Prelude` |
| `Hale.Base.Data.Foldable` | `Data.Foldable` |
| `Hale.Base.Data.Traversable` | `Data.Traversable` |
| `Hale.Base.Data.Ratio` | `Data.Ratio` |
| `Hale.Base.Data.Complex` | `Data.Complex` |
| `Hale.Base.Data.Fixed` | `Data.Fixed` |
| `Hale.Base.Control.Arrow` | `Control.Arrow` |
| `Hale.Base.Control.Concurrent` | `Control.Concurrent` |
| `Hale.Base.Control.Concurrent.MVar` | `Control.Concurrent.MVar` |
| `Hale.Base.Control.Concurrent.Chan` | `Control.Concurrent.Chan` |
| `Hale.Base.Control.Concurrent.QSem` | `Control.Concurrent.QSem` |
| `Hale.Base.Control.Concurrent.QSemN` | `Control.Concurrent.QSemN` |
| `Hale.Base.System.IO` | `System.IO` |
| `Hale.Base.System.Exit` | `System.Exit` |
| `Hale.Base.System.Environment` | `System.Environment` |

Reference: https://hackage.haskell.org/package/bytestring

| Lean Module | Haskell Module |
|---|---|
| `Hale.ByteString.Data.ByteString.Internal` | `Data.ByteString.Internal` |
| `Hale.ByteString.Data.ByteString` | `Data.ByteString` |
| `Hale.ByteString.Data.ByteString.Char8` | `Data.ByteString.Char8` |
| `Hale.ByteString.Data.ByteString.Short` | `Data.ByteString.Short` |
| `Hale.ByteString.Data.ByteString.Lazy.Internal` | `Data.ByteString.Lazy.Internal` |
| `Hale.ByteString.Data.ByteString.Lazy` | `Data.ByteString.Lazy` |
| `Hale.ByteString.Data.ByteString.Lazy.Char8` | `Data.ByteString.Lazy.Char8` |
| `Hale.ByteString.Data.ByteString.Builder` | `Data.ByteString.Builder` |

## Folder Organization Policy

The `Hale` project ports multiple Haskell libraries. Each Haskell library gets its own **top-level folder** named after the library (Lean naming convention). Within that folder, the **subfolder path mirrors the Haskell module path** exactly.

```
Hale/
  Base/                                  ← Haskell `base` library
    Data/
      Void.lean                          ← Data.Void
      Function.lean                      ← Data.Function
      Functor/
        Const.lean                       ← Data.Functor.Const
        Identity.lean                    ← Data.Functor.Identity
        Compose.lean                     ← Data.Functor.Compose
        Contravariant.lean               ← Data.Functor.Contravariant
      List/
        NonEmpty.lean                    ← Data.List.NonEmpty
      ...
    Control/
      Category.lean                      ← Control.Category
      Arrow.lean                         ← Control.Arrow
      Concurrent.lean                    ← Control.Concurrent
      Concurrent/
        MVar.lean                        ← Control.Concurrent.MVar
        Chan.lean                        ← Control.Concurrent.Chan
        ...
  Containers/                            ← Haskell `containers` (future)
    Data/
      Map.lean                           ← Data.Map
      Set.lean                           ← Data.Set
      ...
  Text/                                  ← Haskell `text` (future)
    Data/
      Text.lean                          ← Data.Text
      ...
```

**Rules:**
1. **Top-level folder = Haskell library name** in Lean naming convention (`Base` for `base`, `Containers` for `containers`, etc.)
2. **Subfolder path = Haskell module path** exactly (`Data/Functor/Const.lean` for `Data.Functor.Const`)
3. **Namespace = Haskell module path** — namespaces mirror the Haskell hierarchy, NOT the library name. Examples:
   - `Hale/Base/Data/Ratio.lean` → `namespace Data` (outer), `namespace Ratio` (inner for methods)
   - `Hale/Base/Data/Functor/Const.lean` → `namespace Data.Functor` (outer), `namespace Const` (inner)
   - `Hale/Base/Control/Concurrent/MVar.lean` → `namespace Control.Concurrent` (outer), `namespace MVar` (inner)
   - Users write `open Data` or `open Control.Concurrent` to access types, just like Haskell `import Data.Ratio` or `import Control.Concurrent.MVar`
4. **Sub-namespaces for methods** — use `namespace Ratio` within `namespace Data` for dot-notation methods (e.g., `Ratio.floor`)
5. **Re-export file** — each library has a re-export file (`Hale/Base.lean`) that imports all its modules

**Tests** mirror the Haskell module structure: `Tests/Control/TestMVar.lean` for `Control.Concurrent.MVar`.

**Docs** mirror the Haskell module structure: `docs/Control/MVar.md` for `Control.Concurrent.MVar`.

## Module Organization

- **Foundational:** Basic types and combinators — `Void`, `Function`, `Newtype`
- **Core Abstractions:** Functor variants and composition — `Bifunctor`, `Contravariant`, `Const`, `Identity`, `Compose`, `Category`
- **Data Structures:** Concrete data types — `NonEmpty`, `Either`, `Ord`, `Tuple`
- **Traversals:** Fold and traverse abstractions — `Foldable`, `Traversable`
- **Numeric Types:** Exact arithmetic — `Ratio`, `Complex`, `Fixed`
- **Advanced Abstractions:** Arrow computations — `Arrow`
- **Concurrency:** Thread management and synchronisation — `Concurrent`, `MVar`, `Chan`, `QSem`, `QSemN`

## Build & Test

```bash
# Build the library
lake build

# Run smoke tests
lake exe hale

# Build and run the test suite
lake build hale-tests
lake exe hale-tests

# Run Haskell cross-verification (requires GHC)
bash tests/cross-check/run-all.sh
```

## Porting Approach

When porting a Haskell library:

1. **Same API, adapted implementation.** Port the same public API surface. Use the same implementation approach unless Lean's standard library provides a better backing (e.g., `ByteArray`, `HashMap`, `Array`, `IO.FS`) or language differences (lazy vs strict evaluation) make a different implementation more appropriate.
2. **Port the value-add:** Focus on typed invariants, O(1) slicing, algebraic proofs, and API surface that Lean lacks.
3. **Port transitive dependencies first.** If the Haskell library depends on another unported Haskell library, port that dependency before proceeding.
4. **Lean stdlib preference:** When Lean's stdlib already provides equivalent functionality, use it as the backing implementation and provide Haskell-compatible naming on top.

## Haskell Cross-Verification

Every ported module must be cross-verified against Haskell's actual behavior:

1. **Haskell reference program:** `Tests/cross-check/haskell/<Module>.hs` — exercises key operations and prints deterministic output.
2. **Lean smoke test:** `Main.lean` (or a dedicated exe) produces identical output lines.
3. **Shell script:** `Tests/cross-check/check-<module>.sh` compares the outputs.
4. **Coverage targets:** construction, basic operations, edge cases (empty input, single element, boundaries), typeclass behavior (Eq, Ord, Monoid), and I/O roundtrips.
5. **Run all:** `bash Tests/cross-check/run-all.sh`

## For Downstream Porters

To port a Haskell library that depends on `base`, depend on `hale` for the base types. The mapping table above shows which Lean module corresponds to each Haskell `base` module.

## Adding Missing Modules or Functions

When porting a Haskell package that depends on `base` or on an already-ported package, you may discover that a module or function is missing from `hale`. In that case, add it directly to the appropriate dependency (`hale` for `base`, or the corresponding ported package) with the same level of rigor:

1. **Maximalist typing:** Encode correctness proofs, invariants, and guarantees in the types (see "Typing approach" above)
2. **Lawful instances:** If adding a typeclass instance, prove the relevant laws (identity, composition, associativity, etc.)
3. **Lean tests:** Add tests in `Tests/<HaskellPath>/Test<Module>.lean` covering construction, operations, instance behavior, and edge cases (e.g., `Tests/Control/TestMVar.lean` for `Control.Concurrent.MVar`)
4. **Documentation:** Add or update the corresponding `docs/<HaskellPath>/<Module>.md` with API mapping, instances, proofs, and examples (e.g., `docs/Control/MVar.md`)
5. **Cross-check (when applicable):** If the function has observable output, add a Haskell cross-verification script in `tests/cross-check/`
6. **Update the mapping table:** Keep the Haskell-to-Lean mapping in this file and in `README.md` up to date

Do not add the missing functionality in the downstream package — always contribute it back to the ported dependency so all downstream consumers benefit.

## Documentation Standards

Every public definition and module must be documented:

1. **Module-level docstring:** Purpose, design rationale, typing guarantees, axiom-dependent properties
2. **Definition-level docstring:** Include LaTeX equations for the type signature (e.g., `$$\text{take} : \text{MVar}\ \alpha \to \text{BaseIO}\ (\text{Task}\ \alpha)$$`)
3. **Docs folder:** Each module gets a corresponding `docs/<HaskellPath>/<Module>.md` with:
   - Haskell-to-Lean API mapping table
   - Instance documentation
   - Proof/invariant documentation
   - Usage examples
   - Performance/scalability notes

## Strict Typing Review

Before finalising any module, review every definition for stricter types:

1. **Return types:** Can the return type carry a proof? (e.g., `{n : Nat // n > 0}` instead of `Nat`)
2. **Arguments:** Can arguments be constrained? (e.g., `(n : Nat) (h : n > 0)` instead of bare `Nat`)
3. **Structures:** Can fields carry invariants? (e.g., state invariants as proof obligations)
4. **Type aliases:** Do they encode meaningful guarantees? (e.g., `Concurrent α := BaseIO (Task α)` encodes non-blocking)

When proofs are infeasible due to opaque runtime primitives (e.g., `Std.Mutex`, `IO.Promise`), document the invariant as an axiom-dependent property.

## Code Simplification Review

Before finalising any module, review for simplification:

1. **Factor common patterns:** If two functions share >50% of their logic, extract a shared helper
2. **Avoid redundant state copies:** Use `modify` over `get`/`set` when the entire state changes
3. **Prefer `Std.Mutex.atomically` with direct state operations** over manual lock/unlock
4. **Minimise `sorry` and `panic!`:** Every `sorry` must have a tracking comment; every `panic!` must be unreachable by construction

## Standard Test Porting Procedure

### Port upstream Haskell tests

Every ported module must also port the corresponding Haskell test suite. The upstream tests are the primary source of test coverage — they define what behavior the port must match. Ported Haskell tests become part of the validation harness and must pass during generation.

### Proofs over tests

When porting a Haskell test, always ask: **can this test be expressed as a proof embedded in the types?** A type-checking theorem is strictly stronger than a runtime test — it holds for all inputs, not just the tested ones, and never needs to be run.

**Preference hierarchy:**
1. **Proof in source** (theorem in the module) — strongest, covers all cases, verified at compile time
2. **Runtime test in `Tests/`** — for IO, opaque primitives, or properties that are infeasible to prove
3. **`sorry`-marked theorem** with tracking comment — for invariants that should be provable but aren't yet

**Examples of tests that become proofs:**
- "map id = id" → `theorem map_id (x : F α) : id <$> x = x := rfl`
- "pure a >>= f = f a" → `theorem pure_bind ...`
- "conjugate (conjugate z) = z" → `theorem conjugate_conjugate ...`
- "partition preserves length" → `theorem partitionEithers_length ...`

**Tests that stay as tests:**
- IO roundtrips (write then read back)
- Opaque ByteArray operations
- Concrete numeric edge cases (overflow, rounding)
- Thunk/lazy evaluation behavior

### Coverage rule

Every public `def`, `instance`, `structure`, `class`, or `theorem` must be covered by either a proof or a runtime test.

**Coverage table:**

| Category | Required |
|---|---|
| Typeclass instance | All laws (identity, composition, associativity) — prefer as proofs |
| Algebraic operation | Identity, commutativity, associativity, inverse — prefer as proofs |
| Constructor | Construction + accessor roundtrip |
| Conversion | Roundtrip identity (`pack`/`unpack`, `toStrict`/`fromStrict`) — prove when possible |
| Predicate | True case, false case, empty-input edge case |
| Fold/traversal | Empty, singleton, multi-element |

**Coverage header comment** required in each test file listing:
- Proofs in source (covered by type-checking)
- Tested here (runtime tests)
- Not yet covered (tracking gaps)

**`proofCovered` helper:** Use `proofCovered` in `Tests/Harness.lean` to record proof-based coverage in test output — a theorem in source always passes, but appears in the report.