/-
  Hale.Base — Haskell `base` for Lean 4

  Re-exports all Base sub-modules. Inspired by Haskell's `base` package,
  with a maximalist approach to typing: types encode correctness proofs,
  invariants, and guarantees.
-/

-- Phase 0: Foundational
import Hale.Base.Data.Void
import Hale.Base.Data.Function
import Hale.Base.Data.Newtype

-- Phase 1: Core Abstractions
import Hale.Base.Data.Bifunctor
import Hale.Base.Data.Functor.Contravariant
import Hale.Base.Data.Functor.Const
import Hale.Base.Data.Functor.Identity
import Hale.Base.Data.Functor.Compose
import Hale.Base.Control.Category

-- Phase 2: Data Structures
import Hale.Base.Data.List.NonEmpty
import Hale.Base.Data.Either
import Hale.Base.Data.Ord
import Hale.Base.Data.Tuple

-- Phase 3: Traversals
import Hale.Base.Data.Foldable
import Hale.Base.Data.Traversable

-- Phase 4: Numeric Types
import Hale.Base.Data.Ratio
import Hale.Base.Data.Complex
import Hale.Base.Data.Fixed

-- Phase 5: Advanced Abstractions
import Hale.Base.Control.Arrow

-- Concurrency
import Hale.Base.Control.Concurrent
import Hale.Base.Control.Concurrent.MVar
import Hale.Base.Control.Concurrent.Chan
import Hale.Base.Control.Concurrent.QSem
import Hale.Base.Control.Concurrent.QSemN
