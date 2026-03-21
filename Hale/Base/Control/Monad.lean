/-
  Hale.Base.Control.Monad — Haskell-compatible monad combinators

  Provides the standard monad utility functions from Haskell's `Control.Monad`.
  Many of these wrap or extend Lean stdlib facilities, presented under the
  Haskell-compatible API in the `Control.Monad` namespace.
-/

namespace Control.Monad

/-- Monadic join: flattens a nested monadic value.

    $$\text{join} : m\,(m\;\alpha) \to m\;\alpha$$
    $$\text{join}\;mma = mma \mathbin{>>=} \text{id}$$ -/
def join {m : Type → Type} [Monad m] (mma : m (m α)) : m α :=
  mma >>= id

/-- Discard the result of a functor action, keeping only the effect.

    $$\text{void} : f\;\alpha \to f\;\text{Unit}$$
    $$\text{void}\;fa = (\lambda\;\_ \Rightarrow ()) \mathbin{<\$>} fa$$ -/
def void {f : Type → Type} [Functor f] (fa : f α) : f Unit :=
  (fun _ => ()) <$> fa

/-- Conditional execution: run the action only when the boolean is true.

    $$\text{«when»}\;b\;a = \begin{cases} a & \text{if } b \\ \text{pure}\;() & \text{otherwise} \end{cases}$$ -/
def «when» {m : Type → Type} [Monad m] (b : Bool) (action : m Unit) : m Unit :=
  if b then action else pure ()

/-- Conditional execution: run the action only when the boolean is false.

    $$\text{unless}\;b\;a = \text{when}\;(\lnot b)\;a$$ -/
def «unless» {m : Type → Type} [Monad m] (b : Bool) (action : m Unit) : m Unit :=
  «when» (!b) action

/-- Map a monadic function over a list, discarding the results.

    $$\text{mapM\_}\;f\;[x_1, \ldots, x_n] = f\;x_1 \mathbin{>>} \cdots \mathbin{>>} f\;x_n \mathbin{>>} \text{pure}\;()$$ -/
def mapM_ {m : Type → Type} [Monad m] (f : α → m β) : List α → m Unit
  | [] => pure ()
  | x :: xs => f x >>= fun _ => mapM_ f xs

/-- Flipped `mapM_`: iterate over a list with a monadic action, discarding results.

    $$\text{forM\_}\;xs\;f = \text{mapM\_}\;f\;xs$$ -/
def forM_ {m : Type → Type} [Monad m] (xs : List α) (f : α → m β) : m Unit :=
  mapM_ f xs

/-- Monadic left fold over a list.

    $$\text{foldM}\;f\;z\;[x_1, \ldots, x_n] = f\;z\;x_1 \mathbin{>>=} \lambda z_1 \Rightarrow f\;z_1\;x_2 \mathbin{>>=} \cdots$$ -/
def foldM {m : Type → Type} [Monad m] (f : β → α → m β) (init : β) : List α → m β
  | [] => pure init
  | x :: xs => f init x >>= fun acc => foldM f acc xs

/-- Monadic filter: keep elements for which the predicate returns true in the monad.

    $$\text{filterM}\;p\;xs = [x \in xs \mid p\;x]$$ -/
def filterM {m : Type → Type} [Monad m] (p : α → m Bool) : List α → m (List α)
  | [] => pure []
  | x :: xs => do
    let b ← p x
    let rest ← filterM p xs
    pure (if b then x :: rest else rest)

/-- Monadic zip: apply a binary monadic function to corresponding elements.

    $$\text{zipWithM}\;f\;[a_1, \ldots]\;[b_1, \ldots] = [f\;a_1\;b_1, f\;a_2\;b_2, \ldots]$$ -/
def zipWithM {m : Type → Type} [Monad m] (f : α → β → m γ) : List α → List β → m (List γ)
  | [], _ => pure []
  | _, [] => pure []
  | a :: as, b :: bs => do
    let c ← f a b
    let cs ← zipWithM f as bs
    pure (c :: cs)

/-- Repeat a monadic action `n` times, collecting the results.

    $$\text{replicateM}\;n\;ma = [ma, ma, \ldots]\text{ (n times)}$$ -/
def replicateM {m : Type → Type} [Monad m] (n : Nat) (ma : m α) : m (List α) :=
  match n with
  | 0 => pure []
  | n + 1 => do
    let a ← ma
    let as ← replicateM n ma
    pure (a :: as)

/-- Repeat a monadic action `n` times, discarding the results.

    $$\text{replicateM\_}\;n\;ma = ma \mathbin{>>} \cdots \mathbin{>>} ma \mathbin{>>} \text{pure}\;()$$ -/
def replicateM_ {m : Type → Type} [Monad m] (n : Nat) (ma : m α) : m Unit :=
  match n with
  | 0 => pure ()
  | n + 1 => ma >>= fun _ => replicateM_ n ma

/-- Kleisli composition (left-to-right): $(f \mathbin{>=>} g)\;a = f\;a \mathbin{>>=} g$.

    $$\text{fish} : (\alpha \to m\;\beta) \to (\beta \to m\;\gamma) \to \alpha \to m\;\gamma$$ -/
def fish {m : Type → Type} [Monad m] (f : α → m β) (g : β → m γ) : α → m γ :=
  fun a => f a >>= g

/-- Kleisli composition (right-to-left): $(g \mathbin{<=<} f) = f \mathbin{>=>} g$.

    $$\text{fishBack} : (\beta \to m\;\gamma) \to (\alpha \to m\;\beta) \to \alpha \to m\;\gamma$$ -/
def fishBack {m : Type → Type} [Monad m] (g : β → m γ) (f : α → m β) : α → m γ :=
  fish f g

/-- **Join-pure law:** joining a pure value is the identity.

    $$\text{join}\;(\text{pure}\;x) = x$$ -/
theorem join_pure {m : Type → Type} [Monad m] [LawfulMonad m] (x : m α) :
    join (pure x) = x := by
  simp [join, pure_bind]

end Control.Monad
