/-
  Hale.HttpTypes.Network.HTTP.Types.Version — HTTP version
-/

namespace Network.HTTP.Types

/-- HTTP protocol version.
    $$\text{HttpVersion} = \{ \text{major} : \mathbb{N},\; \text{minor} : \mathbb{N} \}$$ -/
structure HttpVersion where
  major : Nat
  minor : Nat
deriving BEq, Repr

instance : ToString HttpVersion where
  toString v := s!"HTTP/{v.major}.{v.minor}"

instance : Ord HttpVersion where
  compare a b :=
    match compare a.major b.major with
    | .eq => compare a.minor b.minor
    | ord => ord

/-- HTTP/0.9 -/
def http09 : HttpVersion := ⟨0, 9⟩
/-- HTTP/1.0 -/
def http10 : HttpVersion := ⟨1, 0⟩
/-- HTTP/1.1 -/
def http11 : HttpVersion := ⟨1, 1⟩
/-- HTTP/2.0 -/
def http20 : HttpVersion := ⟨2, 0⟩

end Network.HTTP.Types
