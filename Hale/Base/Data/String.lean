/-
  Hale.Base.Data.String — String utilities and IsString class

  Provides Haskell's `Data.String` API: `IsString` class and
  string splitting/joining utilities (`lines`, `words`, `unlines`, `unwords`).
-/

namespace Data.DataString

/-- Overloaded string literals. A type `α` with an `IsString` instance
    can be constructed from a `String` literal.

    $$\text{fromString} : \text{String} \to \alpha$$ -/
class IsString (α : Type u) where
  /-- Convert a `String` to the target type. -/
  fromString : String → α

instance : IsString String where
  fromString := id

/-- Split a string into lines (on `'\n'`).

    $$\text{lines}(\text{"a\\nb\\nc"}) = [\text{"a"}, \text{"b"}, \text{"c"}]$$ -/
def lines (s : String) : List String :=
  s.splitOn "\n"

/-- Split a string into words (on whitespace).

    $$\text{words}(\text{"hello world"}) = [\text{"hello"}, \text{"world"}]$$ -/
def words (s : String) : List String :=
  let chars := s.toList
  let rec go (acc : List Char) (rest : List Char) (result : List String) : List String :=
    match rest with
    | [] => if acc.isEmpty then result.reverse else (String.mk acc.reverse :: result).reverse
    | c :: cs =>
      if c.isWhitespace then
        if acc.isEmpty then go [] cs result
        else go [] cs (String.mk acc.reverse :: result)
      else go (c :: acc) cs result
  go [] chars []

/-- Join lines with `'\n'`.

    $$\text{unlines}([\text{"a"}, \text{"b"}]) = \text{"a\\nb\\n"}$$ -/
def unlines (ls : List String) : String :=
  String.intercalate "\n" ls ++ if ls.isEmpty then "" else "\n"

/-- Join words with `' '`.

    $$\text{unwords}([\text{"hello"}, \text{"world"}]) = \text{"hello world"}$$ -/
def unwords (ws : List String) : String :=
  String.intercalate " " ws

-- ── Proofs ─────────────────────────────────────

/-- `unwords` of empty list is empty. -/
theorem unwords_nil : unwords [] = "" := rfl


end Data.DataString
