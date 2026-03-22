/-
  Hale.HttpTypes.Network.HTTP.Types.Method — HTTP request methods
-/

namespace Network.HTTP.Types

/-- Standard HTTP methods (RFC 7231 + RFC 5789).
    $$\text{StdMethod} = \text{GET} \mid \text{POST} \mid \text{HEAD} \mid \ldots$$ -/
inductive StdMethod where
  | GET | POST | HEAD | PUT | DELETE | TRACE | CONNECT | OPTIONS | PATCH
deriving BEq, Repr, Inhabited

instance : ToString StdMethod where
  toString
    | .GET     => "GET"
    | .POST    => "POST"
    | .HEAD    => "HEAD"
    | .PUT     => "PUT"
    | .DELETE  => "DELETE"
    | .TRACE   => "TRACE"
    | .CONNECT => "CONNECT"
    | .OPTIONS => "OPTIONS"
    | .PATCH   => "PATCH"

/-- An HTTP method: either a standard method or a custom string. -/
inductive Method where
  | standard : StdMethod → Method
  | custom : String → Method
deriving BEq, Repr

instance : ToString Method where
  toString
    | .standard m => toString m
    | .custom s => s

/-- Parse a string to a Method. Known methods return `standard`, others `custom`. -/
def parseMethod (s : String) : Method :=
  match s with
  | "GET"     => .standard .GET
  | "POST"    => .standard .POST
  | "HEAD"    => .standard .HEAD
  | "PUT"     => .standard .PUT
  | "DELETE"  => .standard .DELETE
  | "TRACE"   => .standard .TRACE
  | "CONNECT" => .standard .CONNECT
  | "OPTIONS" => .standard .OPTIONS
  | "PATCH"   => .standard .PATCH
  | other     => .custom other

/-- Render a method to its canonical string form. -/
@[inline] def renderMethod (m : Method) : String := toString m

end Network.HTTP.Types
