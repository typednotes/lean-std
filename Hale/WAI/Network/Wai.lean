/-
  Hale.WAI.Network.Wai — Web Application Interface

  Public API for WAI. Re-exports core types and provides convenience functions.
-/

import Hale.WAI.Network.Wai.Internal

namespace Network.Wai

open Network.HTTP.Types

/-- Create a simple response from a status, headers, and body string. -/
def responseLBS (status : Status)
    (headers : ResponseHeaders) (body : String) : Response :=
  .responseBuilder status headers body.toUTF8

/-- Create a file response. -/
def responseFile' (status : Status)
    (headers : ResponseHeaders)
    (path : String) (part : Option Network.Sendfile.FilePart := none) : Response :=
  .responseFile status headers path part

/-- Create a streaming response. -/
def responseStream' (status : Status)
    (headers : ResponseHeaders)
    (body : StreamingBody) : Response :=
  .responseStream status headers body

/-- Get a header value from a request by name. -/
def requestHeader (name : HeaderName)
    (req : Request) : Option String :=
  req.requestHeaders.find? (fun (n, _) => n == name) |>.map (·.2)

/-- The identity middleware (does nothing). -/
def idMiddleware : Middleware := id

/-- Compose two middlewares.
    $$\text{composeMiddleware}(f, g) = f \circ g$$ -/
@[inline] def composeMiddleware (f g : Middleware) : Middleware := f ∘ g

/-- Add a header to the response. -/
def addHeader (name : HeaderName) (val : String)
    (resp : Response) : Response :=
  resp.mapResponseHeaders ((name, val) :: ·)

end Network.Wai
