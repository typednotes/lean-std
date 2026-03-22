/-
  Hale.Warp.Network.Wai.Handler.Warp.Request — HTTP request parsing

  Ports Haskell's Warp request parser. Reads from a socket line-by-line,
  parses the request line and headers, and constructs a `Network.Wai.Request`.

  ## Design

  Uses a simple line-based parser: read CRLF-terminated lines from the socket
  until an empty line is encountered (end of headers). The first line is parsed
  as the request line (method, path, query, version), and subsequent lines are
  parsed as headers.

  ## Guarantees

  - `parseRequestLine` returns `none` for malformed input (total function)
  - `parseHeaders` is total and handles malformed header lines gracefully
  - `parseHttpVersion` validates the "HTTP/x.y" format
-/

import Hale.WAI
import Hale.HttpTypes
import Hale.Network

namespace Network.Wai.Handler.Warp

open Network.HTTP.Types
open Network.Wai
open Network.Socket

/-- Parse an HTTP version string like "HTTP/1.1".
    $$\text{parseHttpVersion} : \text{String} \to \text{Option}(\text{HttpVersion})$$ -/
def parseHttpVersion (s : String) : Option HttpVersion :=
  if s == "HTTP/1.1" then some http11
  else if s == "HTTP/1.0" then some http10
  else if s == "HTTP/0.9" then some http09
  else if s == "HTTP/2.0" then some http20
  else if s.startsWith "HTTP/" then
    let rest := (s.drop 5).toString
    match rest.splitOn "." with
    | [maj, min] => do
      let major ← maj.toNat?
      let minor ← min.toNat?
      some ⟨major, minor⟩
    | _ => none
  else none

/-- Parse a request line like "GET /path?query HTTP/1.1".
    Returns (method, rawPath, rawQuery, version) or `none` if malformed.
    $$\text{parseRequestLine} : \text{String} \to \text{Option}(\text{Method} \times \text{String} \times \text{String} \times \text{HttpVersion})$$ -/
def parseRequestLine (line : String) : Option (Method × String × String × HttpVersion) := do
  let parts := line.splitOn " "
  match parts with
  | [methodStr, uri, versionStr] =>
    let method := parseMethod methodStr
    let version ← parseHttpVersion versionStr
    -- Split URI into path and query
    let (path, query) :=
      match uri.splitOn "?" with
      | [p] => (p, "")
      | [p, q] => (p, "?" ++ q)
      | _ => (uri, "")
    some (method, path, query, version)
  | _ => none

/-- Parse a single header line like "Content-Type: text/html".
    Returns `none` if the line doesn't contain a colon.
    $$\text{parseHeaderLine} : \text{String} \to \text{Option}(\text{Header})$$ -/
def parseHeaderLine (line : String) : Option Header :=
  match line.splitOn ":" with
  | [] => none
  | [_] => none
  | name :: rest =>
    let value := (":".intercalate rest).trimAscii.toString
    some (Data.CI.mk' name.trimAscii.toString, value)

/-- Parse header lines into a list of headers.
    $$\text{parseHeaders} : \text{List}(\text{String}) \to \text{RequestHeaders}$$ -/
def parseHeaders (lines : List String) : RequestHeaders :=
  lines.filterMap parseHeaderLine

/-- Read a CRLF-terminated line from a socket. Buffers data until "\r\n" is found.
    Returns the line without the CRLF terminator.
    $$\text{recvLine} : \text{Socket} \to \text{IO}(\text{String})$$ -/
def recvLine (sock : Socket) : IO String := do
  let mut buf := ByteArray.empty
  let mut found := false
  while !found do
    let chunk ← Network.Socket.recv sock 1
    if chunk.size == 0 then
      found := true  -- Connection closed
    else
      buf := buf ++ chunk
      -- Check if buffer ends with \r\n
      if buf.size >= 2 then
        if buf.get! (buf.size - 2) == 13 && buf.get! (buf.size - 1) == 10 then
          found := true
  -- Strip trailing \r\n
  if buf.size >= 2 && buf.get! (buf.size - 2) == 13 && buf.get! (buf.size - 1) == 10 then
    pure (String.fromUTF8! (buf.extract 0 (buf.size - 2)))
  else
    pure (String.fromUTF8! buf)

/-- Read all header lines from a socket until an empty line.
    Returns the request line and header lines.
    $$\text{recvHeaders} : \text{Socket} \to \text{IO}(\text{String} \times \text{List}(\text{String}))$$ -/
def recvHeaders (sock : Socket) : IO (String × List String) := do
  let requestLine ← recvLine sock
  let mut headers : List String := []
  let mut done := false
  while !done do
    let line ← recvLine sock
    if line.isEmpty then
      done := true
    else
      headers := headers ++ [line]
  pure (requestLine, headers)

/-- Find a header value by name in a header list. -/
private def findHeader (name : HeaderName) (headers : RequestHeaders) : Option String :=
  headers.find? (fun (n, _) => n == name) |>.map (·.2)

/-- Parse a full HTTP request from a socket.
    Returns `none` if the request line is malformed or the connection is closed.
    $$\text{parseRequest} : \text{Socket} \to \text{SockAddr} \to \text{IO}(\text{Option}(\text{Request}))$$ -/
def parseRequest (sock : Socket) (remoteAddr : SockAddr) : IO (Option Request) := do
  let (requestLine, headerLines) ← recvHeaders sock
  if requestLine.isEmpty then
    return none
  match parseRequestLine requestLine with
  | none => return none
  | some (method, rawPath, rawQuery, version) =>
    let headers := parseHeaders headerLines
    -- Extract special headers
    let hostHeader := findHeader hHost headers
    let rangeHeader := findHeader hRange headers
    let refererHeader := findHeader hReferer headers
    let uaHeader := findHeader hUserAgent headers
    -- Parse content length
    let contentLength := do
      let clStr ← findHeader hContentLength headers
      clStr.toNat?
    -- Parse path segments
    let pathSegments :=
      let segs := rawPath.splitOn "/"
      segs.filter (! ·.isEmpty)
    -- Parse query string
    let query := parseQuery rawQuery
    -- Create a body reader: for now, read based on Content-Length
    -- If no Content-Length, return empty immediately
    let bodyRef ← IO.mkRef contentLength
    let bodyReader : IO ByteArray := do
      let remaining ← bodyRef.get
      match remaining with
      | none => pure ByteArray.empty
      | some 0 => pure ByteArray.empty
      | some n =>
        let toRead := min n 4096
        let chunk ← Network.Socket.recv sock toRead
        let newRemaining := n - chunk.size
        bodyRef.set (some newRemaining)
        pure chunk
    return some {
      requestMethod := method
      httpVersion := version
      rawPathInfo := rawPath
      rawQueryString := rawQuery
      requestHeaders := headers
      isSecure := false
      remoteHost := remoteAddr
      pathInfo := pathSegments
      queryString := query
      requestBody := bodyReader
      vault := Data.Vault.empty
      requestBodyLength := contentLength
      requestHeaderHost := hostHeader
      requestHeaderRange := rangeHeader
      requestHeaderReferer := refererHeader
      requestHeaderUserAgent := uaHeader
    }

end Network.Wai.Handler.Warp
