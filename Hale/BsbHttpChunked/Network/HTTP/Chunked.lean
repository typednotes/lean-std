/-
  Hale.BsbHttpChunked.Network.HTTP.Chunked — HTTP chunked transfer encoding

  Wraps data in HTTP/1.1 chunked transfer encoding framing.

  ## Design

  Mirrors Haskell's `bsb-http-chunked`. Works with ByteArrays directly.
  Each chunk is: `<hex-length>\r\n<data>\r\n`
  Terminator is: `0\r\n\r\n`
-/

namespace Network.HTTP.Chunked

/-- Encode a hex digit (0-15) as a character. -/
private def hexChar (n : Nat) : UInt8 :=
  if n < 10 then (48 + n).toUInt8  -- '0'-'9'
  else (87 + n).toUInt8              -- 'a'-'f'

/-- Encode a natural number as hex bytes. -/
private def natToHex (n : Nat) : ByteArray :=
  if n == 0 then ByteArray.mk #[48]  -- "0"
  else
    let rec go (n : Nat) (acc : List UInt8) (fuel : Nat) : List UInt8 :=
      match fuel with
      | 0 => acc
      | fuel' + 1 =>
        if n == 0 then acc
        else go (n / 16) (hexChar (n % 16) :: acc) fuel'
    let digits := go n [] 16
    ByteArray.mk digits.toArray

private def crlf : ByteArray := ByteArray.mk #[13, 10]  -- \r\n

/-- Wrap data in a single HTTP chunk.
    $$\text{chunkedTransferEncoding}(d) = \text{hex}(|d|) \cdot \texttt{\\r\\n} \cdot d \cdot \texttt{\\r\\n}$$

    Returns empty if the input is empty (no zero-length chunks;
    use `chunkedTransferTerminator` to end the transfer). -/
def chunkedTransferEncoding (data : ByteArray) : ByteArray :=
  if data.size == 0 then ByteArray.empty
  else
    let hexLen := natToHex data.size
    hexLen ++ crlf ++ data ++ crlf

/-- The chunked transfer encoding terminator.
    $$\text{chunkedTransferTerminator} = \texttt{0\\r\\n\\r\\n}$$ -/
def chunkedTransferTerminator : ByteArray :=
  ByteArray.mk #[48, 13, 10, 13, 10]  -- "0\r\n\r\n"

/-- Encode a list of chunks into a complete chunked transfer body.
    $$\text{encodeChunked}([c_1, \ldots, c_n]) = \text{chunk}(c_1) \cdots \text{chunk}(c_n) \cdot \text{terminator}$$ -/
def encodeChunked (chunks : List ByteArray) : ByteArray :=
  let encoded := chunks.foldl (fun acc chunk => acc ++ chunkedTransferEncoding chunk) ByteArray.empty
  encoded ++ chunkedTransferTerminator

end Network.HTTP.Chunked
