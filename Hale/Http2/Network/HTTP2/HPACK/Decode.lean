/-
  Hale.Http2.Network.HTTP2.HPACK.Decode — HPACK header decoding

  Decodes HPACK wire format into header lists as defined in RFC 7541.

  ## Design

  Parses the HPACK integer and string primitives, then dispatches on the
  leading bits of each byte to determine the representation type.

  ## Guarantees

  - Integer decoding handles multi-byte variable-length encoding correctly
  - String decoding handles both raw and Huffman-encoded strings
  - Header block decoding processes all bytes and returns remaining table state

  ## Haskell equivalent
  `Network.HTTP2.HPACK.Decode` (https://hackage.haskell.org/package/http2)
-/
import Hale.Http2.Network.HTTP2.HPACK.Table
import Hale.Http2.Network.HTTP2.HPACK.Huffman

namespace Network.HTTP2.HPACK

-- ── Integer decoding (RFC 7541 Section 5.1) ────────────

/-- Decode result: parsed value and the number of bytes consumed. -/
structure DecodeResult (α : Type) where
  /-- The decoded value. -/
  value : α
  /-- Number of bytes consumed from input. -/
  consumed : Nat
  deriving Repr

/-- Decode an integer with the given prefix size (1-8 bits).
    Returns the decoded integer and number of bytes consumed.

    $$\text{decodeInteger} : \text{ByteArray} \to \text{Nat} \to \text{Nat} \to \text{Option}(\text{DecodeResult}(\text{Nat}))$$ -/
def decodeInteger (bs : ByteArray) (offset : Nat) (prefixBits : Nat) : Option (DecodeResult Nat) :=
  if offset >= bs.size then none
  else
    let maxPrefix := (1 <<< prefixBits) - 1
    let mask := maxPrefix.toUInt8
    let firstByte := bs[offset]! &&& mask
    if firstByte.toNat < maxPrefix then
      some { value := firstByte.toNat, consumed := 1 }
    else
      -- Multi-byte encoding
      let rec go (pos : Nat) (value : Nat) (shift : Nat) (fuel : Nat) : Option (DecodeResult Nat) :=
        match fuel with
        | 0 => none  -- Too many continuation bytes
        | fuel' + 1 =>
          if pos >= bs.size then none
          else
            let byte := bs[pos]!
            let contrib := (byte &&& 0x7F).toNat
            let value' := value + (contrib <<< shift)
            if (byte &&& 0x80) == 0 then
              some { value := value', consumed := pos - offset + 1 }
            else
              go (pos + 1) value' (shift + 7) fuel'
      go (offset + 1) maxPrefix 0 10

-- ── String decoding (RFC 7541 Section 5.2) ─────────────

/-- Decode a string from HPACK format. The high bit of the first byte indicates
    Huffman encoding.

    $$\text{decodeString} : \text{ByteArray} \to \text{Nat} \to \text{Option}(\text{DecodeResult}(\text{String}))$$ -/
def decodeString (bs : ByteArray) (offset : Nat) : Option (DecodeResult String) :=
  if offset >= bs.size then none
  else
    let firstByte := bs[offset]!
    let isHuffman := (firstByte &&& 0x80) != 0
    match decodeInteger bs offset 7 with
    | none => none
    | some lenResult =>
      let strLen := lenResult.value
      let dataStart := offset + lenResult.consumed
      if dataStart + strLen > bs.size then none
      else
        let raw := bs.extract dataStart (dataStart + strLen)
        let str := if isHuffman then
          match huffmanDecode raw with
          | some s => s
          | none => match String.fromUTF8? raw with
            | some s => s
            | none => ""
        else
          match String.fromUTF8? raw with
          | some s => s
          | none => ""
        some { value := str, consumed := lenResult.consumed + strLen }

-- ── Header block decoding ──────────────────────────────

/-- Decode a complete HPACK header block into a list of header fields.
    Updates the dynamic table as fields with indexing are decoded.

    $$\text{decodeHeaders} : \text{DynamicTable} \to \text{ByteArray} \to \text{Option}(\text{List}(\text{HeaderField}) \times \text{DynamicTable})$$ -/
def decodeHeaders (dt : DynamicTable) (bs : ByteArray) : Option (List HeaderField × DynamicTable) :=
  let rec go (offset : Nat) (dt : DynamicTable) (acc : List HeaderField) (fuel : Nat) :
      Option (List HeaderField × DynamicTable) :=
    match fuel with
    | 0 => some (acc.reverse, dt)  -- Safety limit reached
    | fuel' + 1 =>
      if offset >= bs.size then some (acc.reverse, dt)
      else
        let byte := bs[offset]!
        if (byte &&& 0x80) != 0 then
          -- Indexed Header Field (Section 6.1): 1xxxxxxx
          match decodeInteger bs offset 7 with
          | none => none
          | some idxResult =>
            let idx := idxResult.value
            match indexLookup dt idx with
            | none => none
            | some field =>
              go (offset + idxResult.consumed) dt (field :: acc) fuel'
        else if (byte &&& 0xC0) == 0x40 then
          -- Literal with Incremental Indexing (Section 6.2.1): 01xxxxxx
          match decodeInteger bs offset 6 with
          | none => none
          | some idxResult =>
            let nameIdx := idxResult.value
            let pos := offset + idxResult.consumed
            if nameIdx > 0 then
              -- Name from index
              match indexLookup dt nameIdx with
              | none => none
              | some (name, _) =>
                match decodeString bs pos with
                | none => none
                | some valResult =>
                  let field := (name, valResult.value)
                  let dt' := dt.insert name valResult.value
                  go (pos + valResult.consumed) dt' (field :: acc) fuel'
            else
              -- New name
              match decodeString bs pos with
              | none => none
              | some nameResult =>
                let pos' := pos + nameResult.consumed
                match decodeString bs pos' with
                | none => none
                | some valResult =>
                  let field := (nameResult.value, valResult.value)
                  let dt' := dt.insert nameResult.value valResult.value
                  go (pos' + valResult.consumed) dt' (field :: acc) fuel'
        else if (byte &&& 0xE0) == 0x20 then
          -- Dynamic Table Size Update (Section 6.3): 001xxxxx
          match decodeInteger bs offset 5 with
          | none => none
          | some sizeResult =>
            let dt' := dt.resize sizeResult.value
            go (offset + sizeResult.consumed) dt' acc fuel'
        else
          -- Literal without Indexing (Section 6.2.2): 0000xxxx
          -- or Literal Never Indexed (Section 6.2.3): 0001xxxx
          let prefixBits := 4
          match decodeInteger bs offset prefixBits with
          | none => none
          | some idxResult =>
            let nameIdx := idxResult.value
            let pos := offset + idxResult.consumed
            if nameIdx > 0 then
              match indexLookup dt nameIdx with
              | none => none
              | some (name, _) =>
                match decodeString bs pos with
                | none => none
                | some valResult =>
                  let field := (name, valResult.value)
                  go (pos + valResult.consumed) dt (field :: acc) fuel'
            else
              match decodeString bs pos with
              | none => none
              | some nameResult =>
                let pos' := pos + nameResult.consumed
                match decodeString bs pos' with
                | none => none
                | some valResult =>
                  let field := (nameResult.value, valResult.value)
                  go (pos' + valResult.consumed) dt (field :: acc) fuel'
  go 0 dt [] (bs.size + 1)

end Network.HTTP2.HPACK
