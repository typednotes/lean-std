/-
  Hale.Http3.Network.HTTP3.QPACK.Decode -- QPACK header decoding

  Decodes QPACK-encoded header fields (RFC 9204).
  This implementation handles static-table-only decoding.

  ## Design

  QPACK decoding recognises the three encoding formats:
  1. Indexed field line (static table)
  2. Literal field line with name reference (static table)
  3. Literal field line with literal name

  Since we only support static-table mode (Required Insert Count = 0),
  dynamic table references will produce an error.

  ## Guarantees

  - Decoding rejects Required Insert Count != 0 (dynamic table not supported)
  - Static table lookups are bounds-checked

  ## Haskell equivalent
  QPACK decoding from the `http3` package
-/

import Hale.Http3.Network.HTTP3.QPACK.Table

namespace Network.HTTP3.QPACK

/-- Helper: get byte at offset, returning 0 if out of bounds. -/
@[inline] private def getByte (buf : ByteArray) (i : Nat) : UInt8 :=
  if h : i < buf.size then buf[i] else 0

/-- Decode a QPACK integer with the given prefix bit width (RFC 9204 Section 4.1.1).
    $$\text{decodeQInt}(n, \text{buf}, \text{off}) = \text{Option}(\text{value} \times \text{bytesConsumed})$$ -/
def decodeQInt (prefixBits : Nat) (buf : ByteArray) (offset : Nat) : Option (Nat × Nat) :=
  if offset ≥ buf.size then none
  else
    let firstByte := getByte buf offset
    let mask := ((1 <<< prefixBits) - 1).toUInt8
    let prefixVal := (firstByte &&& mask).toNat
    let maxPfx := mask.toNat
    if prefixVal < maxPfx then
      some (prefixVal, 1)
    else
      -- Multi-byte integer
      let rec decodeMulti (pos : Nat) (value : Nat) (shift : Nat) (fuel : Nat) : Option (Nat × Nat) :=
        match fuel with
        | 0 => none
        | fuel' + 1 =>
          if pos ≥ buf.size then none
          else
            let b := getByte buf pos
            let value' := value + ((b &&& 0x7F).toNat <<< shift)
            if (b &&& 0x80) == 0 then
              some (value', pos + 1 - offset)
            else
              decodeMulti (pos + 1) value' (shift + 7) fuel'
      decodeMulti (offset + 1) maxPfx 0 (buf.size - offset)

/-- Decode a QPACK string literal (RFC 9204 Section 4.1.2).
    $$\text{decodeStringLiteral} : \text{ByteArray} \to \mathbb{N} \to \text{Option}(\text{String} \times \mathbb{N})$$ -/
def decodeStringLiteral (buf : ByteArray) (offset : Nat) : Option (String × Nat) := do
  let (strLen, lenBytes) ← decodeQInt 7 buf offset
  let strStart := offset + lenBytes
  let strEnd := strStart + strLen
  if strEnd ≤ buf.size then
    let payload := buf.extract strStart strEnd
    some (String.fromUTF8! payload, lenBytes + strLen)
  else none

/-- Decode headers from the encoded block. Uses fuel-based recursion. -/
private def decodeHeaderEntries (buf : ByteArray) (startPos : Nat) : Option (List HeaderField) :=
  let rec go (pos : Nat) (acc : List HeaderField) (fuel : Nat) : Option (List HeaderField) :=
    match fuel with
    | 0 => some acc
    | fuel' + 1 =>
      if pos ≥ buf.size then some acc
      else
        let firstByte := getByte buf pos
        if (firstByte &&& 0x80) != 0 then
          -- Indexed Field Line: 1Txxxxxx
          let isStatic := (firstByte &&& 0x40) != 0
          if !isStatic then none  -- Dynamic table reference not supported
          else
            match decodeQInt 6 buf pos with
            | none => none
            | some (idx, idxLen) =>
              match staticLookup idx with
              | none => none
              | some entry => go (pos + idxLen) (acc ++ [entry]) fuel'
        else if (firstByte &&& 0x40) != 0 then
          -- Literal Field Line With Name Reference: 01NTxxxx
          let isStatic := (firstByte &&& 0x10) != 0
          if !isStatic then none
          else
            match decodeQInt 4 buf pos with
            | none => none
            | some (nameIdx, nameIdxLen) =>
              match staticLookup nameIdx with
              | none => none
              | some (name, _) =>
                match decodeStringLiteral buf (pos + nameIdxLen) with
                | none => none
                | some (value, valueLen) =>
                  go (pos + nameIdxLen + valueLen) (acc ++ [(name, value)]) fuel'
        else if (firstByte &&& 0x20) != 0 then
          -- Literal Field Line With Literal Name: 001Nxxxx
          match decodeStringLiteral buf (pos + 1) with
          | none => none
          | some (name, nameLen) =>
            match decodeStringLiteral buf (pos + 1 + nameLen) with
            | none => none
            | some (value, valueLen) =>
              go (pos + 1 + nameLen + valueLen) (acc ++ [(name, value)]) fuel'
        else none  -- Not supported in static-only mode
  go startPos [] buf.size

/-- Decode a list of header fields from a QPACK-encoded header block.
    The input must start with the Encoded Field Section Prefix
    (Required Insert Count + Delta Base).
    $$\text{decodeHeaders} : \text{ByteArray} \to \text{Option}(\text{List HeaderField})$$ -/
def decodeHeaders (buf : ByteArray) : Option (List HeaderField) := do
  -- Decode Required Insert Count (must be 0 for static-only)
  let (reqInsertCount, ricLen) ← decodeQInt 8 buf 0
  if reqInsertCount != 0 then none  -- Dynamic table not supported
  -- Decode Delta Base (ignored when RIC = 0)
  let (_deltaBase, dbLen) ← decodeQInt 7 buf ricLen
  decodeHeaderEntries buf (ricLen + dbLen)

end Network.HTTP3.QPACK
