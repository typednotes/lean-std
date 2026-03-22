/-
  Hale.Http2.Network.HTTP2.HPACK.Table — HPACK static and dynamic tables

  Implements the HPACK header compression tables as defined in RFC 7541.

  ## Design

  The static table is a compile-time constant array of 61 entries.
  The dynamic table is a bounded FIFO queue with eviction when the size
  exceeds the maximum allowed by SETTINGS_HEADER_TABLE_SIZE.

  ## Guarantees

  - Static table indices are 1-based (1..61), matching RFC 7541 Appendix A
  - Dynamic table maintains the invariant `currentSize <= maxSize`
  - Eviction is automatic on insertion

  ## Haskell equivalent
  `Network.HTTP2.HPACK.Table` (https://hackage.haskell.org/package/http2)
-/

namespace Network.HTTP2.HPACK

/-- A header field is a name-value pair of strings.
    $$\text{HeaderField} = \text{String} \times \text{String}$$ -/
abbrev HeaderField := String × String

/-- The HPACK static table as defined in RFC 7541 Appendix A.
    Index 1-based, containing 61 pre-defined header fields. -/
def staticTable : Array HeaderField := #[
  -- Index 1
  (":authority", ""),
  -- Index 2
  (":method", "GET"),
  -- Index 3
  (":method", "POST"),
  -- Index 4
  (":path", "/"),
  -- Index 5
  (":path", "/index.html"),
  -- Index 6
  (":scheme", "http"),
  -- Index 7
  (":scheme", "https"),
  -- Index 8
  (":status", "200"),
  -- Index 9
  (":status", "204"),
  -- Index 10
  (":status", "206"),
  -- Index 11
  (":status", "304"),
  -- Index 12
  (":status", "400"),
  -- Index 13
  (":status", "404"),
  -- Index 14
  (":status", "500"),
  -- Index 15
  ("accept-charset", ""),
  -- Index 16
  ("accept-encoding", "gzip, deflate"),
  -- Index 17
  ("accept-language", ""),
  -- Index 18
  ("accept-ranges", ""),
  -- Index 19
  ("accept", ""),
  -- Index 20
  ("access-control-allow-origin", ""),
  -- Index 21
  ("age", ""),
  -- Index 22
  ("allow", ""),
  -- Index 23
  ("authorization", ""),
  -- Index 24
  ("cache-control", ""),
  -- Index 25
  ("content-disposition", ""),
  -- Index 26
  ("content-encoding", ""),
  -- Index 27
  ("content-language", ""),
  -- Index 28
  ("content-length", ""),
  -- Index 29
  ("content-location", ""),
  -- Index 30
  ("content-range", ""),
  -- Index 31
  ("content-type", ""),
  -- Index 32
  ("cookie", ""),
  -- Index 33
  ("date", ""),
  -- Index 34
  ("etag", ""),
  -- Index 35
  ("expect", ""),
  -- Index 36
  ("expires", ""),
  -- Index 37
  ("from", ""),
  -- Index 38
  ("host", ""),
  -- Index 39
  ("if-match", ""),
  -- Index 40
  ("if-modified-since", ""),
  -- Index 41
  ("if-none-match", ""),
  -- Index 42
  ("if-range", ""),
  -- Index 43
  ("if-unmodified-since", ""),
  -- Index 44
  ("last-modified", ""),
  -- Index 45
  ("link", ""),
  -- Index 46
  ("location", ""),
  -- Index 47
  ("max-forwards", ""),
  -- Index 48
  ("proxy-authenticate", ""),
  -- Index 49
  ("proxy-authorization", ""),
  -- Index 50
  ("range", ""),
  -- Index 51
  ("referer", ""),
  -- Index 52
  ("refresh", ""),
  -- Index 53
  ("retry-after", ""),
  -- Index 54
  ("server", ""),
  -- Index 55
  ("set-cookie", ""),
  -- Index 56
  ("strict-transport-security", ""),
  -- Index 57
  ("transfer-encoding", ""),
  -- Index 58
  ("user-agent", ""),
  -- Index 59
  ("vary", ""),
  -- Index 60
  ("via", ""),
  -- Index 61
  ("www-authenticate", "")
]

/-- Size of the static table. Always 61. -/
def staticTableSize : Nat := 61

/-- Look up an entry in the static table by 1-based index.
    $$\text{staticLookup}(i) = \text{staticTable}[i-1]$$ for $1 \leq i \leq 61$.
    Returns `none` for out-of-range indices. -/
def staticLookup (index : Nat) : Option HeaderField :=
  if index >= 1 && index <= staticTableSize then
    staticTable[index - 1]?
  else none

/-- The HPACK dynamic table. Entries are stored most-recent-first.
    The table has a maximum size in octets, and entries are evicted
    from the end (oldest) when the size would exceed the maximum.

    Size of an entry: `name.length + value.length + 32` (RFC 7541 Section 4.1). -/
structure DynamicTable where
  /-- Entries stored most-recent-first. -/
  entries : Array HeaderField
  /-- Current size in octets. -/
  currentSize : Nat
  /-- Maximum size in octets (from SETTINGS_HEADER_TABLE_SIZE). -/
  maxSize : Nat
  deriving Repr

/-- Calculate the HPACK entry size per RFC 7541 Section 4.1.
    $$\text{entrySize}(n, v) = |n| + |v| + 32$$ -/
@[inline] def entrySize (name value : String) : Nat :=
  name.length + value.length + 32

namespace DynamicTable

/-- Create an empty dynamic table with the given maximum size.
    $$\text{empty}(m) = \{ \text{entries} = [], \text{currentSize} = 0, \text{maxSize} = m \}$$ -/
def empty (maxSize : Nat) : DynamicTable :=
  { entries := #[], currentSize := 0, maxSize := maxSize }

instance : Inhabited DynamicTable := ⟨empty 4096⟩

/-- Get the number of entries in the dynamic table. -/
@[inline] def size (dt : DynamicTable) : Nat := dt.entries.size

/-- Look up an entry by 0-based index (0 = most recent).
    $$\text{lookup}(dt, i) = dt.\text{entries}[i]$$ -/
def lookup (dt : DynamicTable) (index : Nat) : Option HeaderField :=
  dt.entries[index]?

/-- Evict entries from the end of the table until `currentSize + newEntrySize ≤ targetMax`.
    Returns the updated table. -/
private def evict (dt : DynamicTable) (targetMax : Nat) : DynamicTable :=
  let rec go (entries : Array HeaderField) (curSize : Nat) (fuel : Nat) :
      Array HeaderField × Nat :=
    match fuel with
    | 0 => (entries, curSize)
    | fuel' + 1 =>
      if curSize ≤ targetMax then (entries, curSize)
      else if entries.size == 0 then (#[], 0)
      else
        let last : HeaderField := entries.getD (entries.size - 1) ("", "")
        let lastSize := entrySize last.1 last.2
        let entries' := entries.pop
        let curSize' := if curSize ≥ lastSize then curSize - lastSize else 0
        go entries' curSize' fuel'
  let (entries', curSize') := go dt.entries dt.currentSize dt.entries.size
  { entries := entries'
    currentSize := curSize'
    maxSize := dt.maxSize }

/-- Insert a new entry at the front of the dynamic table.
    Evicts old entries as needed to maintain the size invariant.
    If the entry itself is larger than maxSize, the table is emptied.

    $$\text{insert}(dt, n, v) = \text{evict}(\{n:v\} :: dt.\text{entries})$$ -/
def insert (dt : DynamicTable) (name value : String) : DynamicTable :=
  let eSize := entrySize name value
  if eSize > dt.maxSize then
    -- Entry too large: empty the table per RFC 7541 Section 4.4
    { entries := #[], currentSize := 0, maxSize := dt.maxSize }
  else
    let dt' := dt.evict (dt.maxSize - eSize)
    { entries := #[(name, value)] ++ dt'.entries
      currentSize := dt'.currentSize + eSize
      maxSize := dt.maxSize }

/-- Resize the dynamic table to a new maximum size. Evicts entries if needed.
    $$\text{resize}(dt, m) = \text{evict}(dt, m)$$ with updated maxSize. -/
def resize (dt : DynamicTable) (newMaxSize : Nat) : DynamicTable :=
  let dt' := { dt with maxSize := newMaxSize }
  if dt'.currentSize ≤ newMaxSize then dt'
  else dt'.evict newMaxSize

/-- Find a header field in the dynamic table. Returns the 0-based index if found.
    Searches for exact (name, value) match first, then name-only match. -/
def find (dt : DynamicTable) (name value : String) : Option (Nat × Bool) :=
  -- First pass: exact match
  let exactIdx := dt.entries.findIdx? (fun (n, v) => n == name && v == value)
  match exactIdx with
  | some idx => some (idx, true)
  | none =>
    -- Second pass: name-only match
    let nameIdx := dt.entries.findIdx? (fun (n, _) => n == name)
    match nameIdx with
    | some idx => some (idx, false)
    | none => none

end DynamicTable

/-- Look up a header field by HPACK index (1-based, static table first, then dynamic).
    $$\text{indexLookup}(dt, i) = \begin{cases}
      \text{staticTable}[i-1] & \text{if } 1 \leq i \leq 61 \\
      \text{dt.entries}[i-62] & \text{if } i > 61
    \end{cases}$$ -/
def indexLookup (dt : DynamicTable) (index : Nat) : Option HeaderField :=
  if index <= staticTableSize then
    staticLookup index
  else
    dt.lookup (index - staticTableSize - 1)

/-- Find a header field in the combined static + dynamic tables.
    Returns `(index, exactMatch)` where index is 1-based HPACK index. -/
def findInTables (dt : DynamicTable) (name value : String) : Option (Nat × Bool) :=
  -- Search static table first
  let staticResult := do
    let exactIdx := staticTable.findIdx? (fun (n, v) => n == name && v == value)
    match exactIdx with
    | some idx => some (idx + 1, true)
    | none =>
      let nameIdx := staticTable.findIdx? (fun (n, _) => n == name)
      match nameIdx with
      | some idx => some (idx + 1, false)
      | none => none
  match staticResult with
  | some (idx, true) => some (idx, true)
  | staticNameMatch =>
    -- Search dynamic table
    match dt.find name value with
    | some (dIdx, true) => some (dIdx + staticTableSize + 1, true)
    | some (dIdx, false) =>
      -- Prefer static name match over dynamic name match
      match staticNameMatch with
      | some result => some result
      | none => some (dIdx + staticTableSize + 1, false)
    | none => staticNameMatch

end Network.HTTP2.HPACK
