import Hale
import Tests.Harness

open Network.HTTP.Date Tests

/-
  Coverage:
  - Proofs: None
  - Tested: parseHTTPDate (IMF-fixdate, asctime), formatHTTPDate, roundtrip
  - Not covered: RFC 850 format
-/

namespace TestHttpDate

def tests : List TestResult :=
  let imf := "Sun, 06 Nov 1994 08:49:37 GMT"
  let asc := "Sun Nov  6 08:49:37 1994"
  let parsed := HTTPDate.parseHTTPDate imf
  let parsedAsc := HTTPDate.parseHTTPDate asc
  [ -- IMF-fixdate parsing
    check "parse IMF-fixdate" parsed.isSome
  , checkEq "IMF year" (some 1994) (parsed.map (·.year))
  , checkEq "IMF month" (some 11) (parsed.map (·.month))
  , checkEq "IMF day" (some 6) (parsed.map (·.day))
  , checkEq "IMF hour" (some 8) (parsed.map (·.hour))
  , checkEq "IMF minute" (some 49) (parsed.map (·.minute))
  , checkEq "IMF second" (some 37) (parsed.map (·.second))
  -- asctime parsing
  , check "parse asctime" parsedAsc.isSome
  , checkEq "asctime year" (some 1994) (parsedAsc.map (·.year))
  , checkEq "asctime month" (some 11) (parsedAsc.map (·.month))
  -- Format
  , let d : HTTPDate := ⟨1994, 11, 6, 8, 49, 37⟩
    checkEq "format IMF-fixdate" imf (d.formatHTTPDate)
  -- Invalid
  , check "parse invalid" (HTTPDate.parseHTTPDate "not a date" |>.isNone)
  , check "parse bad month" (HTTPDate.parseHTTPDate "Sun, 06 Xxx 1994 08:49:37 GMT" |>.isNone)
  ]

end TestHttpDate
