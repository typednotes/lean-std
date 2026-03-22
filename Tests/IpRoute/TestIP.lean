import Hale
import Tests.Harness

open Data.IP Tests

/-
  Coverage:
  - Proofs: None yet
  - Tested: IPv4 construction, octets, ToString, parsing, CIDR matching
  - Not covered: IPv6 operations, routing table
-/

namespace TestIP

def tests : List TestResult :=
  let ip := IPv4.ofOctets 192 168 1 100
  let (a, b, c, d) := ip.toOctets
  let cidr := AddrRange4.mk (IPv4.ofOctets 192 168 1 0) 24 (by omega)
  [ -- IPv4 construction
    checkEq "IPv4 toString" "192.168.1.100" (toString ip)
  , checkEq "IPv4 octet a" 192 a.toNat
  , checkEq "IPv4 octet b" 168 b.toNat
  , checkEq "IPv4 octet c" 1 c.toNat
  , checkEq "IPv4 octet d" 100 d.toNat
  -- Constants
  , checkEq "IPv4 loopback" "127.0.0.1" (toString IPv4.loopback)
  , checkEq "IPv4 any" "0.0.0.0" (toString IPv4.any)
  -- Parsing
  , check "parseIPv4 valid" (parseIPv4 "10.0.0.1" |>.isSome)
  , check "parseIPv4 invalid" (parseIPv4 "256.0.0.1" |>.isNone)
  , check "parseIPv4 bad format" (parseIPv4 "abc" |>.isNone)
  -- CIDR matching
  , check "CIDR match in range" (cidr.isMatchedTo (IPv4.ofOctets 192 168 1 50))
  , check "CIDR match base" (cidr.isMatchedTo (IPv4.ofOctets 192 168 1 0))
  , check "CIDR no match" (!cidr.isMatchedTo (IPv4.ofOctets 192 168 2 1))
  -- CIDR /32
  , let host := AddrRange4.mk ip 32 (by omega)
    check "CIDR /32 matches self" (host.isMatchedTo ip)
  , let host32 := AddrRange4.mk ip 32 (by omega)
    check "CIDR /32 no match other" (!host32.isMatchedTo (IPv4.ofOctets 192 168 1 101))
  -- CIDR /0 matches everything
  , let all := AddrRange4.mk IPv4.any 0 (by omega)
    check "CIDR /0 matches all" (all.isMatchedTo (IPv4.ofOctets 8 8 8 8))
  -- Parse CIDR
  , check "parseCIDR4 valid" (parseCIDR4 "10.0.0.0/8" |>.isSome)
  , check "parseCIDR4 invalid mask" (parseCIDR4 "10.0.0.0/33" |>.isNone)
  -- ToString
  , checkEq "CIDR toString" "192.168.1.0/24" (toString cidr)
  ]

end TestIP
