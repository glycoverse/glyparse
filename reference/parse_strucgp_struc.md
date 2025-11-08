# Parse StrucGP Structures

Parse StrucGP-style structure characters into a
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html).
See example below for the structure format.

## Usage

``` r
parse_strucgp_struc(x)
```

## Arguments

- x:

  A character vector of StrucGP-style structure strings.

## Value

A
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
object.

## Examples

``` r
glycan <- parse_strucgp_struc("A2B2C1D1E2F1fedD1E2edcbB5ba")
print(glycan, verbose = TRUE)
#> <glycan_structure[1]>
#> [1] Hex(??-?)HexNAc(??-?)Hex(??-?)[HexNAc(??-?)Hex(??-?)]Hex(??-?)HexNAc(??-?)[dHex(??-?)]HexNAc(??-
#> # Unique structures: 1
```
