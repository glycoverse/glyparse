# Parse pGlyco Structures

Parse pGlyco-style structure characters into a
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html).
See example below for the structure format.

## Usage

``` r
parse_pglyco_struc(x)
```

## Arguments

- x:

  A character vector of pGlyco-style structure strings.

## Value

A
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
object.

## Examples

``` r
glycan <- parse_pglyco_struc("(N(F)(N(H(H(N))(H(N(H))))))")
print(glycan, verbose = TRUE)
#> <glycan_structure[1]>
#> [1] Hex(??-?)HexNAc(??-?)Hex(??-?)[HexNAc(??-?)Hex(??-?)]Hex(??-?)HexNAc(??-?)[dHex(??-?)]HexNAc(??-
#> # Unique structures: 1
```
