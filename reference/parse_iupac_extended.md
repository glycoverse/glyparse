# Parse IUPAC-extended Structures

Parse IUPAC-extended-style structure characters into a
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html).
For more information about IUPAC-extended format, see
[doi:10.1351/pac199668101919](https://doi.org/10.1351/pac199668101919) .

## Usage

``` r
parse_iupac_extended(x)
```

## Arguments

- x:

  A character vector of IUPAC-extended strings.

## Value

A
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
object.

## See also

[`parse_iupac_condensed()`](https://glycoverse.github.io/glyparse/reference/parse_iupac_condensed.md),
[`parse_iupac_short()`](https://glycoverse.github.io/glyparse/reference/parse_iupac_short.md)

## Examples

``` r
iupac <- "\u03b2-D-Galp-(1\u21923)-\u03b1-D-GalpNAc-(1\u2192"
parse_iupac_extended(iupac)
#> <glycan_structure[1]>
#> [1] Gal(b1-3)GalNAc(a1-
#> # Unique structures: 1
```
