# Parse IUPAC-short Structures

Parse IUPAC-short-style structure characters into a
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html).
For more information about IUPAC-short format, see
[doi:10.1351/pac199668101919](https://doi.org/10.1351/pac199668101919) .

## Usage

``` r
parse_iupac_short(x)
```

## Arguments

- x:

  A character vector of IUPAC-short strings. NA values are allowed and
  will be returned as NA structures.

## Value

A
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
object.

## Details

The IUPAC-short notation is a compact form of IUPAC-condensed notation.
It is rarely used in database, but appears a lot in literature for its
conciseness. Compared with IUPAC-condensed notation, IUPAC-short
notation ignore the anomer positions, assuming they are known for common
monosaccharides. For example, "Neu5Aca3Gala-" assumes the anomer of
Neu5Ac is C2 (a2-3 linked). Also, the parentheses around linkages are
omitted, and parentheses are used to indicate branching, e.g.
"Neu5Aca3Gala3(Fuca3)GlcNAcb-".

In the first case, the anomer is "a2". In the second case, the anomer is
"?2".

## See also

[`parse_iupac_condensed()`](https://glycoverse.github.io/glyparse/reference/parse_iupac_condensed.md),
[`parse_iupac_extended()`](https://glycoverse.github.io/glyparse/reference/parse_iupac_extended.md)

## Examples

``` r
iupac <- "Neu5Aca3Gala3(Fuca6)GlcNAcb-"
parse_iupac_short(iupac)
#> <glycan_structure[1]>
#> [1] Neu5Ac(a2-3)Gal(a1-3)[Fuc(a1-6)]GlcNAc(b1-
#> # Unique structures: 1
```
