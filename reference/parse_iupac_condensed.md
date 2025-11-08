# Parse IUPAC-condensed Structures

This function parses IUPAC-condensed strings into a
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html).
For more information about IUPAC-condensed notation, see
[doi:10.1351/pac199668101919](https://doi.org/10.1351/pac199668101919) .

## Usage

``` r
parse_iupac_condensed(x)
```

## Arguments

- x:

  A character vector of IUPAC-condensed strings.

## Value

A
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
object.

## Details

The IUPAC-condensed notation is a compact form of IUPAC-extended
notation. It is used by the [GlyConnect](https://glyconnect.expasy.org/)
database. It contains the following information:

- Monosaccharide name, e.g. "Gal", "GlcNAc", "Neu5Ac".

- Substituent, e.g. "9Ac", "4Ac", "3Me", "?S".

- Linkage, e.g. "b1-3", "a1-2", "a1-?".

An example of IUPAC-condensed string is "Gal(b1-3)GlcNAc(b1-4)Glc(a1-".

The reducing-end monosaccharide can be with or without anomer
information. For example, the two strings below are all valid:

- "Neu5Ac(a2-"

- "Neu5Ac"

In the first case, the anomer is "a2". In the second case, the anomer is
"?2".

## See also

[`parse_iupac_short()`](https://glycoverse.github.io/glyparse/reference/parse_iupac_short.md),
[`parse_iupac_extended()`](https://glycoverse.github.io/glyparse/reference/parse_iupac_extended.md)

## Examples

``` r
iupac <- "Gal(b1-3)GlcNAc(b1-4)Glc(a1-"
parse_iupac_condensed(iupac)
#> <glycan_structure[1]>
#> [1] Gal(b1-3)GlcNAc(b1-4)Glc(a1-
#> # Unique structures: 1
```
