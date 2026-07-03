# Parse IUPAC-compact Structures

Parse IUPAC-compact strings into a
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html).

## Usage

``` r
parse_iupac_compact(x, on_failure = "error", progress = FALSE)
```

## Arguments

- x:

  A character vector of IUPAC-compact strings. NA values are allowed and
  will be returned as NA structures.

- on_failure:

  How to handle parsing failures. `"error"` aborts when a structure
  cannot be parsed. `"na"` returns `NA` at invalid positions.

- progress:

  Whether to show a progress bar while parsing.

## Value

A
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
object.

## Details

IUPAC-compact notation is similar to IUPAC-condensed notation, but
linkages are written directly after the monosaccharide, such as
`"Galb1-3GlcNAc"`, and branches are written in parentheses. The parser
normalizes compact notation into IUPAC-condensed notation, then uses the
IUPAC-condensed parser to construct the glycan structure.

Alditol glycans are parsed as regular reducing-end glycans with unknown
anomer configurations.

## See also

[`parse_iupac_condensed()`](https://glycoverse.github.io/glyparse/dev/reference/parse_iupac_condensed.md)

## Examples

``` r
iupac <- "Mana1-3(Mana1-6)Manb1-4GlcNAcb"
parse_iupac_compact(iupac)
#> <glycan_structure[1]>
#> [1] Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-
#> # Unique structures: 1
```
