# Parse GlyCAM IUPAC Structures

Parse GlyCAM IUPAC-style structure strings into a
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html).

## Usage

``` r
parse_glycam_iupac(x, on_failure = "error")
```

## Arguments

- x:

  A character vector of GlyCAM IUPAC strings. NA values are allowed and
  will be returned as NA structures.

- on_failure:

  How to handle parsing failures. `"error"` aborts when a structure
  cannot be parsed. `"na"` returns `NA` at invalid positions.

## Value

A
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
object.

## Details

GlyCAM IUPAC is similar to IUPAC-condensed notation, but monosaccharides
include configuration and ring markers such as `"DGlcp"` and `"LFucp"`,
terminal reducing-end residues end in `"-OH"`, and residue modifiers are
written in brackets, such as `"DGalp[6S]b1-4"`.

The parser normalizes GlyCAM IUPAC into IUPAC-condensed notation, then
uses the IUPAC-condensed parser to construct the glycan structure.
Explicit reducing-end moieties, such as `"-OH"` or `"-OME"`, are
normalized to the regular reducing-end IUPAC-condensed form because
glyrepr does not represent the terminal moiety separately.

## See also

[`parse_iupac_condensed()`](https://glycoverse.github.io/glyparse/dev/reference/parse_iupac_condensed.md)

## Examples

``` r
glycam <- "DManpa1-3[DManpa1-6]DManpb1-4DGlcpNAcb1-OH"
parse_glycam_iupac(glycam)
#> <glycan_structure[1]>
#> [1] Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-
#> # Unique structures: 1
```
