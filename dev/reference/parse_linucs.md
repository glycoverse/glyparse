# Parse LINUCS Structures

Parse LINUCS strings into a
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html).
LINUCS is a tree-oriented glycan format that writes each residue as a
linkage token, a residue token, and a braced child list, for example
`"[][Hexp]{[(4+1)][Hexp]{}}"`.

## Usage

``` r
parse_linucs(x, on_failure = "error", progress = FALSE, validate = TRUE)
```

## Arguments

- x:

  A character vector of LINUCS strings. NA values are allowed and will
  be returned as NA structures.

- on_failure:

  How to handle parsing failures. `"error"` aborts when a structure
  cannot be parsed. `"na"` returns `NA` at invalid positions.

- progress:

  Whether to show a progress bar while parsing.

- validate:

  Whether to validate parsed glycan graphs before constructing the
  result.

## Value

A
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
object.

## Details

LINUCS linkages are written as `"(parent+child)"`, where `parent` is the
linkage position on the parent residue and `child` is the anomeric
linkage position on the child residue. Residue labels are normalized to
the monosaccharide and substituent vocabulary used by
[glyrepr::glyrepr](https://glycoverse.github.io/glyrepr/reference/glyrepr-package.html).

## Examples

``` r
linucs <- "[][b-D-Glcp]{[(4+1)][b-D-Galp]{}}"
parse_linucs(linucs)
#> <glycan_structure[1]>
#> [1] Gal(b1-4)Glc(b1-
#> # Unique structures: 1
```
