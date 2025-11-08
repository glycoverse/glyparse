# Parse GlycoCT Structures

This function parses GlycoCT strings into a
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html).
GlycoCT is a format used by databases like GlyTouCan and GlyGen.

## Usage

``` r
parse_glycoct(x)
```

## Arguments

- x:

  A character vector of GlycoCT strings.

## Value

A
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
object.

## Details

GlycoCT format consists of two parts:

- RES: Contains monosaccharides (lines starting with 'b:') and
  substituents (lines starting with 's:')

- LIN: Contains linkage information between residues

For more information about GlycoCT format, see the glycoct.md
documentation.

## Examples

``` r
glycoct <- paste0(
  "RES\n",
  "1b:a-dgal-HEX-1:5\n",
  "2s:n-acetyl\n",
  "3b:b-dgal-HEX-1:5\n",
  "LIN\n",
  "1:1d(2+1)2n\n",
  "2:1o(3+1)3d"
)
parse_glycoct(glycoct)
#> <glycan_structure[1]>
#> [1] Gal(b1-3)GalNAc(a1-
#> # Unique structures: 1
```
