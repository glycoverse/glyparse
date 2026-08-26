# Parse KCF Structures

This function parses KCF strings into a
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html).
KCF is a graph-oriented format used by KEGG GLYCAN.

## Usage

``` r
parse_kcf(x, on_failure = "error", progress = FALSE, validate = TRUE)
```

## Arguments

- x:

  A character vector of KCF strings. NA values are allowed and will be
  returned as NA structures.

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

## Examples

``` r
kcf <- paste0(
  "ENTRY       G00066                      Glycan\n",
  "NODE        6\n",
  "            1   Cer        18     0\n",
  "            2   Glc        12     0\n",
  "            3   Gal         6     0\n",
  "            4   GlcNAc     -2     0\n",
  "            5   Gal       -10     0\n",
  "            6   GlcNAc    -18     0\n",
  "EDGE        5\n",
  "            1     2:b1    1:1\n",
  "            2     3:b1    2:4\n",
  "            3     4:b1    3:3\n",
  "            4     5:b1    4:4\n",
  "            5     6:b1    5:3\n",
  "///"
)
parse_kcf(kcf)
#> <glycan_structure[1]>
#> [1] GlcNAc(b1-3)Gal(b1-4)GlcNAc(b1-3)Gal(b1-4)Glc(b1-
#> # Unique structures: 1
```
