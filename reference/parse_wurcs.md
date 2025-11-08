# Parse WURCS Structures

This function parses WURCS strings into a
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html).
Currently, only WURCS 2.0 is supported. For more information about
WURCS, see [WURCS](https://github.com/glycoinfo/WURCS/wiki).

## Usage

``` r
parse_wurcs(x)
```

## Arguments

- x:

  A character vector of WURCS strings.

## Value

A
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
object.

## Examples

``` r
wurcs <- paste0(
  "WURCS=2.0/3,5,4/",
  "[a2122h-1b_1-5_2*NCC/3=O][a1122h-1b_1-5][a1122h-1a_1-5]/",
  "1-1-2-3-3/a4-b1_b4-c1_c3-d1_c6-e1"
)
parse_wurcs(wurcs)
#> <glycan_structure[1]>
#> [1] Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-
#> # Unique structures: 1
```
