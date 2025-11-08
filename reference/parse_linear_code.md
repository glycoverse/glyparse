# Parse Linear Code Structures

Parse Linear Code structures into a
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html).
To know more about Linear Code, see [this
article](https://www.jstage.jst.go.jp/article/tigg1989/14/77/14_77_127/_article).

## Usage

``` r
parse_linear_code(x)
```

## Arguments

- x:

  A character vector of Linear Code strings.

## Value

A
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
object.

## Examples

``` r
linear_code <- "Ma3(Ma6)Mb4GNb4GNb"
parse_linear_code(linear_code)
#> <glycan_structure[1]>
#> [1] Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-
#> # Unique structures: 1
```
