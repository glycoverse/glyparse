# Automatic Structure Parsing

Detect the structure string type and use the appropriate parser to parse
automatically. Mixed types are supported.

Supported types:

1.  GlycoCT

2.  IUPAC-condensed

3.  IUPAC-extended

4.  IUPAC-short

5.  WURCS

6.  Linear Code

7.  pGlyco

8.  StrucGP

## Usage

``` r
auto_parse(x)
```

## Arguments

- x:

  A character vector of structure strings. NA values are allowed and
  will be returned as NA structures.

## Value

A
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
object.

## Examples

``` r
# Single structure
x <- "Gal(b1-3)GlcNAc(b1-4)Glc(a1-"  # IUPAC-condensed
auto_parse(x)
#> <glycan_structure[1]>
#> [1] Gal(b1-3)GlcNAc(b1-4)Glc(a1-
#> # Unique structures: 1

# Mixed types
x <- c(
  "Gal(b1-3)GlcNAc(b1-4)Glc(a1-",  # IUPAC-condensed
  "Neu5Aca3Gala3(Fuca6)GlcNAcb-"  # IUPAC-short
)
auto_parse(x)
#> <glycan_structure[2]>
#> [1] Gal(b1-3)GlcNAc(b1-4)Glc(a1-
#> [2] Neu5Ac(a2-3)Gal(a1-3)[Fuc(a1-6)]GlcNAc(b1-
#> # Unique structures: 2
```
