
<!-- README.md is generated from README.Rmd. Please edit that file -->

# glyparse <a href="https://glycoverse.github.io/glyparse/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/glyparse)](https://CRAN.R-project.org/package=glyparse)
[![R-CMD-check](https://github.com/glycoverse/glyparse/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/glycoverse/glyparse/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/glycoverse/glyparse/graph/badge.svg)](https://app.codecov.io/gh/glycoverse/glyparse)
<!-- badges: end -->

The goal of glyparse is to parse glycan structure text strings into a
`glyrepr` glycan graph.

## Installation

You can install the development version of glyparse from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("glycoverse/glyparse")
```

## Example

``` r
library(glyparse)
```

``` r
# Parse a StrucGP-style structure string
parse_strucgp_struc("A2B2C1D1E2F1fedD1E2edcbB5ba")
#> Glycan Graph (NE)
#> dHex: 1, Hex: 4, HexNAc: 4
#> ------------------
#> HexNAc
#> ├─HexNAc
#> │ └─Hex
#> │   ├─Hex
#> │   │ └─HexNAc
#> │   │   └─Hex
#> │   └─Hex
#> │     └─HexNAc
#> └─dHex
```

``` r
# Parse a pGlyco-style structure string
parse_pglyco_struc("(N(F)(N(H(H(N))(H(N(H))))))")
#> Glycan Graph (NE)
#> F: 1, H: 4, N: 4
#> ------------------
#> N
#> ├─F
#> └─N
#>   └─H
#>     ├─H
#>     │ └─N
#>     └─H
#>       └─N
#>         └─H
```

``` r
# Parse a condensed IUPAC structure string
parse_iupac_condensed("Gal(b1-3)GlcNAc(b1-4)Glc")
#> Glycan Graph (NE)
#> Gal: 1, Glc: 1, GlcNAc: 1
#> ------------------
#> Glc
#> └─GlcNAc (b1-4)
#>   └─Gal (b1-3)
```
