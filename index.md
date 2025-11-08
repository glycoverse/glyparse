# glyparse

Parse different glycan structure text nomenclature into
[`glyrepr::glycan_structure`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html).

Currently, the following formats are supported: IUPAC-extended,
IUPAC-condensed, IUPAC-short, WURCS, GlycoCT, StrucGP-style,
pGlyco-style.

## Installation

You can install the latest release of glyparse from CRAN with:

``` r
install.packages("glyparse")
```

Or install the development version from GitHub:

``` r
remotes::install_github("glycoverse/glyparse")
```

## Documentation

- 📚 Reference:
  [Here](https://glycoverse.github.io/glyparse/reference/index.html)

## Role in `glycoverse`

While `glyrepr` natively supports parsing IUPAC-condensed format through
`as_glycan_structure()`, the glycan community has developed numerous
other nomenclature formats that remain widely adopted in different
contexts. `glyparse` serves as a comprehensive extension to `glyrepr`,
providing robust parsing capabilities for these diverse formats. The
package can be utilized directly by researchers to parse glycan
structure text strings, or integrated with other packages such as
`glyread` to seamlessly handle structure parsing when processing results
from glycan and glycopeptide analysis software.

## Example

``` r
library(glyparse)
```

``` r
# Parse a StrucGP-style structure string
parse_strucgp_struc("A2B2C1D1E2F1fedD1E2edcbB5ba")
#> <glycan_structure[1]>
#> [1] Hex(??-?)HexNAc(??-?)Hex(??-?)[HexNAc(??-?)Hex(??-?)]Hex(??-?)HexNAc(??-?)[dHex(??-?)]HexNAc(??-
#> # Unique structures: 1
```

``` r
# Parse a pGlyco-style structure string
parse_pglyco_struc("(N(F)(N(H(H(N))(H(N(H))))))")
#> <glycan_structure[1]>
#> [1] Hex(??-?)HexNAc(??-?)Hex(??-?)[HexNAc(??-?)Hex(??-?)]Hex(??-?)HexNAc(??-?)[dHex(??-?)]HexNAc(??-
#> # Unique structures: 1
```

``` r
# Parse a condensed IUPAC structure string
parse_iupac_condensed("Gal(b1-3)GlcNAc(b1-4)Glc(a1-")
#> <glycan_structure[1]>
#> [1] Gal(b1-3)GlcNAc(b1-4)Glc(a1-
#> # Unique structures: 1
```

``` r
# Parse a WURCS structure string
parse_wurcs("WURCS=2.0/3,5,4/[a2122h-1b_1-5_2*NCC/3=O][a1122h-1b_1-5][a1122h-1a_1-5]/1-1-2-3-3/a4-b1_b4-c1_c3-d1_c6-e1")
#> <glycan_structure[1]>
#> [1] Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-
#> # Unique structures: 1
```
