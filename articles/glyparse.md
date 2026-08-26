# Getting Started with glyparse

## Parsing glycan text

Glycan structures appear in several text formats. If you work with data
from different sources, you may find that each database, software tool,
or research group uses its own conventions for representing structures
as text.

`glyparse` provides a parser for each supported format and converts the
results to a common
[`glyrepr::glycan_structure`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
representation.

This gives you a consistent starting point for structure comparison and
downstream analysis, without rewriting every input by hand.

All public parsers return
[`glyrepr::glycan_structure`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
objects. If you are unfamiliar with `glyrepr`, see the [`glyrepr`
vignette](https://glycoverse.github.io/glyrepr/articles/glyrepr.html).

``` r

library(glyparse)
```

## Supported formats

Here is a quick overview of the formats supported by the package:

| Format | Parser | Common sources |
|----|----|----|
| **IUPAC-condensed** | [`parse_iupac_condensed()`](https://glycoverse.github.io/glyparse/reference/parse_iupac_condensed.md) | Literature, UniCarbKB |
| **IUPAC-short** | [`parse_iupac_short()`](https://glycoverse.github.io/glyparse/reference/parse_iupac_short.md) | Literature, UniCarbKB |
| **IUPAC-extended** | [`parse_iupac_extended()`](https://glycoverse.github.io/glyparse/reference/parse_iupac_extended.md) | Literature, UniCarbKB |
| **IUPAC-compact** | [`parse_iupac_compact()`](https://glycoverse.github.io/glyparse/reference/parse_iupac_compact.md) | Compact database and literature notation |
| **GlyCAM IUPAC** | [`parse_glycam_iupac()`](https://glycoverse.github.io/glyparse/reference/parse_glycam_iupac.md) | GlyCAM workflows |
| **GlycoCT** | [`parse_glycoct()`](https://glycoverse.github.io/glyparse/reference/parse_glycoct.md) | Literature, GlycomeDB |
| **WURCS** | [`parse_wurcs()`](https://glycoverse.github.io/glyparse/reference/parse_wurcs.md) | Literature, GlyTouCan |
| **Linear Code** | [`parse_linear_code()`](https://glycoverse.github.io/glyparse/reference/parse_linear_code.md) | Literature |
| **LINUCS** | [`parse_linucs()`](https://glycoverse.github.io/glyparse/reference/parse_linucs.md) | Glycan structure software |
| **KCF** | [`parse_kcf()`](https://glycoverse.github.io/glyparse/reference/parse_kcf.md) | KEGG GLYCAN |
| **GlycoWorkbench** | [`parse_gwb()`](https://glycoverse.github.io/glyparse/reference/parse_gwb.md) | GlycoWorkbench and GWS files |
| **pGlyco** | [`parse_pglyco_struc()`](https://glycoverse.github.io/glyparse/reference/parse_pglyco_struc.md) | pGlyco software results |
| **StrucGP** | [`parse_strucgp_struc()`](https://glycoverse.github.io/glyparse/reference/parse_strucgp_struc.md) | StrucGP software results |

The package includes an automatic parser and 13 format-specific parsers.
Each parser accepts a character vector of structure strings and returns
a
[`glyrepr::glycan_structure`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
object.

## Part 1: Automatic parsing

When the input format is not known in advance, use
[`auto_parse()`](https://glycoverse.github.io/glyparse/reference/auto_parse.md).
It detects the format one element at a time, so a vector can contain
mixed formats.

``` r

x <- c(
  "Gal(b1-3)GalNAc(b1-",
  "(N(F)(N(H(H(N))(H(N(H))))))",
  "WURCS=2.0/3,3,2/[a2122h-1b_1-5][a1122h-1b_1-5][a1122h-1a_1-5]/1-2-3/a4-b1_b3-c1"
)
auto_parse(x)
#> <glycan_structure[3]>
#> [1] Gal(b1-3)GalNAc(b1-
#> [2] Hex(??-?)HexNAc(??-?)Hex(??-?)[HexNAc(??-?)Hex(??-?)]Hex(??-?)HexNAc(??-?)[dHex(??-?)]HexNAc(??-
#> [3] Man(a1-3)Man(b1-4)Glc(b1-
#> # Unique structures: 3
```

## Part 2: IUPAC notation

### IUPAC-condensed

This format is widely used in scientific literature and databases like
UniCarbKB.

For more detail about this notation, see the [`glyrepr` IUPAC
vignette](https://glycoverse.github.io/glyrepr/articles/iupac.html).

``` r

# Single structure
iupac_condensed <- "Neu5Ac(a2-3)Gal(b1-4)[Fuc(a1-3)]GlcNAc(b1-4)Gal(b1-4)Glc(a1-"
parse_iupac_condensed(iupac_condensed)
#> <glycan_structure[1]>
#> [1] Neu5Ac(a2-3)Gal(b1-4)[Fuc(a1-3)]GlcNAc(b1-4)Gal(b1-4)Glc(a1-
#> # Unique structures: 1
```

``` r

# Multiple structures at once
glycans <- c(
  "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-",  # N-glycan core
  "Gal(b1-3)GalNAc(b1-",                                  # O-glycan core 1
  "Neu5Ac(a2-3)Gal(b1-3)[GlcNAc(b1-6)]GalNAc(b1-"         # O-glycan core 2
)
parse_iupac_condensed(glycans)
#> <glycan_structure[3]>
#> [1] Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-
#> [2] Gal(b1-3)GalNAc(b1-
#> [3] Neu5Ac(a2-3)Gal(b1-3)[GlcNAc(b1-6)]GalNAc(b1-
#> # Unique structures: 3
```

### IUPAC-short

This compact format is popular in research papers because it saves
space:

``` r

# The same structures in short format
iupac_short <- c(
  "Mana3(Mana6)Manb4GlcNAcb4GlcNAcb-",
  "Galb3GalNAcb-", 
  "Neu5Aca3Galb3(GlcNAcb6)GalNAcb-"
)
parse_iupac_short(iupac_short)
#> <glycan_structure[3]>
#> [1] Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-
#> [2] Gal(b1-3)GalNAc(b1-
#> [3] Neu5Ac(a2-3)Gal(b1-3)[GlcNAc(b1-6)]GalNAc(b1-
#> # Unique structures: 3
```

The parser infers common linkage positions when they are omitted.

### IUPAC-extended

This verbose format includes full chemical names and stereochemistry:

``` r

iupac_extended <- paste0(
  "alpha-D-Galp-(1->3)-",
  "beta-D-Galp-(1->"
)
parse_iupac_extended(iupac_extended)
#> <glycan_structure[1]>
#> [1] Gal(a1-3)Gal(b1-
#> # Unique structures: 1
```

### IUPAC-compact

IUPAC-compact notation places the linkage immediately after each residue
and uses parentheses for branches.

``` r

iupac_compact <- "Mana1-3(Mana1-6)Manb1-4GlcNAcb"
parse_iupac_compact(iupac_compact)
#> <glycan_structure[1]>
#> [1] Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-
#> # Unique structures: 1
```

### GlyCAM IUPAC

GlyCAM IUPAC strings include configuration and ring markers such as
`DManp` and use a terminal `-OH` marker for the reducing end. The parser
normalizes these strings before parsing them.

``` r

glycam <- "DManpa1-3[DManpa1-6]DManpb1-4DGlcpNAcb1-OH"
parse_glycam_iupac(glycam)
#> <glycan_structure[1]>
#> [1] Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-
#> # Unique structures: 1
```

## Part 3: Database and exchange formats

### GlycoCT

GlycoCT uses separate `RES` and `LIN` sections. It is verbose, but
records residue and linkage information explicitly.

``` r

glycoct <- paste0(
  "RES\n",
  "1b:b-dglc-HEX-1:5\n",
  "2b:b-dgal-HEX-1:5\n", 
  "3b:a-dgal-HEX-1:5\n",
  "LIN\n",
  "1:1o(4+1)2d\n",
  "2:2o(3+1)3d"
)
parse_glycoct(glycoct)
#> <glycan_structure[1]>
#> [1] Gal(a1-3)Gal(b1-4)Glc(b1-
#> # Unique structures: 1
```

### WURCS

WURCS (Web3 Unique Representation of Carbohydrate Structures) is a
compact standardised exchange format used by resources such as
GlyTouCan.

``` r

wurcs <- paste0(
  "WURCS=2.0/3,3,2/",
  "[a2122h-1b_1-5][a1122h-1b_1-5][a1122h-1a_1-5]/",
  "1-2-3/a4-b1_b3-c1"
)
parse_wurcs(wurcs)
#> <glycan_structure[1]>
#> [1] Man(a1-3)Man(b1-4)Glc(b1-
#> # Unique structures: 1
```

### KCF

KCF represents a glycan as a graph with `NODE` and `EDGE` sections. It
is used by KEGG GLYCAN.

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

### Linear Code

Linear Code is a compact notation used in literature and glycan
software:

``` r

linear_code <- "Ma3(Ma6)Mb4GNb4GNb"
parse_linear_code(linear_code)
#> <glycan_structure[1]>
#> [1] Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-
#> # Unique structures: 1
```

### LINUCS

LINUCS expresses each residue as a token followed by a braced child
list. A linkage such as `[(4+1)]` records the parent and child
positions.

``` r

linucs <- "[][b-D-Glcp]{[(4+1)][b-D-Galp]{}}"
parse_linucs(linucs)
#> <glycan_structure[1]>
#> [1] Gal(b1-4)Glc(b1-
#> # Unique structures: 1
```

## Part 4: Software-specific formats

### GlycoWorkbench

GlycoWorkbench strings describe the structure from the reducing end and
may include mass options after `$`. Those options are ignored because
they are not part of the glycan graph.

``` r

gwb <- paste0(
  "freeEnd--1b1D-GlcNAc,p(--6a1L-Fuc,p)",
  "--4b1D-Gal,p--3a2D-NeuAc,p$MONO,Und,0,0,freeEnd"
)
parse_gwb(gwb)
#> <glycan_structure[1]>
#> [1] Neu5Ac(a2-3)Gal(b1-4)[Fuc(a1-6)]GlcNAc(b1-
#> # Unique structures: 1
```

### pGlyco

If you work with glycoproteomics, you may encounter pGlyco’s
parenthetical notation:

``` r

pglyco <- "(N(F)(N(H(H(N))(H(N(H))))))"
parse_pglyco_struc(pglyco)
#> <glycan_structure[1]>
#> [1] Hex(??-?)HexNAc(??-?)Hex(??-?)[HexNAc(??-?)Hex(??-?)]Hex(??-?)HexNAc(??-?)[dHex(??-?)]HexNAc(??-
#> # Unique structures: 1
```

In this notation:

- N = HexNAc
- F = Fuc
- H = Hex (Man or Gal)

### StrucGP

StrucGP uses a letter-based encoding system:

``` r

strucgp <- "A2B2C1D1E2F1fedD1E2edcbB5ba"
parse_strucgp_struc(strucgp)
#> <glycan_structure[1]>
#> [1] Hex(??-?)HexNAc(??-?)Hex(??-?)[HexNAc(??-?)Hex(??-?)]Hex(??-?)HexNAc(??-?)[dHex(??-?)]HexNAc(??-
#> # Unique structures: 1
```

## Handling parsing failures

By default, a malformed structure stops the call with an informative
error. When processing a larger collection, `on_failure = "na"`
preserves the input positions and returns `NA` for records that cannot
be parsed.

``` r

parse_iupac_compact(
  c(valid = "Galb1-3GalNAca", invalid = "not a glycan"),
  on_failure = "na"
)
#> <glycan_structure[2]>
#> [1] valid    Gal(b1-3)GalNAc(a1-
#> [2] invalid  NA
#> # Unique structures: 1
```
