# Getting Started with glyparse

## Your Universal Glycan Text Translator 🔄

Welcome to the world of glycan text parsing! If you’ve ever worked with
glycan data from different sources, you know the frustration: every
database, software tool, and research group seems to have their own way
of representing glycan structures in text format.

That’s where `glyparse` comes to the rescue! 🚀

Think of `glyparse` as your **universal glycan translator** — it can
read glycan structures written in many different “languages” and convert
them all into a unified format that your computer can understand and
work with.

**Note:** All functions in `glyparse` return
[`glyrepr::glycan_structure`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
objects. If you are unfamiliar with `glyrepr`, you can read the
documentation
[here](https://glycoverse.github.io/glyrepr/articles/glyrepr.html).

``` r
library(glyparse)
```

## The Babel Tower of Glycan Text Formats 🗼

Before we dive in, let’s see what we’re dealing with. Here’s the same
N-glycan core structure written in different formats:

| Format              | Example                                                                                   | Where You’ll See It      |
|---------------------|-------------------------------------------------------------------------------------------|--------------------------|
| **IUPAC-condensed** | `Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc`                                         | Literature, UniCarbKB    |
| **IUPAC-short**     | `Mana3(Mana6)Manb4GlcNAcb4GlcNAc`                                                         | Literature, UniCarbKB    |
| **IUPAC-extended**  | `alpha-D-Man-(1->3)-[alpha-D-Man-(1->6)]-beta-D-Man-(1->4)-beta-D-GlcNAc-(1->4)-D-GlcNAc` | Literature, UniCarbKB    |
| **GlycoCT**         | Complex multi-line format                                                                 | Literature, GlycomeDB    |
| **WURCS**           | `WURCS=2.0/3,5,4/[...]/1-1-2-3-3/a4-b1_b4-c1...`                                          | Literature, GlyTouCan    |
| **Linear Code**     | `Ma3(Ma6)Mb4GNb4GNb`                                                                      | Literature               |
| **pGlyco**          | `(N(N(H(H(H)))))`                                                                         | pGlyco software results  |
| **StrucGP**         | `A2B2C1D1E2fedcba`                                                                        | StrucGP software results |

Confusing, right? 😵‍💫 `glyparse` understands them all!

## Your Parsing Toolkit 🛠️

`glyparse` provides seven specialized parsers, each optimized for a
specific format:

- **[`parse_iupac_condensed()`](https://glycoverse.github.io/glyparse/reference/parse_iupac_condensed.md)**:
  The most common format
- **[`parse_iupac_short()`](https://glycoverse.github.io/glyparse/reference/parse_iupac_short.md)**:
  Compact literature format  
- **[`parse_iupac_extended()`](https://glycoverse.github.io/glyparse/reference/parse_iupac_extended.md)**:
  Verbose formal format
- **[`parse_glycoct()`](https://glycoverse.github.io/glyparse/reference/parse_glycoct.md)**:
  Database standard format
- **[`parse_wurcs()`](https://glycoverse.github.io/glyparse/reference/parse_wurcs.md)**:
  Modern standardized format
- **[`parse_linear_code()`](https://glycoverse.github.io/glyparse/reference/parse_linear_code.md)**:
  Linear Code format
- **[`parse_pglyco_struc()`](https://glycoverse.github.io/glyparse/reference/parse_pglyco_struc.md)**:
  pGlyco software format
- **[`parse_strucgp_struc()`](https://glycoverse.github.io/glyparse/reference/parse_strucgp_struc.md)**:
  StrucGP software format

All parsers follow the same pattern:

- **Input**: Character vector of structure strings
- **Output**: A
  [`glyrepr::glycan_structure`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
  object that you can analyze

## Part 0: `auto_parse()`

Don’t know what you’re dealing with? Give it to
[`auto_parse()`](https://glycoverse.github.io/glyparse/reference/auto_parse.md)!
This function tries to identify the format automatically and use the
appropriate parser. Even input with mixed formats is supported.

``` r
x <- c(
  "Gal(b1-3)GalNAc(b1-",
  "(N(F)(N(H(H(N))(H(N(H))))))",
  "WURCS=2.0/3,3,2/[a2122h-1b_1-5][a1122h-1b_1-5][a1122h-1a_1-5]/1-2-3/a4-b1_b3-c1"
)
auto_parse(x)
#> Error in `validate_glycan_structure_vector()`:
#> ! All structures must have the same monosaccharide type.
#> ✖ Found 2 concrete and 1 generic structure(s) in the same vector.
#> ℹ Use `convert_to_generic()` to convert concrete structures to generic type.
```

## Part 1: IUPAC Family — The Popular Kids 🌟

Let’s start with the IUPAC formats.

### IUPAC-Condensed: The Literature Standard

This format is widely used in scientific literature and databases like
UniCarbKB.

Want to know more about IUPAC-condensed format? Check
[this](https://glycoverse.github.io/glyrepr/articles/iupac.html) out!

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

### IUPAC-Short: Literature’s Favorite

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

Notice how much more compact this is! The parser is smart enough to
infer common linkage positions (like Neu5Ac always being a2-linked).

### IUPAC-Extended: The Formal One

This verbose format includes full chemical names and stereochemistry:

``` r
iupac_extended <- paste0(
  "α-D-Manp-(1→3)[α-D-Manp-(1→6)]-β-D-Manp-(1→4)",
  "-β-D-GlcpNAc-(1→4)-β-D-GlcpNAc-(1→"
)
parse_iupac_extended(iupac_extended)
#> <glycan_structure[1]>
#> [1] Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-
#> # Unique structures: 1
```

## Part 2: Database Formats — The Heavy Hitters 💪

### GlycoCT: The Precision Format

GlycoCT is used in literature for precise representation and in
databases like GlycomeDB. It’s more complex but extremely precise:

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

### WURCS: The Complex Structure Format

WURCS (Web3 Unique Representation of Carbohydrate Structures) is used in
literature for complex structures and in databases like GlyTouCan:

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

### Linear Code: The Simplified Format

Linear Code is a simplified format used in literature for complex
structures:

``` r
linear_code <- "Ma3(Ma6)Mb4GNb4GNb"
parse_linear_code(linear_code)
#> <glycan_structure[1]>
#> [1] Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-
#> # Unique structures: 1
```

## Part 3: Software-Specific Formats — The Specialists 🔬

### pGlyco Format: Proteomics Tool

If you work with glycoproteomics, you might encounter pGlyco’s
parenthetical notation:

``` r
pglyco <- "(N(F)(N(H(H(N))(H(N(H))))))"
parse_pglyco_struc(pglyco)
#> <glycan_structure[1]>
#> [1] Hex(??-?)HexNAc(??-?)Hex(??-?)[HexNAc(??-?)Hex(??-?)]Hex(??-?)HexNAc(??-?)[dHex(??-?)]HexNAc(??-
#> # Unique structures: 1
```

This cryptic notation actually represents a complex N-glycan:

- N = HexNAc
- F = Fuc  
- H = Hex (Man or Gal)

### StrucGP Format: Alphabetical System

StrucGP uses a letter-based encoding system:

``` r
strucgp <- "A2B2C1D1E2F1fedD1E2edcbB5ba"
parse_strucgp_struc(strucgp)
#> <glycan_structure[1]>
#> [1] Hex(??-?)HexNAc(??-?)Hex(??-?)[HexNAc(??-?)Hex(??-?)]Hex(??-?)HexNAc(??-?)[dHex(??-?)]HexNAc(??-
#> # Unique structures: 1
```

## The Bottom Line 🎯

`glyparse` transforms the chaos of glycan text formats into order. No
matter where your glycan data comes from, databases, literature, or
software tools, you can now parse it into
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
for further analysis. In fact, `glyread` package uses these parsing
functions internally when reading output from common glycopeptide
identification softwares.

**Next steps:**

- Explore the `glyrepr` package for structure manipulation
- Try `glymotif` for motif analysis of your parsed structures  
- Use `glyexp` for experimental data analysis
- Check out the rest of the `glycoverse` ecosystem!

Happy parsing! 🧬✨
