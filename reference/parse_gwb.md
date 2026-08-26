# Parse GlycoWorkbench Structures

Parse GlycoWorkbench (GWB/GWS) structure strings into a
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html).

## Usage

``` r
parse_gwb(x, on_failure = "error", progress = FALSE)
```

## Arguments

- x:

  A character vector of GlycoWorkbench strings. NA values are allowed
  and will be returned as NA structures.

- on_failure:

  How to handle parsing failures. `"error"` aborts when a structure
  cannot be parsed. `"na"` returns `NA` at invalid positions.

- progress:

  Whether to show a progress bar while parsing.

## Value

A
[`glyrepr::glycan_structure()`](https://glycoverse.github.io/glyrepr/reference/glycan_structure.html)
object.

## Details

GlycoWorkbench writes glycans from the reducing end towards the
non-reducing ends. Residues include their anomer, configuration, and
ring form, for example `"--4b1D-Gal,p"`. Branches are enclosed in
parentheses, and the structure is followed by mass options after `$`.

The parser normalizes the glycan tree to IUPAC-condensed notation before
constructing the glycan structure. GlycoWorkbench substituent nodes such
as `"--6S"` and `"--9Ac"` are retained as monosaccharide substituents.
Mass options are ignored because they are not part of the glycan graph.
Explicit open-chain residues (`,o`) are supported only for a reduced
`redEnd` root; other open-chain forms cannot be represented by
`glyrepr`.

## See also

[`parse_iupac_condensed()`](https://glycoverse.github.io/glyparse/reference/parse_iupac_condensed.md)

## Examples

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
