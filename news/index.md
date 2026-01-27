# Changelog

## glyparse (development version)

## glyparse 0.5.4

CRAN release: 2026-01-27

### Minor improvements and fixes

- Adapt to glyrepr 0.10.0.

## glyparse 0.5.3

CRAN release: 2025-11-04

### Minor improvements and fixes

- Prepare for release on CRAN.

## glyparse 0.5.2

### Minor improvements and fixes

- glyaprse now depends on the CRAN version of glyrepr.

## glyparse 0.5.1

### Minor improvements and fixes

- [`parse_iupac_extended()`](https://glycoverse.github.io/glyparse/reference/parse_iupac_extended.md)
  and
  [`parse_iupac_short()`](https://glycoverse.github.io/glyparse/reference/parse_iupac_short.md)
  now support generic monosaccharides, e.g. “Hex”, “HexNAc”, “HexN”.

## glyparse 0.5.0

### New features

- Add
  [`parse_linear_code()`](https://glycoverse.github.io/glyparse/reference/parse_linear_code.md)
  to parse Linear Code strings.
- [`auto_parse()`](https://glycoverse.github.io/glyparse/reference/auto_parse.md)
  now supports Linear Code strings.

### Minor improvements and fixes

- Revise the documentations of all parsers to be more accurate about
  return value type.

## glyparse 0.4.5

### Minor improvements and fixes

- Fix the bug that linkages like “1→3/4” cannot be parsed by
  [`parse_iupac_extended()`](https://glycoverse.github.io/glyparse/reference/parse_iupac_extended.md).
- Fix the bug that linkages like “b1-a3\|a4” cannot be parsed by
  [`parse_wurcs()`](https://glycoverse.github.io/glyparse/reference/parse_wurcs.md).
- Fix the bug that linkages like “a3/4” cannot be parsed by
  [`parse_iupac_short()`](https://glycoverse.github.io/glyparse/reference/parse_iupac_short.md).
- Fix the bug that linkages like “3:3o(3\|6+1)4d” cannot be parsed by
  [`parse_glycoct()`](https://glycoverse.github.io/glyparse/reference/parse_glycoct.md).

## glyparse 0.4.4

### Minor improvements and bug fixes

- Update dependencies to depend on release versions of glycoverse
  packages.

## glyparse 0.4.3

### Minor improvements and fixes

- Fix bugs introduced by the breaking changes in `glyrepr` v0.7.0.

## glyparse 0.4.2

### Minor improvements and fixes

- Fix some incorrect structure strings in vignettes.

## glyparse 0.4.1

### Minor improvements and fixes

- Fix some incorrect structure strings in tests and documentations.
- Remove some legacy documentations.

## glyparse 0.4.0

### Breaking changes

- [`parse_iupac_condensed()`](https://glycoverse.github.io/glyparse/reference/parse_iupac_condensed.md)
  and
  [`parse_iupac_short()`](https://glycoverse.github.io/glyparse/reference/parse_iupac_short.md)
  now require the reducing-end monosaccharide to have anomer
  information. For example, “Neu5Ac(a2-” and “Neu5Aca-” are valid, but
  “Neu5Ac” is not.

## glyparse 0.3.1

### Minor improvements and fixes

- [`parse_pglyco_struc()`](https://glycoverse.github.io/glyparse/reference/parse_pglyco_struc.md)
  now support “aH” and “pH” monosaccharides.

## glyparse 0.3.0

### New features

- Add
  [`auto_parse()`](https://glycoverse.github.io/glyparse/reference/auto_parse.md)
  to automatically detect and parse different glycan structure string
  formats.

### Minor improvements and fixes

- Better error messages. Before:

  ``` r
  > parse_iupac_condensed("bad_glycan")
  ```

      Error in `purrr::map()` at glyparse/R/struc-parser-wrapper.R:13:3:
      ℹ In index: 1.
      Caused by error in `value[[3L]]()`:
      ! Could not parse IUPAC-condensed string: {.val {x}}
      ℹ Invalid characters or format in IUPAC-condensed string
      Run `rlang::last_trace()` to see where the error occurred.

  Now:

  ``` r
  > parse_iupac_condensed("bad_glycan")
  ```

      Error in `parse_iupac_condensed()`:
      ! Can't parse: "bad_glycan"
      Run `rlang::last_trace()` to see where the error occurred.

## glyparse 0.2.1

### Minor improvements and fixes

- Update README.
- A “Get Started” vignette is added.

## glyparse 0.2.0

### Major improvements

- Add
  [`parse_glycoct()`](https://glycoverse.github.io/glyparse/reference/parse_glycoct.md)
  to parse glycans in GlycoCT format.

## glyparse 0.1.2

### Minor improvements

- [`parse_iupac_short()`](https://glycoverse.github.io/glyparse/reference/parse_iupac_short.md),
  [`parse_iupac_extended()`](https://glycoverse.github.io/glyparse/reference/parse_iupac_extended.md),
  [`parse_iupac_condensed()`](https://glycoverse.github.io/glyparse/reference/parse_iupac_condensed.md),
  [`parse_wurcs()`](https://glycoverse.github.io/glyparse/reference/parse_wurcs.md)
  now support multiple substituents on the same monosaccharide, to align
  with the updates in `glyrepr` v0.5.0.
