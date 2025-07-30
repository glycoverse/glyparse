# glyparse 0.4.0

## Breaking changes

* `parse_iupac_condensed()` and `parse_iupac_short()` now require the reducing-end monosaccharide to have anomer information. For example, "Neu5Ac(a2-" and "Neu5Aca-" are valid, but "Neu5Ac" is not.

# glyparse 0.3.1

## Minor improvements and fixes

* `parse_pglyco_struc()` now support "aH" and "pH" monosaccharides.

# glyparse 0.3.0

## New features

* Add `auto_parse()` to automatically detect and parse different glycan structure string formats.

## Minor improvements and fixes

* Better error messages.
  Before:
  ```r
  > parse_iupac_condensed("bad_glycan")
  ```
  ```
  Error in `purrr::map()` at glyparse/R/struc-parser-wrapper.R:13:3:
  ℹ In index: 1.
  Caused by error in `value[[3L]]()`:
  ! Could not parse IUPAC-condensed string: {.val {x}}
  ℹ Invalid characters or format in IUPAC-condensed string
  Run `rlang::last_trace()` to see where the error occurred.
  ```
  Now:
  ```r
  > parse_iupac_condensed("bad_glycan")
  ```
  ```
  Error in `parse_iupac_condensed()`:
  ! Can't parse: "bad_glycan"
  Run `rlang::last_trace()` to see where the error occurred.
  ```

# glyparse 0.2.1

## Minor improvements and fixes

* Update README.
* A "Get Started" vignette is added.

# glyparse 0.2.0

## Major improvements

* Add `parse_glycoct()` to parse glycans in GlycoCT format.

# glyparse 0.1.2

## Minor improvements

* `parse_iupac_short()`, `parse_iupac_extended()`, `parse_iupac_condensed()`,
  `parse_wurcs()` now support multiple substituents on the same monosaccharide,
  to align with the updates in `glyrepr` v0.5.0.
