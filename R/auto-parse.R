#' Automatic Structure Parsing
#'
#' @description
#' Detect the structure string type and use the appropriate parser
#' to parse automatically.
#' Mixed types are supported.
#'
#' Supported types:
#' 1. GlycoCT
#' 2. IUPAC-condensed
#' 3. IUPAC-extended
#' 4. IUPAC-short
#' 5. GlyCAM IUPAC
#' 6. IUPAC-compact
#' 7. WURCS
#' 8. Linear Code
#' 9. pGlyco
#' 10. StrucGP
#' 11. KCF
#' 12. LINUCS
#'
#' @param x A character vector of structure strings. NA values are allowed and will be returned as NA structures.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param progress Whether to show a progress bar while parsing.
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' # Single structure
#' x <- "Gal(b1-3)GlcNAc(b1-4)Glc(a1-"  # IUPAC-condensed
#' auto_parse(x)
#'
#' # Mixed types
#' x <- c(
#'   "Gal(b1-3)GlcNAc(b1-4)Glc(a1-",  # IUPAC-condensed
#'   "Neu5Aca3Gala3(Fuca6)GlcNAcb-"  # IUPAC-short
#' )
#' auto_parse(x)
#'
#' @export
auto_parse <- function(x, on_failure = "error", progress = FALSE) {
  struc_parser_wrapper(
    x,
    do_auto_parse,
    on_failure = on_failure,
    progress = progress
  )
}

do_auto_parse <- function(x) {
  parser <- choose_parser(x)
  parser(x)
}

choose_parser <- function(x) {
  if (stringr::str_detect(x, "ENTRY")) {
    return(do_parse_kcf)
  } else if (stringr::str_detect(x, "RES")) {
    return(do_parse_glycoct)
  } else if (stringr::str_detect(x, "WURCS")) {
    return(do_parse_wurcs)
  } else if (stringr::str_starts(x, "\\([HNAGFSap]")) {
    return(do_parse_pglyco_struc)
  } else if (stringr::str_starts(x, "A") && stringr::str_ends(x, "a")) {
    return(do_parse_strucgp_struc)
  } else if (stringr::str_ends(x, "-OH")) {
    return(do_parse_glycam_iupac)
  } else if (
    stringr::str_detect(x, "\\u2192") || # Unicode arrow →
      stringr::str_detect(x, "->") || # Plain text arrow ->
      stringr::str_detect(x, "alpha|beta") # Plain text anomers
  ) {
    return(do_parse_iupac_extended)
  } else if (stringr::str_detect(x, "\\w+\\([ab\\?][\\d\\?]-")) {
    return(do_parse_iupac_condensed)
  } else if (is_linucs_string(x)) {
    return(do_parse_linucs)
  } else if (is_iupac_compact_string(x)) {
    return(do_parse_iupac_compact)
  } else if (stringr::str_ends(x, "-")) {
    return(do_parse_iupac_short)
  } else {
    # Assume Linear Code
    return(do_parse_linear_code)
  }
}
