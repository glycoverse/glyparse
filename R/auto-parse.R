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
#' 5. WURCS
#' 6. Linear Code
#' 7. pGlyco
#' 8. StrucGP
#'
#' @param x A character vector of structure strings.
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
auto_parse <- function(x) {
  struc_parser_wrapper(x, do_auto_parse)
}

do_auto_parse <- function(x) {
  parser <- choose_parser(x)
  parser(x)
}

choose_parser <- function(x) {
  if (stringr::str_detect(x, "RES")) {
    return(do_parse_glycoct)
  } else if (stringr::str_detect(x, "WURCS")) {
    return(do_parse_wurcs)
  } else if (stringr::str_starts(x, stringr::fixed("("))) {
    return(do_parse_pglyco_struc)
  } else if (stringr::str_starts(x, "A") && stringr::str_ends(x, "a")) {
    return(do_parse_strucgp_struc)
  } else if (stringr::str_detect(x, "\\u2192")) {  # \\u2192 is →
    return(do_parse_iupac_extended)
  } else if (stringr::str_detect(x, "\\w+\\([ab\\?][\\d\\?]-")) {
    return(do_parse_iupac_condensed)
  } else if (stringr::str_ends(x, "-")) {
    return(do_parse_iupac_short)
  } else {  # Assume Linear Code
    return(do_parse_linear_code)
  }
}