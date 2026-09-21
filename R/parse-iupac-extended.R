#' Parse IUPAC-extended Structures
#'
#' Parse IUPAC-extended-style structure characters into a [glyrepr::glycan_structure()].
#' For more information about IUPAC-extended format, see \doi{10.1351/pac199668101919}.
#'
#' @param x A character vector of IUPAC-extended strings. NA values are allowed and will be returned as NA structures.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param progress Whether to show a progress bar while parsing.
#'
#' @details
#' The function accepts both a Unicode format (using the Greek letters alpha/beta
#' and the arrow symbol ->) and a plain-text format (using the strings "alpha",
#' "beta", and "->"). For example,
#' both `"\u03b2-D-Galp-(1\u21923)-\u03b1-D-GalpNAc-(1\u2192"` and
#' `"beta-D-Galp-(1->3)-alpha-D-GalpNAc-(1->"` are valid inputs.
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' iupac <- "\u03b2-D-Galp-(1\u21923)-\u03b1-D-GalpNAc-(1\u2192"
#' parse_iupac_extended(iupac)
#' parse_iupac_extended("beta-D-Galp-(1->3)-alpha-D-GalpNAc-(1->")
#'
#' @seealso [parse_iupac_condensed()], [parse_iupac_short()]
#'
#' @export
parse_iupac_extended <- function(
  x,
  on_failure = "error",
  progress = FALSE
) {
  normalized_struc_parser_wrapper(
    x,
    convert_ext_to_con,
    on_failure = on_failure,
    progress = progress
  )
}
