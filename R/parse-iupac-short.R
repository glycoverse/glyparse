#' Parse IUPAC-short Structures
#'
#' Parse IUPAC-short-style structure characters into a [glyrepr::glycan_structure()].
#' For more information about IUPAC-short format, see \doi{10.1351/pac199668101919}.
#'
#' @details
#' The IUPAC-short notation is a compact form of IUPAC-condensed notation.
#' It is rarely used in database, but appears a lot in literature for its
#' conciseness. Compared with IUPAC-condensed notation, IUPAC-short notation
#' ignore the anomer positions, assuming they are known for common monosaccharides.
#' For example, "Neu5Aca3Gala-" assumes the anomer of Neu5Ac is C2 (a2-3 linked).
#' Also, the parentheses around linkages are omitted, and parentheses are used
#' to indicate branching, e.g. "Neu5Aca3Gala3(Fuca3)GlcNAcb-".
#'
#' In the first case, the anomer is "a2". In the second case, the anomer is "?2".
#'
#' @param x A character vector of IUPAC-short strings. NA values are allowed and will be returned as NA structures.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param progress Whether to show a progress bar while parsing.
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' iupac <- "Neu5Aca3Gala3(Fuca6)GlcNAcb-"
#' parse_iupac_short(iupac)
#'
#' @seealso [parse_iupac_condensed()], [parse_iupac_extended()]
#'
#' @export
parse_iupac_short <- function(
  x,
  on_failure = "error",
  progress = FALSE
) {
  normalized_struc_parser_wrapper(
    x,
    convert_short_to_condensed,
    on_failure = on_failure,
    progress = progress
  )
}
