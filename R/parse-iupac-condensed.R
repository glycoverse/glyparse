#' Parse IUPAC-condensed Structures
#'
#' This function parses IUPAC-condensed strings into a [glyrepr::glycan_structure()].
#' For more information about IUPAC-condensed notation, see \doi{10.1351/pac199668101919}.
#'
#' @details
#' The IUPAC-condensed notation is a compact form of IUPAC-extended notation.
#' It is used by the [GlyConnect](https://glyconnect.expasy.org/) database.
#' It contains the following information:
#' - Monosaccharide name, e.g. "Gal", "GlcNAc", "Neu5Ac".
#' - Substituent, e.g. "9Ac", "4Ac", "3Me", "?S".
#' - Linkage, e.g. "b1-3", "a1-2", "a1-?".
#'
#' An example of IUPAC-condensed string is "Gal(b1-3)GlcNAc(b1-4)Glc(a1-".
#'
#' The reducing-end monosaccharide can be with or without anomer information.
#' For example, the two strings below are all valid:
#' - "Neu5Ac(a2-"
#' - "Neu5Ac"
#'
#' In the first case, the anomer is "a2".
#' In the second case, the anomer is "?2".
#'
#' @param x A character vector of IUPAC-condensed strings.
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' iupac <- "Gal(b1-3)GlcNAc(b1-4)Glc(a1-"
#' parse_iupac_condensed(iupac)
#'
#' @seealso [parse_iupac_short()], [parse_iupac_extended()]
#'
#' @export
parse_iupac_condensed <- function(x) {
  struc_parser_wrapper(x, do_parse_iupac_condensed)
}

do_parse_iupac_condensed <- function(x) {
  struc <- glyrepr::as_glycan_structure(x)
  graph <- glyrepr::get_structure_graphs(struc)
  graph
}
