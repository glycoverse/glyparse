#' Parse IUPAC-compact Structures
#'
#' Parse IUPAC-compact strings into a [glyrepr::glycan_structure()].
#'
#' @details
#' IUPAC-compact notation is similar to IUPAC-condensed notation, but linkages
#' are written directly after the monosaccharide, such as `"Galb1-3GlcNAc"`,
#' and branches are written in parentheses. The parser normalizes compact
#' notation into IUPAC-condensed notation, then uses the IUPAC-condensed parser
#' to construct the glycan structure.
#'
#' Alditol glycans marked with `+aldi` retain their alditol status and use an
#' unknown reducing-end anomer configuration.
#'
#' @param x A character vector of IUPAC-compact strings. NA values are allowed
#'   and will be returned as NA structures.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param progress Whether to show a progress bar while parsing.
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' iupac <- "Mana1-3(Mana1-6)Manb1-4GlcNAcb"
#' parse_iupac_compact(iupac)
#'
#' @seealso [parse_iupac_condensed()]
#'
#' @export
parse_iupac_compact <- function(
  x,
  on_failure = "error",
  progress = FALSE
) {
  normalized_struc_parser_wrapper(
    x,
    convert_iupac_compact_to_condensed,
    on_failure = on_failure,
    progress = progress
  )
}
