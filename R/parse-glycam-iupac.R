#' Parse GlyCAM IUPAC Structures
#'
#' Parse GlyCAM IUPAC-style structure strings into a
#' [glyrepr::glycan_structure()].
#'
#' @details
#' GlyCAM IUPAC is similar to IUPAC-condensed notation, but monosaccharides
#' include configuration and ring markers such as `"DGlcp"` and `"LFucp"`,
#' terminal reducing-end residues end in `"-OH"`, and residue modifiers are
#' written in brackets, such as `"DGalp[6S]b1-4"`.
#'
#' The parser normalizes GlyCAM IUPAC into IUPAC-condensed notation, then uses
#' the IUPAC-condensed parser to construct the glycan structure. Explicit
#' reducing-end moieties, such as `"-OH"` or `"-OME"`, are normalized to the
#' regular reducing-end IUPAC-condensed form because glyrepr does not represent
#' the terminal moiety separately.
#'
#' @param x A character vector of GlyCAM IUPAC strings. NA values are allowed
#'   and will be returned as NA structures.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param progress Whether to show a progress bar while parsing.
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' glycam <- "DManpa1-3[DManpa1-6]DManpb1-4DGlcpNAcb1-OH"
#' parse_glycam_iupac(glycam)
#'
#' @seealso [parse_iupac_condensed()]
#'
#' @export
parse_glycam_iupac <- function(
  x,
  on_failure = "error",
  progress = FALSE
) {
  normalized_struc_parser_wrapper(
    x,
    convert_glycam_iupac_to_condensed,
    on_failure = on_failure,
    progress = progress
  )
}
