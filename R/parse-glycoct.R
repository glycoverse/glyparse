#' Parse GlycoCT Structures
#'
#' This function parses GlycoCT strings into a [glyrepr::glycan_structure()].
#' GlycoCT is a format used by databases like GlyTouCan and GlyGen.
#'
#' @details
#' GlycoCT format consists of:
#' - RES: Contains monosaccharides (lines starting with 'b:') and substituents (lines starting with 's:')
#' - LIN: Contains linkage information between residues
#' - UND: Contains floating substructures or substituents whose attachment to
#'   the main glycan is unresolved
#'
#' Main reducing-end alditol residues retain their alditol status and use an
#' unknown anomer configuration.
#'
#' For more information about GlycoCT format, see the glycoct.md documentation.
#'
#' @param x A character vector of GlycoCT strings. NA values are allowed and will be returned as NA structures.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param progress Whether to show a progress bar while parsing.
#' @param validate Retained for compatibility. Array records are always validated
#'   by [glyrepr::structure_from_arrays()], including when `FALSE`.
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' glycoct <- paste0(
#'   "RES\n",
#'   "1b:a-dgal-HEX-1:5\n",
#'   "2s:n-acetyl\n",
#'   "3b:b-dgal-HEX-1:5\n",
#'   "LIN\n",
#'   "1:1d(2+1)2n\n",
#'   "2:1o(3+1)3d"
#' )
#' parse_glycoct(glycoct)
#'
#' @export
parse_glycoct <- function(
  x,
  on_failure = "error",
  progress = FALSE,
  validate = TRUE
) {
  struc_parser_wrapper(
    x,
    "glycoct",
    on_failure = on_failure,
    progress = progress,
    validate = validate
  )
}
