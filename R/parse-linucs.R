#' Parse LINUCS Structures
#'
#' Parse LINUCS strings into a [glyrepr::glycan_structure()].
#' LINUCS is a tree-oriented glycan format that writes each residue as a
#' linkage token, a residue token, and a braced child list, for example
#' `"[][Hexp]{[(4+1)][Hexp]{}}"`.
#'
#' @details
#' LINUCS linkages are written as `"(parent+child)"`, where `parent` is the
#' linkage position on the parent residue and `child` is the anomeric linkage
#' position on the child residue. Residue labels are normalized to the
#' monosaccharide and substituent vocabulary used by [glyrepr].
#' A `-ol` suffix on the root residue is retained as alditol status.
#'
#' @param x A character vector of LINUCS strings. NA values are allowed and
#'   will be returned as NA structures.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param progress Whether to show a progress bar while parsing.
#' @param validate Deprecated and ignored. Array records
#'   are always validated by [glyrepr::structure_from_arrays()].
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' linucs <- "[][b-D-Glcp]{[(4+1)][b-D-Galp]{}}"
#' parse_linucs(linucs)
#'
#' @export
parse_linucs <- function(
  x,
  on_failure = "error",
  progress = FALSE,
  validate = lifecycle::deprecated()
) {
  if (!missing(validate)) {
    warn_deprecated_validate("parse_linucs")
  }
  struc_parser_wrapper(
    x,
    "linucs",
    on_failure = on_failure,
    progress = progress
  )
}
