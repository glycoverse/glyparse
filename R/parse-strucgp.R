#' Parse StrucGP Structures
#'
#' Parse StrucGP-style structure characters into a [glyrepr::glycan_structure()].
#' See example below for the structure format.
#'
#' @param x A character vector of StrucGP-style structure strings. NA values are allowed and will be returned as NA structures.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param progress Whether to show a progress bar while parsing.
#' @param validate Deprecated and ignored. Array records
#'   are always validated by [glyrepr::structure_from_arrays()].
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' glycan <- parse_strucgp_struc("A2B2C1D1E2F1fedD1E2edcbB5ba")
#' print(glycan, verbose = TRUE)
#'
#' @export
parse_strucgp_struc <- function(
  x,
  on_failure = "error",
  progress = FALSE,
  validate = lifecycle::deprecated()
) {
  if (!missing(validate)) {
    warn_deprecated_validate("parse_strucgp_struc")
  }
  struc_parser_wrapper(
    x,
    "strucgp",
    on_failure = on_failure,
    progress = progress
  )
}
