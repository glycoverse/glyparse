#' Parse Linear Code Structures
#'
#' Parse Linear Code structures into a [glyrepr::glycan_structure()].
#' To know more about Linear Code, see [this article](https://www.jstage.jst.go.jp/article/tigg1989/14/77/14_77_127/_article).
#'
#' @param x A character vector of Linear Code strings. NA values are allowed and will be returned as NA structures.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param progress Whether to show a progress bar while parsing.
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' linear_code <- "Ma3(Ma6)Mb4GNb4GNb"
#' parse_linear_code(linear_code)
#'
#' @export
parse_linear_code <- function(
  x,
  on_failure = "error",
  progress = FALSE
) {
  normalized_struc_parser_wrapper(
    x,
    convert_linear_to_iupac,
    on_failure = on_failure,
    progress = progress
  )
}
