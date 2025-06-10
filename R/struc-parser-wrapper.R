# This wrapper function adds the following functionalities to a
# structure parser function:
# - Check input type;
# - Vectorized parsing;
# - More informative error messages.
struc_parser_wrapper <- function(x, parser) {
  checkmate::assert_character(x)
  graphs <- purrr::map(x, parser)
  glyrepr::as_glycan_structure(graphs)
}
