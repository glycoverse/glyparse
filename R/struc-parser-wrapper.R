# This wrapper function adds the following functionalities to a
# structure parser function:
# - Check input type;
# - Vectorized parsing;
# - More informative error messages.
struc_parser_wrapper <- function(x, parser, call = rlang::caller_env()) {
  checkmate::assert_character(x)

  # Get unique values to avoid redundant computation
  unique_x <- unique(x)

  # Parse only unique values
  unique_graphs <- purrr::map(unique_x, purrr::possibly(parser, NA))
  invalid_unique_x <- unique_x[is.na(unique_graphs)]
  if (length(invalid_unique_x) > 0) {
    cli::cli_abort("Can't parse: {.val {invalid_unique_x}}", call = call)
  }

  # Map back to original dimension
  # Find the index of each original element in unique_x
  indices <- match(x, unique_x)
  struc <- glyrepr::as_glycan_structure(unique_graphs)
  struc[indices]
}
