# This wrapper function adds the following functionalities to a
# structure parser function:
# - Check input type;
# - Vectorized parsing;
# - More informative error messages.
struc_parser_wrapper <- function(x, parser) {
  checkmate::assert_character(x)
  
  # Get unique values to avoid redundant computation
  unique_x <- unique(x)
  
  # Parse only unique values
  unique_graphs <- purrr::map(unique_x, parser)
  
  # Map back to original dimension
  # Find the index of each original element in unique_x
  indices <- match(x, unique_x)
  struc <- glyrepr::as_glycan_structure(unique_graphs)
  struc[indices]
}
