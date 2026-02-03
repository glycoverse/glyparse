# This wrapper function adds the following functionalities to a
# structure parser function:
# - Check input type;
# - Vectorized parsing;
# - More informative error messages;
# - Preserve names of the input character vector.
struc_parser_wrapper <- function(x, parser, call = rlang::caller_env()) {
  checkmate::assert_character(x)

  # Preserve names from input
  original_names <- names(x)

  # Identify NA positions
  na_mask <- is.na(x)

  # Handle edge case: all NA
  if (all(na_mask)) {
    result <- glyrepr::glycan_structure(NA)
    result <- result[rep(1, length(x))]
    if (!is.null(original_names)) {
      attr(result, "names") <- original_names
    }
    return(result)
  }

  # Get unique non-NA values to avoid redundant computation
  non_na_x <- x[!na_mask]
  unique_x <- unique(non_na_x)

  # Parse only unique values
  unique_graphs <- purrr::map(unique_x, purrr::possibly(parser, NA))
  # Check for parsing failures (parser returned NA, not a graph)
  # Use identical() to check if the result is exactly NA (scalar)
  invalid_mask <- purrr::map_lgl(unique_graphs, ~ identical(.x, NA))
  invalid_unique_x <- unique_x[invalid_mask]
  if (length(invalid_unique_x) > 0) {
    cli::cli_abort("Can't parse: {.val {invalid_unique_x}}", call = call)
  }

  # Create structure from parsed unique graphs
  unique_structure <- glyrepr::as_glycan_structure(unique_graphs)

  # Build result by combining structures
  # For each position, use either the parsed structure or NA
  non_na_match_indices <- match(non_na_x, unique_x)
  
  # Create a list of structures to combine
  na_structure <- glyrepr::glycan_structure(NA)
  struct_list <- vector("list", length(x))
  
  # Fill non-NA positions with parsed structures
  non_na_counter <- 1
  for (i in seq_along(x)) {
    if (na_mask[i]) {
      struct_list[[i]] <- na_structure
    } else {
      struct_list[[i]] <- unique_structure[non_na_match_indices[non_na_counter]]
      non_na_counter <- non_na_counter + 1
    }
  }
  
  # Combine all structures
  result <- purrr::reduce(struct_list, c)

  # Restore names (only if input had names)
  if (!is.null(original_names)) {
    attr(result, "names") <- original_names
  }
  result
}
