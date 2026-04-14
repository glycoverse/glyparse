#' Wrap a structure parser with vectorization and failure handling.
#'
#' @param x A character vector of structure strings.
#' @param parser A parser function that returns one glycan graph per string.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param call The call to report in user-facing errors.
#'
#' @return A [glyrepr::glycan_structure()] object.
#' @noRd
struc_parser_wrapper <- function(
    x,
    parser,
    on_failure = "error",
    call = rlang::caller_env()
) {
  checkmate::assert_character(x)
  on_failure <- rlang::arg_match(
    on_failure,
    values = c("error", "na"),
    error_call = call
  )

  # Preserve names from input
  original_names <- names(x)

  # Identify NA positions
  na_mask <- is.na(x)

  # Handle edge case: all NA
  if (all(na_mask)) {
    return(make_na_glycan_structure(length(x), names = original_names))
  }

  # Get unique non-NA values to avoid redundant computation
  non_na_x <- x[!na_mask]
  unique_x <- unique(non_na_x)

  # Parse only unique values, while normalizing late validation failures
  safe_parse_one_structure <- purrr::possibly(
    parse_one_structure,
    otherwise = NA
  )
  parsed_unique_structures <- purrr::map(
    unique_x,
    safe_parse_one_structure,
    parser = parser
  )

  invalid_mask <- purrr::map_lgl(parsed_unique_structures, ~ identical(.x, NA))
  invalid_unique_x <- unique_x[invalid_mask]
  if (length(invalid_unique_x) > 0 && on_failure == "error") {
    cli::cli_abort("Can't parse: {.val {invalid_unique_x}}", call = call)
  }

  valid_unique_x <- unique_x[!invalid_mask]
  valid_unique_structures <- parsed_unique_structures[!invalid_mask]

  if (length(valid_unique_structures) == 0) {
    return(make_na_glycan_structure(length(x), names = original_names))
  }

  valid_unique_iupacs <- purrr::map_chr(
    valid_unique_structures,
    ~ vctrs::vec_data(.x)[[1]]
  )
  graph_keep_mask <- !duplicated(valid_unique_iupacs)
  valid_graphs <- purrr::map(
    valid_unique_structures[graph_keep_mask],
    ~ attr(.x, "graphs")[[1]]
  )
  names(valid_graphs) <- valid_unique_iupacs[graph_keep_mask]

  # Build result by directly constructing the IUPAC vector
  non_na_match_indices <- match(non_na_x, valid_unique_x)

  result_iupacs <- character(length(x))
  result_iupacs[na_mask] <- NA_character_
  result_iupacs[!na_mask] <- valid_unique_iupacs[non_na_match_indices]

  result <- glyrepr:::new_glycan_structure(result_iupacs, valid_graphs)

  # Restore names (only if input had names)
  if (!is.null(original_names)) {
    attr(result, "names") <- original_names
  }
  result
}


#' Parse a single structure string into a validated glycan structure.
#'
#' @param x A single structure string.
#' @param parser A parser function that returns a glycan graph.
#'
#' @return A length-1 [glyrepr::glycan_structure()] object.
#' @noRd
parse_one_structure <- function(x, parser) {
  glyrepr::as_glycan_structure(list(parser(x)))
}


#' Construct an all-`NA` glycan structure vector.
#'
#' @param n Desired length.
#' @param names Optional names to restore on the result.
#'
#' @return A [glyrepr::glycan_structure()] object filled with `NA`.
#' @noRd
make_na_glycan_structure <- function(n, names = NULL) {
  result <- glyrepr::glycan_structure(NA)
  result <- result[rep(1, n)]
  if (!is.null(names)) {
    attr(result, "names") <- names
  }
  result
}
