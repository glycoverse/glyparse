#' Wrap a structure parser with vectorization and failure handling.
#'
#' @param x A character vector of structure strings.
#' @param parser A parser function that returns one glycan graph per string.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param progress Whether to show a progress bar while parsing.
#' @param call The call to report in user-facing errors.
#'
#' @return A [glyrepr::glycan_structure()] object.
#' @noRd
struc_parser_wrapper <- function(
  x,
  parser,
  on_failure = "error",
  progress = FALSE,
  call = rlang::caller_env()
) {
  on_failure <- validate_struc_parser_wrapper_args(
    x,
    on_failure,
    progress,
    call = call
  )
  wrapper_input <- prepare_struc_parser_input(x)

  if (wrapper_input$all_na) {
    return(make_na_glycan_structure(
      wrapper_input$size,
      names = wrapper_input$names
    ))
  }

  parsed_unique <- parse_unique_structures(
    wrapper_input$unique_x,
    parser,
    progress = progress
  )
  abort_on_invalid_parse(
    parsed_unique$invalid_unique_x,
    on_failure,
    call = call
  )

  if (length(parsed_unique$valid_unique_x) == 0) {
    return(make_na_glycan_structure(
      wrapper_input$size,
      names = wrapper_input$names
    ))
  }

  build_wrapped_structure(
    wrapper_input = wrapper_input,
    parsed_unique = parsed_unique
  )
}


#' Validate wrapper arguments.
#'
#' @param x A character vector of structure strings.
#' @param on_failure How to handle parsing failures.
#' @param progress Whether to show a progress bar while parsing.
#' @param call The call to report in user-facing errors.
#'
#' @return The validated `on_failure` value.
#' @noRd
validate_struc_parser_wrapper_args <- function(x, on_failure, progress, call) {
  checkmate::assert_character(x)
  checkmate::assert_flag(progress)
  rlang::arg_match(
    on_failure,
    values = c("error", "na"),
    error_call = call
  )
}


#' Prepare vectorized parser input metadata.
#'
#' @param x A character vector of structure strings.
#'
#' @return A list with input metadata used by `struc_parser_wrapper()`.
#' @noRd
prepare_struc_parser_input <- function(x) {
  na_mask <- is.na(x)
  non_na_x <- x[!na_mask]
  list(
    size = length(x),
    names = names(x),
    na_mask = na_mask,
    non_na_x = non_na_x,
    unique_x = unique(non_na_x),
    all_na = all(na_mask)
  )
}


#' Parse unique input structures and separate failures.
#'
#' @param unique_x A character vector of unique, non-`NA` structure strings.
#' @param parser A parser function that returns one glycan graph per string.
#' @param progress Whether to show a progress bar while parsing.
#'
#' @return A list containing valid and invalid unique parse results.
#' @noRd
parse_unique_structures <- function(unique_x, parser, progress = FALSE) {
  safe_parse_one_structure <- purrr::possibly(
    parse_one_structure,
    otherwise = NA
  )
  parsed_unique_structures <- purrr::map(
    unique_x,
    safe_parse_one_structure,
    parser = parser,
    .progress = progress
  )
  invalid_mask <- purrr::map_lgl(parsed_unique_structures, ~ identical(.x, NA))
  list(
    invalid_unique_x = unique_x[invalid_mask],
    valid_unique_x = unique_x[!invalid_mask],
    valid_unique_structures = parsed_unique_structures[!invalid_mask]
  )
}


#' Abort on invalid parse inputs when requested.
#'
#' @param invalid_unique_x Unique structure strings that could not be parsed.
#' @param on_failure How to handle parsing failures.
#' @param call The call to report in user-facing errors.
#'
#' @return `NULL`, invisibly.
#' @noRd
abort_on_invalid_parse <- function(invalid_unique_x, on_failure, call) {
  if (length(invalid_unique_x) > 0 && on_failure == "error") {
    cli::cli_abort("Can't parse: {.val {invalid_unique_x}}", call = call)
  }
  invisible(NULL)
}


#' Build the final wrapped glycan structure result.
#'
#' @param wrapper_input Prepared input metadata.
#' @param parsed_unique Parsed valid unique structures.
#'
#' @return A [glyrepr::glycan_structure()] object.
#' @noRd
build_wrapped_structure <- function(wrapper_input, parsed_unique) {
  result_graphs <- build_wrapped_structure_graphs(
    wrapper_input = wrapper_input,
    parsed_unique = parsed_unique
  )
  result <- do.call(glyrepr::glycan_structure, result_graphs)
  if (!is.null(wrapper_input$names)) {
    attr(result, "names") <- wrapper_input$names
  }
  result
}


#' Build final graph arguments for wrapped parser output.
#'
#' @param wrapper_input Prepared input metadata.
#' @param parsed_unique Parsed valid unique structures.
#'
#' @return A list of igraph objects and `NA` values in original input order.
#' @noRd
build_wrapped_structure_graphs <- function(wrapper_input, parsed_unique) {
  valid_graphs <- purrr::map(
    parsed_unique$valid_unique_structures,
    glyrepr::get_structure_graphs,
    return_list = FALSE
  )
  non_na_match_indices <- match(
    wrapper_input$non_na_x,
    parsed_unique$valid_unique_x
  )

  result_graphs <- rep(list(NA_character_), wrapper_input$size)
  non_na_graphs <- rep(list(NA_character_), length(wrapper_input$non_na_x))
  valid_non_na <- !is.na(non_na_match_indices)
  non_na_graphs[valid_non_na] <-
    valid_graphs[non_na_match_indices[valid_non_na]]
  result_graphs[!wrapper_input$na_mask] <- non_na_graphs

  result_graphs
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
