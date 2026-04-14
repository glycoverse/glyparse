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
  on_failure <- validate_struc_parser_wrapper_args(x, on_failure, call = call)
  wrapper_input <- prepare_struc_parser_input(x)

  if (wrapper_input$all_na) {
    return(make_na_glycan_structure(wrapper_input$size, names = wrapper_input$names))
  }

  parsed_unique <- parse_unique_structures(wrapper_input$unique_x, parser)
  abort_on_invalid_parse(parsed_unique$invalid_unique_x, on_failure, call = call)

  if (length(parsed_unique$valid_unique_x) == 0) {
    return(make_na_glycan_structure(wrapper_input$size, names = wrapper_input$names))
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
#' @param call The call to report in user-facing errors.
#'
#' @return The validated `on_failure` value.
#' @noRd
validate_struc_parser_wrapper_args <- function(x, on_failure, call) {
  checkmate::assert_character(x)
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
#'
#' @return A list containing valid and invalid unique parse results.
#' @noRd
parse_unique_structures <- function(unique_x, parser) {
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
  structure_payload <- extract_structure_payload(
    parsed_unique$valid_unique_structures
  )
  non_na_match_indices <- match(
    wrapper_input$non_na_x,
    parsed_unique$valid_unique_x
  )

  result_iupacs <- character(wrapper_input$size)
  result_iupacs[wrapper_input$na_mask] <- NA_character_
  result_iupacs[!wrapper_input$na_mask] <-
    structure_payload$iupacs[non_na_match_indices]

  result <- glyrepr:::new_glycan_structure(
    result_iupacs,
    structure_payload$graphs
  )
  if (!is.null(wrapper_input$names)) {
    attr(result, "names") <- wrapper_input$names
  }
  result
}


#' Extract IUPAC codes and graph lookup payload from parsed structures.
#'
#' @param valid_unique_structures A list of length-1 glycan structures.
#'
#' @return A list with `iupacs` and named `graphs`.
#' @noRd
extract_structure_payload <- function(valid_unique_structures) {
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
  list(
    iupacs = valid_unique_iupacs,
    graphs = valid_graphs
  )
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
