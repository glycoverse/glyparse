#' Wrap a structure parser with vectorization and failure handling.
#'
#' @param x A character vector of structure strings.
#' @param parser A parser function that returns one glycan graph per string.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param progress Whether to show a progress bar while parsing.
#' @param validate Whether to validate parsed glycan graphs before constructing
#'   the result.
#' @param drop_generic Whether to replace parsed generic glycans with `NA`.
#' @param call The call to report in user-facing errors.
#'
#' @return A [glyrepr::glycan_structure()] object.
#' @noRd
struc_parser_wrapper <- function(
  x,
  parser,
  on_failure = "error",
  progress = FALSE,
  validate = TRUE,
  drop_generic = FALSE,
  call = rlang::caller_env()
) {
  on_failure <- validate_struc_parser_wrapper_args(
    x,
    on_failure,
    progress,
    validate,
    drop_generic,
    call = call
  )
  wrapper_input <- prepare_struc_parser_input(x)

  if (wrapper_input$all_na) {
    return(make_na_glycan_structure(
      wrapper_input$size,
      names = wrapper_input$names
    ))
  }

  parsed_unique <- parse_unique_graphs(
    wrapper_input$unique_x,
    parser,
    progress = progress,
    validate = validate
  )
  abort_on_invalid_parse(
    parsed_unique$invalid_unique_x,
    on_failure,
    call = call
  )
  if (drop_generic) {
    parsed_unique <- drop_generic_parsed_graphs(
      wrapper_input,
      parsed_unique
    )
  }

  if (length(parsed_unique$valid_unique_x) == 0) {
    return(make_na_glycan_structure(
      wrapper_input$size,
      names = wrapper_input$names
    ))
  }

  build_wrapped_structure(
    wrapper_input = wrapper_input,
    parsed_unique = parsed_unique,
    validate = validate
  )
}


#' Normalize structure strings and parse their unique IUPAC-condensed forms.
#'
#' @param x A character vector of structure strings.
#' @param normalizer A vectorized function that converts strings to
#'   IUPAC-condensed notation.
#' @inheritParams struc_parser_wrapper
#'
#' @return A [glyrepr::glycan_structure()] object.
#' @noRd
normalized_struc_parser_wrapper <- function(
  x,
  normalizer = identity,
  on_failure = "error",
  progress = FALSE,
  drop_generic = FALSE,
  call = rlang::caller_env()
) {
  on_failure <- validate_struc_parser_wrapper_args(
    x,
    on_failure,
    progress,
    drop_generic = drop_generic,
    call = call
  )
  wrapper_input <- prepare_struc_parser_input(x)

  if (wrapper_input$all_na) {
    return(make_na_glycan_structure(
      wrapper_input$size,
      names = wrapper_input$names
    ))
  }

  normalized_unique <- normalize_unique_structures(
    wrapper_input$unique_x,
    normalizer,
    progress = progress
  )
  if (drop_generic) {
    abort_on_invalid_parse(
      wrapper_input$unique_x[is.na(normalized_unique)],
      on_failure,
      call = call
    )
    normalized_x <- normalized_unique[
      build_normalized_structure_indices(wrapper_input)
    ]
    if (!is.null(wrapper_input$names)) {
      attr(normalized_x, "names") <- wrapper_input$names
    }
    return(struc_parser_wrapper(
      normalized_x,
      do_parse_iupac_condensed,
      on_failure = on_failure,
      progress = progress,
      drop_generic = TRUE,
      call = call
    ))
  }
  unique_result <- suppressWarnings(glyrepr::as_glycan_structure(
    normalized_unique,
    on_failure = "na"
  ))
  invalid_mask <- is.na(unique_result)
  abort_on_invalid_parse(
    wrapper_input$unique_x[invalid_mask],
    on_failure,
    call = call
  )

  result <- unique_result[build_normalized_structure_indices(wrapper_input)]
  if (!is.null(wrapper_input$names)) {
    attr(result, "names") <- wrapper_input$names
  }
  result
}


#' Normalize all unique inputs together, with scalar failure recovery.
#'
#' @param unique_x A character vector of unique structure strings.
#' @param normalizer A vectorized normalization function.
#' @param progress Whether to show a progress bar during scalar recovery.
#'
#' @return A character vector of IUPAC-condensed strings.
#' @noRd
normalize_unique_structures <- function(
  unique_x,
  normalizer,
  progress = FALSE
) {
  normalized <- tryCatch(normalizer(unique_x), error = identity)
  if (!inherits(normalized, "error")) {
    return(normalized)
  }

  safe_normalize <- purrr::possibly(normalizer, otherwise = NA_character_)
  purrr::map_chr(unique_x, safe_normalize, .progress = progress)
}


#' Build indices for restoring normalized unique structures to input order.
#'
#' @param wrapper_input Prepared input metadata.
#'
#' @return An integer vector indexing a non-missing result vector.
#' @noRd
build_normalized_structure_indices <- function(wrapper_input) {
  indices <- rep(NA_integer_, wrapper_input$size)
  indices[!wrapper_input$na_mask] <- match(
    wrapper_input$non_na_x,
    wrapper_input$unique_x
  )
  indices
}


#' Validate wrapper arguments.
#'
#' @param x A character vector of structure strings.
#' @param on_failure How to handle parsing failures.
#' @param progress Whether to show a progress bar while parsing.
#' @param drop_generic Whether to replace parsed generic glycans with `NA`.
#' @param call The call to report in user-facing errors.
#'
#' @return The validated `on_failure` value.
#' @noRd
validate_struc_parser_wrapper_args <- function(
  x,
  on_failure,
  progress,
  validate = TRUE,
  drop_generic = FALSE,
  call
) {
  checkmate::assert_character(x)
  checkmate::assert_flag(progress)
  checkmate::assert_flag(validate)
  checkmate::assert_flag(drop_generic)
  rlang::arg_match(
    on_failure,
    values = c("error", "na"),
    error_call = call
  )
}


#' Replace parsed generic glycans with missing values.
#'
#' @param wrapper_input Prepared input metadata.
#' @param parsed_unique Parsed valid unique structures.
#'
#' @return `parsed_unique` without generic structures.
#' @noRd
drop_generic_parsed_graphs <- function(wrapper_input, parsed_unique) {
  mono_types <- purrr::map_chr(
    parsed_unique$valid_unique_graphs,
    glyrepr::get_mono_type
  )
  generic_mask <- mono_types == "generic"
  if (!any(generic_mask)) {
    return(parsed_unique)
  }

  generic_unique_x <- parsed_unique$valid_unique_x[generic_mask]
  n_generic <- sum(wrapper_input$non_na_x %in% generic_unique_x)
  cli::cli_inform(
    "Dropped {n_generic} generic glycan{?s} (replaced with {.code NA})."
  )

  parsed_unique$valid_unique_x <- parsed_unique$valid_unique_x[!generic_mask]
  parsed_unique$valid_unique_graphs <-
    parsed_unique$valid_unique_graphs[!generic_mask]
  parsed_unique
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


#' Parse, validate, and canonicalize unique input graphs.
#'
#' @param unique_x A character vector of unique, non-`NA` structure strings.
#' @param parser A parser function that returns one glycan graph per string.
#' @param progress Whether to show a progress bar while parsing.
#'
#' @return A list containing valid and invalid unique parse results.
#' @noRd
parse_unique_graphs <- function(
  unique_x,
  parser,
  progress = FALSE,
  validate = TRUE
) {
  safe_parse_one_graph <- purrr::possibly(
    parse_one_graph,
    otherwise = NA
  )
  parsed_unique_graphs <- purrr::map(
    unique_x,
    safe_parse_one_graph,
    parser = parser,
    validate = validate,
    .progress = progress
  )
  invalid_mask <- purrr::map_lgl(parsed_unique_graphs, ~ identical(.x, NA))
  list(
    invalid_unique_x = unique_x[invalid_mask],
    valid_unique_x = unique_x[!invalid_mask],
    valid_unique_graphs = parsed_unique_graphs[!invalid_mask]
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
build_wrapped_structure <- function(wrapper_input, parsed_unique, validate) {
  unique_result <- build_unique_wrapped_structure(
    parsed_unique$valid_unique_graphs,
    validate = validate
  )
  result_indices <- build_wrapped_structure_indices(
    wrapper_input,
    parsed_unique
  )
  result <- unique_result[result_indices]
  if (!is.null(wrapper_input$names)) {
    attr(result, "names") <- wrapper_input$names
  }
  result
}


#' Build a glycan structure vector for unique parsed structures.
#'
#' @param valid_unique_graphs A list of valid, canonical glycan graphs.
#'
#' @return A [glyrepr::glycan_structure()] object.
#' @noRd
build_unique_wrapped_structure <- function(
  valid_unique_graphs,
  validate = TRUE
) {
  if (validate) {
    glyrepr::validate_glycan_graph_vector(valid_unique_graphs)
  }
  iupacs <- purrr::map_chr(valid_unique_graphs, glyrepr::graph_to_iupac)

  unique_iupac <- !duplicated(unname(iupacs))
  unique_graphs <- valid_unique_graphs[unique_iupac]
  names(unique_graphs) <- unname(iupacs[unique_iupac])

  glyrepr::new_glycan_structure(iupacs, unique_graphs)
}


#' Build indices that map unique parser output back to input order.
#'
#' @param wrapper_input Prepared input metadata.
#' @param parsed_unique Parsed valid unique structures.
#'
#' @return An integer vector of indices into the unique result.
#' @noRd
build_wrapped_structure_indices <- function(wrapper_input, parsed_unique) {
  non_na_match_indices <- match(
    wrapper_input$non_na_x,
    parsed_unique$valid_unique_x
  )

  result_indices <- rep(NA_integer_, wrapper_input$size)
  non_na_indices <- rep(NA_integer_, length(wrapper_input$non_na_x))
  valid_non_na <- !is.na(non_na_match_indices)
  non_na_indices[valid_non_na] <- non_na_match_indices[valid_non_na]
  result_indices[!wrapper_input$na_mask] <- non_na_indices

  result_indices
}


#' Parse a single structure string into a valid, canonical graph.
#'
#' @param x A single structure string.
#' @param parser A parser function that returns a glycan graph.
#'
#' @return A valid, canonical glycan graph.
#' @noRd
parse_one_graph <- function(x, parser, validate = TRUE) {
  graph <- parser(x)
  if (validate) {
    graph <- glyrepr::validate_glycan_graph(graph)
  }
  glyrepr::canonicalize_glycan_graph(graph)
}


#' Construct an all-`NA` glycan structure vector.
#'
#' @param n Desired length.
#' @param names Optional names to restore on the result.
#'
#' @return A [glyrepr::glycan_structure()] object filled with `NA`.
#' @noRd
make_na_glycan_structure <- function(n, names = NULL) {
  iupacs <- rep(NA_character_, n)
  attr(iupacs, "names") <- names
  glyrepr::new_glycan_structure(iupacs)
}
