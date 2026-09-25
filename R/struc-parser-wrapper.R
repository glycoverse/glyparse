#' Wrap a structure parser with vectorization and failure handling.
#'
#' @param x A character vector of structure strings.
#' @param parser A parser function that returns one structure array record per string.
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

  records <- if (is.character(parser)) {
    native_records(wrapper_input$unique_x, parser, progress)
  } else {
    purrr::map(
      wrapper_input$unique_x,
      purrr::possibly(parser, otherwise = NULL),
      .progress = progress
    )
  }
  unique_result <- suppressWarnings(glyrepr::structure_from_arrays(
    records,
    on_failure = "na"
  ))
  abort_on_invalid_parse(
    wrapper_input$unique_x[is.na(unique_result)],
    on_failure,
    call = call
  )
  result <- unique_result[build_normalized_structure_indices(wrapper_input)]
  if (!is.null(wrapper_input$names)) {
    attr(result, "names") <- wrapper_input$names
  }
  result
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

  normalized_unique <- normalize_unique_structures(
    wrapper_input$unique_x,
    normalizer,
    progress = progress
  )
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
#' @param call The call to report in user-facing errors.
#'
#' @return The validated `on_failure` value.
#' @noRd
validate_struc_parser_wrapper_args <- function(
  x,
  on_failure,
  progress,
  call
) {
  checkmate::assert_character(x)
  checkmate::assert_flag(progress)
  rlang::arg_match(
    on_failure,
    values = c("error", "na"),
    error_call = call
  )
}


warn_deprecated_validate <- function(parser) {
  lifecycle::deprecate_warn(
    when = "0.9.0",
    what = paste0(parser, "(validate)"),
    details = "The `validate` argument is ignored; validation is always performed.",
    user_env = rlang::caller_env(2)
  )
  invisible(NULL)
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
