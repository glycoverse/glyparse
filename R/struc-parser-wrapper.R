#' Wrapper for Structure Parsers
#'
#' @description
#' This wrapper function adds the following functionalities to a
#' structure parser function:
#' - Check input type;
#' - Vectorized parsing;
#' - More informative error messages.
#'
#' This wrapper is used by all parser interfaces in the package,
#' e.g. [parse_pglyco_struc()].
#' If you are writing a new parser, you can use this function to
#' wrap your parser function.
#'
#' @param x A character vector of structure strings.
#' @param parser A function that parses a single structure string
#' and returns a glycan graph (see [glyrepr::as_glycan_graph()]).
#' @param mode A character string, either "ne" or "dn".
#'
#' @return A glycan graph if `x` is a single character,
#' or a list of glycan graphs if `x` is a character vector.
struc_parser_wrapper <- function(x, parser, mode = "ne") {
  # Check input type
  checkmate::assert_character(x)
  checkmate::assert_function(parser)
  checkmate::assert_choice(mode, c("ne", "dn"))

  # One character
  if (length(x) == 1) {
    glycan <- tryCatch(
      parser(x),
      error = function(e) {
        cli::cli_abort(c("Could not be parsed.", i = conditionMessage(e)))
      }
    )
    if (mode == "dn") {
      glycan <- glyrepr::convert_graph_mode(glycan, to = "dn")
    }
    return(glycan)
  } else {  # Multiple characters
    result <- vector("list", length = length(x))
    failed <- rep(FALSE, length(x))
    for (i in seq_along(x)){
      tryCatch(
        result[[i]] <- parser(x[[i]]),
        error = function(e) failed[[i]] <<- TRUE
      )
    }
    if (any(failed)) {
      cli::cli_abort("These could not be parsed: {.val {x[failed]}}")
    }
    if (mode == "dn") {
      result <- purrr::map(result, glyrepr::convert_graph_mode, to = "dn")
    }
    return(result)
  }
}
