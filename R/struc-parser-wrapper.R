# This wrapper function adds the following functionalities to a
# structure parser function:
# - Check input type;
# - Vectorized parsing;
# - More informative error messages.
struc_parser_wrapper <- function(x, parser) {
  # Check input type
  checkmate::assert_character(x)
  checkmate::assert_function(parser)

  # One character
  if (length(x) == 1) {
    glycan <- tryCatch(
      parser(x),
      error = function(e) {
        cli::cli_abort(c("Could not be parsed.", i = conditionMessage(e)))
      }
    )
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
    return(result)
  }
}
