#' Parse StrucGP Structures
#'
#' Parse StrucGP-style structure characters into a [glyrepr::glycan_structure()].
#' See example below for the structure format.
#'
#' @param x A character vector of StrucGP-style structure strings. NA values are allowed and will be returned as NA structures.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param progress Whether to show a progress bar while parsing.
#' @param validate Retained for compatibility. Array records are always validated
#'   by [glyrepr::structure_from_arrays()], including when `FALSE`.
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' glycan <- parse_strucgp_struc("A2B2C1D1E2F1fedD1E2edcbB5ba")
#' print(glycan, verbose = TRUE)
#'
#' @export
parse_strucgp_struc <- function(
  x,
  on_failure = "error",
  progress = FALSE,
  validate = TRUE
) {
  struc_parser_wrapper(
    x,
    parse_strucgp_struc_arrays,
    on_failure = on_failure,
    progress = progress,
    validate = validate
  )
}


# Parsing logic of `parse_strucgp_struc()`
parse_strucgp_struc_arrays <- function(x) {
  node_capacity <- stringr::str_count(x, "[A-Z]")
  state <- new.env(parent = emptyenv())
  state$count <- 0L
  state$monos <- character(node_capacity)
  state$parents <- integer(node_capacity)
  recur_parse_strucgp(x, state, 0L)

  edges <- integer()
  if (state$count > 1L) {
    child <- seq.int(2L, state$count)
    edges <- as.vector(rbind(state$parents[child], child))
  }

  mono_map <- c(
    "1" = "Hex",
    "2" = "HexNAc",
    "3" = "NeuAc",
    "4" = "NeuGc",
    "5" = "dHex"
  )
  list(
    mono = unname(mono_map[state$monos[seq_len(state$count)]]),
    sub = rep("", state$count),
    edges = edges,
    linkage = rep("??-?", length(edges) / 2L),
    anomer = "??",
    alditol = FALSE
  )
}


recur_parse_strucgp <- function(x, state, last_node) {
  # Base condition: `x` is "".
  if (x == "") {
    return(invisible(NULL))
  }

  start <- stringr::str_sub(x, 1, 1)

  # If last_node is 0, this is the first layer.
  if (last_node == 0) {
    state$count <- 1L
    state$monos[[1L]] <- stringr::str_sub(x, 2, 2)
    recur_parse_strucgp(stringr::str_sub(x, 3, -2), state, 1L)
    return(invisible(NULL))
  }

  # If a branch appears, split `x` and dispatch to the next level.
  if (stringr::str_count(x, start) > 1) {
    end <- stringr::str_to_lower(start)
    pattern <- stringr::str_glue("{start}.*?{end}")
    for (sub_x in stringr::str_extract_all(x, pattern)[[1]]) {
      recur_parse_strucgp(sub_x, state, last_node)
    }
    return(invisible(NULL))
  } else {
    node <- state$count + 1L
    state$count <- node
    state$monos[[node]] <- stringr::str_sub(x, 2, 2)
    state$parents[[node]] <- last_node
    recur_parse_strucgp(stringr::str_sub(x, 3, -2), state, node)
    return(invisible(NULL))
  }
}
