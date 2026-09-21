#' Parse pGlyco Structures
#'
#' Parse pGlyco-style structure characters into a [glyrepr::glycan_structure()].
#' See example below for the structure format.
#'
#' @param x A character vector of pGlyco-style structure strings. NA values are allowed and will be returned as NA structures.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param progress Whether to show a progress bar while parsing.
#' @param validate Retained for compatibility. Array records are always validated
#'   by [glyrepr::structure_from_arrays()], including when `FALSE`.
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' glycan <- parse_pglyco_struc("(N(F)(N(H(H(N))(H(N(H))))))")
#' print(glycan, verbose = TRUE)
#'
#' @export
parse_pglyco_struc <- function(
  x,
  on_failure = "error",
  progress = FALSE,
  validate = TRUE
) {
  struc_parser_wrapper(
    x,
    parse_pglyco_struc_arrays,
    on_failure = on_failure,
    progress = progress,
    validate = validate
  )
}


# Parsing logic of `parse_pglyco_struc()`
parse_pglyco_struc_arrays <- function(x) {
  monos <- stringr::str_split_1(x, "[//(, \\)]")
  monos <- monos[monos != ""]
  parentheses <- stringr::str_split_1(
    stringr::str_replace_all(x, "[^()]", ""),
    ""
  )
  edge_count <- sum(parentheses == "(") - 1L
  edges <- integer(edge_count * 2L)
  edge_index <- 0L
  current_node <- 1L
  node_stack <- rstackdeque::rstack()
  node_stack <- rstackdeque::insert_top(node_stack, 1L)
  for (i in 2:length(parentheses)) {
    if (parentheses[[i]] == "(") {
      current_node <- current_node + 1L
      edge_index <- edge_index + 1L
      edges[[2L * edge_index - 1L]] <- rstackdeque::peek_top(node_stack)
      edges[[2L * edge_index]] <- current_node
      node_stack <- rstackdeque::insert_top(node_stack, current_node)
    } else {
      # must be ")"
      node_stack <- rstackdeque::without_top(node_stack)
    }
  }
  # Map pGlyco monosaccharide codes to standard names
  mono_map <- c(
    "H" = "Hex",
    "N" = "HexNAc",
    "F" = "dHex",
    "A" = "NeuAc",
    "G" = "NeuGc",
    "aH" = "HexN",
    "pH" = "Hex"
  )
  list(
    mono = unname(dplyr::recode(monos, !!!mono_map, .default = monos)),
    sub = ifelse(monos == "pH", "?P", ""),
    edges = edges,
    linkage = rep("??-?", length(edges) / 2L),
    anomer = "??",
    alditol = FALSE
  )
}
