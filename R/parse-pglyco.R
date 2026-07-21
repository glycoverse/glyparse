#' Parse pGlyco Structures
#'
#' Parse pGlyco-style structure characters into a [glyrepr::glycan_structure()].
#' See example below for the structure format.
#'
#' @param x A character vector of pGlyco-style structure strings. NA values are allowed and will be returned as NA structures.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param progress Whether to show a progress bar while parsing.
#' @param validate Whether to validate parsed glycan graphs before constructing
#'   the result.
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
    do_parse_pglyco_struc,
    on_failure = on_failure,
    progress = progress,
    validate = validate
  )
}


# Parsing logic of `parse_pglyco_struc()`
do_parse_pglyco_struc <- function(x) {
  monos <- stringr::str_split_1(x, "[//(, \\)]")
  monos <- monos[monos != ""]
  g <- igraph::make_empty_graph(n = length(monos), directed = TRUE)
  igraph::V(g)$name <- seq_along(monos)
  igraph::V(g)$mono <- monos
  igraph::V(g)$sub <- ""

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
  if (length(edges) > 0L) {
    g <- igraph::add_edges(g, edges)
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
  igraph::V(g)$sub <- dplyr::if_else(igraph::V(g)$mono == "pH", "?P", "")
  igraph::V(g)$mono <- dplyr::recode(
    igraph::V(g)$mono,
    !!!mono_map,
    .default = igraph::V(g)$mono
  )

  igraph::E(g)$linkage <- "??-?"
  g$anomer <- "??"
  g$alditol <- FALSE
  g
}
