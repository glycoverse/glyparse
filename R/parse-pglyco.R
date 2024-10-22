#' Parse pGlyco Structures
#'
#' Parse pGlyco-style structure characters into glycan graphs.
#' See example below for the structure format.
#'
#' @param x A character vector of pGlyco-style structure strings.
#'
#' @return A glycan graph if `x` is a single character,
#' or a list of glycan graphs if `x` is a character vector.
#'
#' @examples
#' glycan <- parse_pglyco_struc("(N(F)(N(H(H(N))(H(N(H))))))")
#' print(glycan, verbose = TRUE)
#'
#' @export
parse_pglyco_struc <- function(x) {
  struc_parser_wrapper(x, do_parse_pglyco_struc)
}


# Parsing logic of `parse_pglyco_struc()`
do_parse_pglyco_struc <- function(x) {
  x <- stringr::str_replace_all(x, "A", "S")
  monos <- stringr::str_split_1(stringr::str_replace_all(x, "[//(, \\)]", ""), "")
  g <- igraph::make_empty_graph(n = length(monos), directed = TRUE)
  igraph::V(g)$mono <- monos

  parentheses <- stringr::str_split_1(stringr::str_replace_all(x, "[^()]", ""), "")
  current_node <- 1
  node_stack <- rstackdeque::rstack()
  node_stack <- rstackdeque::insert_top(node_stack, 1)
  for (i in 2:length(parentheses)) {
    if (parentheses[[i]] == "(") {
      current_node <- current_node + 1
      g <- igraph::add_edges(g, c(rstackdeque::peek_top(node_stack), current_node))
      node_stack <- rstackdeque::insert_top(node_stack, current_node)
    } else {  # must be ")"
      node_stack <- rstackdeque::without_top(node_stack)
    }
  }

  igraph::E(g)$linkage <- NA_character_
  glyrepr::as_ne_glycan_graph(g)
}
