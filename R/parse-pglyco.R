#' Parse pGlyco Structures
#'
#' Parse pGlyco-style structure characters into a [glyrepr::glycan_structure()].
#' See example below for the structure format.
#'
#' @param x A character vector of pGlyco-style structure strings.
#'
#' @return A [glyrepr::glycan_structure()] object.
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
  monos <- stringr::str_split_1(x, "[//(, \\)]")
  monos <- monos[monos != ""]
  g <- igraph::make_empty_graph(n = length(monos), directed = TRUE)
  igraph::V(g)$name <- seq_along(monos)
  igraph::V(g)$mono <- monos
  igraph::V(g)$sub <- ""

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
  igraph::V(g)$mono <- dplyr::recode(igraph::V(g)$mono, !!!mono_map, .default = igraph::V(g)$mono)

  igraph::E(g)$linkage <- "??-?"
  g$anomer <- "??"
  g$alditol <- FALSE
  g
}
