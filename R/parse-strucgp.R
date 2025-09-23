#' Parse StrucGP Structures
#'
#' Parse StrucGP-style structure characters into a [glyrepr::glycan_structure()].
#' See example below for the structure format.
#'
#' @param x A character vector of StrucGP-style structure strings.
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' glycan <- parse_strucgp_struc("A2B2C1D1E2F1fedD1E2edcbB5ba")
#' print(glycan, verbose = TRUE)
#'
#' @export
parse_strucgp_struc <- function(x) {
  struc_parser_wrapper(x, do_parse_strucgp_struc)
}


# Parsing logic of `parse_strucgp_struc()`
do_parse_strucgp_struc <- function(x) {
  graph_env <- list(graph = igraph::make_empty_graph())
  graph <- recur_parse_strucgp(x, graph_env, 0)$graph
  mono_map <- c("1" = "Hex", "2" = "HexNAc", "3" = "NeuAc", "4" = "NeuGc", "5" = "dHex")
  igraph::V(graph)$mono <- mono_map[igraph::V(graph)$mono]
  igraph::V(graph)$sub <- ""
  igraph::E(graph)$linkage <- "??-?"
  graph$anomer <- "??"
  graph$alditol <- FALSE
  graph
}


recur_parse_strucgp <- function(x, graph_env, last_node) {
  # Base condition: `x` is "".
  if (x == "") {
    return(graph_env)
  }

  start <- stringr::str_sub(x, 1, 1)

  # If last_node is 0, this is the first layer.
  if (last_node == 0) {
    mono <- stringr::str_sub(x, 2, 2)
    graph <- igraph::add_vertices(graph_env$graph, 1, mono = mono, name = "1")
    graph_env$graph <- graph
    return(recur_parse_strucgp(stringr::str_sub(x, 3, -2), graph_env, "1"))
  }

  # If a branch appears, split `x` and dispatch to the next level.
  if (stringr::str_count(x, start) > 1) {
    end <- stringr::str_to_lower(start)
    pattern <- stringr::str_glue("{start}.*?{end}")
    for (sub_x in stringr::str_extract_all(x, pattern)[[1]]) {
      graph_env <- recur_parse_strucgp(sub_x, graph_env, last_node)
    }
    return(graph_env)
  }

  # For layer with no branch, add a node.
  else {
    # Build new graph
    mono <- stringr::str_sub(x, 2, 2)
    name = as.character(igraph::vcount(graph_env$graph) + 1)
    graph <- igraph::add_vertices(graph_env$graph, 1, mono = mono, name = name)
    graph <- igraph::add_edges(graph, c(last_node, name))
    graph_env$graph <- graph
    # Dispatch to the next level
    return(recur_parse_strucgp(stringr::str_sub(x, 3, -2), graph_env, name))
  }
}
