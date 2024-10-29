#' Parse IUPAC-condensed Structures
#'
#' This function parses IUPAC-condensed strings into a glycan graph.
#' For more information about IUPAC-condensed notation, see
#' https://doi.org/10.1351/pac199668101919.
#'
#' @param x A character vector of IUPAC-condensed strings.
#' @param mode A character string, either "ne" or "dn". Default is "ne".
#' For more information about "ne" and "dn", see [glyrepr::as_glycan_graph()].
#'
#' @return A glycan graph if `x` is a single character,
#' or a list of glycan graphs if `x` is a character vector.
#'
#' @examples
#' glycan <- parse_iupac_condensed("Gal(b1-6)Glc(a1-2)GalNAc")
#' print(glycan, verbose = TRUE)
#'
#' @export
parse_iupac_condensed <- function(x, mode = "ne") {
  struc_parser_wrapper(x, do_parse_iupac_condensed, mode = mode)
}


# The parsing logic of `parse_iupac_condensed()`
do_parse_iupac_condensed <- function(x) {
  tokens <- tokenize_iupac(x)

  # Create a new graph and add the first node
  graph <- igraph::make_empty_graph()
  first_mono_sub_res <- extract_substituent(tokens[[1]])
  graph <- igraph::add_vertices(
    graph, 1, name = "1",
    mono = first_mono_sub_res[["mono"]],
    sub = first_mono_sub_res[["sub"]]
  )

  if (length(tokens) == 1) {
    graph <- igraph::set_edge_attr(graph, "linkage", value = character(0))
    return(glyrepr::as_ne_glycan_graph(graph))
  }

  # Iterate over the tokens
  node_stack <- rstackdeque::rstack()
  node_stack <- rstackdeque::insert_top(node_stack, 1)
  current_node_id <- 1
  for (token in tokens[2:length(tokens)]) {
    if (token == "[") {
      node_stack <- rstackdeque::insert_top(node_stack, current_node_id)
    } else if (token == "]") {
      current_node_id <- rstackdeque::peek_top(node_stack)
      node_stack <- rstackdeque::without_top(node_stack)
    } else {
      parsed_token <- parse_token(token)
      new_node_id <- igraph::vcount(graph) + 1
      graph <- igraph::add_vertices(
        graph, 1,
        name = as.character(new_node_id),
        mono = parsed_token[["mono"]],
        sub = parsed_token[["sub"]]
      )
      graph <- igraph::add_edges(
        graph,
        c(current_node_id, new_node_id),
        linkage = parsed_token[["linkage"]]
      )
      current_node_id <- new_node_id
    }
  }

  glyrepr::as_ne_glycan_graph(graph)
}


tokenize_iupac <- function(iupac) {
  # This function tokenizes an IUPAC condensed string.
  # A token is either a monosaccharide name (with linkage) or a bracket.
  # e.g. "GalNAc(b1-3)", "[", "]", "GalNAc".
  #
  # Another thing is that the tokens are reversed.
  # This is because IUPAC condensed strings are written from terminal end to reducing end.
  # This order is not suitable for graph construction.
  # After reversing, the first monosaacharide is the reducing end.
  # Note that we also swap "[" and "]" to reduce confusion when parsing.
  linkage_pattern <- "[ab\\?][\\d\\?]-[\\d\\?]"
  mono_linkage_pattern <- stringr::str_glue("\\w+(\\({linkage_pattern}\\))?")
  pattern <- paste(mono_linkage_pattern, "\\[", "\\]", sep = "|")
  tokens <- stringr::str_extract_all(iupac, pattern)[[1]]
  tokens <- stringr::str_replace(tokens, "\\[", "TEMP_LEFT")
  tokens <- stringr::str_replace(tokens, "\\]", "TEMP_RIGHT")
  tokens <- stringr::str_replace(tokens, "TEMP_LEFT", "\\]")
  tokens <- stringr::str_replace(tokens, "TEMP_RIGHT", "\\[")
  rev(tokens)
}


parse_token <- function(token) {
  # This function parses a token (e.g. "Gal(b1-3)) into monosaccharide and linkage.
  # It returns c(mono, sub, linkage).
  left_bracket_pos <- stringr::str_locate(token, "\\(")[1]
  mono <- stringr::str_sub(token, 1, left_bracket_pos - 1)
  mono_sub_res <- extract_substituent(mono)
  mono <- mono_sub_res[["mono"]]
  sub <- mono_sub_res[["sub"]]
  linkage <- stringr::str_sub(token, left_bracket_pos + 1, -2)
  c(mono = mono, sub = sub, linkage = linkage)
}


extract_substituent <- function(mono) {
  # This function extract substituent, if any.
  # It returns c(mono, sub).
  # e.g. "Neu5Ac9Ac" -> c(mono = "Neu5Ac", sub = "9Ac")
  if (mono == "Neu5Ac") {
    # "Neu5Ac" is special that it satisfies the regex below,
    # but should not be split.
    c(mono = mono, sub = "")
  } else if (stringr::str_detect(mono, "\\d(S|P|Ac)$")) {
    sub_loc <- stringr::str_locate(mono, "\\d(S|P|Ac)$")[1]
    sub <- stringr::str_sub(mono, sub_loc, -1)
    mono <- stringr::str_sub(mono, 1, sub_loc - 1)
    c(mono = mono, sub = sub)
  } else {
    c(mono = mono, sub = "")
  }
}
