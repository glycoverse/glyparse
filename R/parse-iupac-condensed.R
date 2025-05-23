#' Parse IUPAC-condensed Structures
#'
#' This function parses IUPAC-condensed strings into a glycan graph.
#' For more information about IUPAC-condensed notation, see
#' [https://doi.org/10.1351/pac199668101919](https://doi.org/10.1351/pac199668101919).
#'
#' @details
#' The IUPAC-condensed notation is a compact form of IUPAC-extended notation.
#' It is used by the [GlyConnect](https://glyconnect.expasy.org/) database.
#' It contains the following information:
#' - Monosaccharide name, e.g. "Gal", "GlcNAc", "Neu5Ac".
#' - Substituent, e.g. "9Ac", "4Ac", "3Me", "?S".
#' - Linkage, e.g. "b1-3", "a1-2", "a1-?".
#'
#' An example of IUPAC-condensed string is "Gal(b1-3)GlcNAc(b1-4)Glc(a1-".
#'
#' The reducing-end monosaccharide can be with or without anomer information.
#' For example, the two strings below are all valid:
#' - "Neu5Ac(a2-"
#' - "Neu5Ac"
#'
#' In the first case, the anomer is "a2".
#' In the second case, the anomer is "?2".
#'
#' @param x A character vector of IUPAC-condensed strings.
#' @param mode A character string, either "ne" or "dn". Default is "ne".
#' For more information about "ne" and "dn", see [glyrepr::as_glycan_graph()].
#'
#' @return A glycan graph if `x` is a single character,
#' or a list of glycan graphs if `x` is a character vector.
#'
#' @examples
#' iupac <- "Gal(b1-3)GlcNAc(b1-4)Glc(a1-"
#' parse_iupac_condensed(iupac)
#'
#' @seealso [parse_iupac_short()], [parse_iupac_extended()]
#'
#' @export
parse_iupac_condensed <- function(x, mode = "ne") {
  struc_parser_wrapper(x, do_parse_iupac_condensed, mode = mode)
}


# The parsing logic of `parse_iupac_condensed()`
do_parse_iupac_condensed <- function(x) {
  anomer <- extract_anomer(x)  # may be NA
  if (!is.na(anomer)) {
    x <- stringr::str_sub(x, 1, -stringr::str_length(anomer)-3)
  }

  alditol <- extract_alditol(x)
  if (alditol) {
    x <- stringr::str_replace(x, "-ol", "")
  }

  tokens <- tokenize_iupac(x)

  # Deal with missing anomer
  first_mono_sub_res <- extract_substituent(tokens[[1]])
  if (is.na(anomer)) {
    anomer <- local({
      anomer_pos <- decide_anomer_pos(first_mono_sub_res[["mono"]])
      paste0("?", anomer_pos)
    })
  }

  # Create a new graph and add the first node
  graph <- igraph::make_empty_graph()
  graph <- igraph::add_vertices(
    graph, 1, name = "1",
    mono = first_mono_sub_res[["mono"]],
    sub = first_mono_sub_res[["sub"]]
  )

  if (length(tokens) == 1) {
    graph <- igraph::set_edge_attr(graph, "linkage", value = character(0))
    graph$anomer <- anomer
    graph$alditol <- alditol
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

  graph$anomer <- anomer
  graph$alditol <- alditol
  glyrepr::as_ne_glycan_graph(graph)
}


extract_anomer <- function(iupac) {
  # This function extracts the anomer of a IUPAC condensed string.
  # e.g. "Neu5Ac(a2-" -> "a2"
  # e.g. "Neu5Ac" -> NA
  p <- "\\(([ab\\?][\\d\\?])-$"
  if (stringr::str_detect(iupac, p)) {
    stringr::str_extract(iupac, p, group = 1)
  } else {
    NA_character_
  }
}


extract_alditol <- function(iupac) {
  # This function extracts the alditol of a IUPAC condensed string.
  stringr::str_detect(iupac, "-ol")
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

  # anomer is either "a", "b", or "?".
  anomer_p <- "[ab\\?]"

  # pos1 is either "1", "2", or "?".
  pos1_p <- "([12\\?])"

  # pos2 is "1"-"9" (followed by any number of "/1-9"), or "?"
  # e.g. "1", "2/3", "?" are all valid
  pos2_p <- "([1-9](/[1-9])*|\\?)"

  # Linkage pattern is "a1-2", "b1-3/6", "a?-3", etc.
  linkage_pattern <- stringr::str_glue("{anomer_p}{pos1_p}-{pos2_p}")

  # Monosaccharide name contains only word characters and "?",
  # and linkages are optional.
  # e.g. Glc(b1-3), GlcNAc
  mono_linkage_pattern <- stringr::str_glue("[\\w\\?]+(\\({linkage_pattern}\\))?")

  # The pattern is either a monosaccharide name or a bracket.
  pattern <- paste(mono_linkage_pattern, "\\[", "\\]", sep = "|")

  tokens <- stringr::str_extract_all(iupac, pattern)[[1]]
  tokens <- stringr::str_replace(tokens, "\\[", "TEMP_LEFT")
  tokens <- stringr::str_replace(tokens, "\\]", "TEMP_RIGHT")
  tokens <- stringr::str_replace(tokens, "TEMP_LEFT", "\\]")
  tokens <- stringr::str_replace(tokens, "TEMP_RIGHT", "\\[")

  # Reverse the tokens to make the first monosaccharide the reducing end.
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
  # e.g. "Neu4Ac5Ac" -> c(mono = "Neu5Ac", sub = "4Ac")
  subs_pattern <- stringr::str_c(glyrepr::available_substituents(), collapse = "|")
  subs_pattern <- stringr::str_glue("[\\d\\?]({subs_pattern})$")
  if (mono == "Neu5Ac") {
    # "Neu5Ac" is special that it satisfies the regex below,
    # but should not be split.
    c(mono = mono, sub = "")
  } else if (mono == "Neu4Ac5Ac") {
    c(mono = "Neu5Ac", sub = "4Ac")
  } else if (mono == "Neu4Ac5Gc") {
    c(mono = "Neu5Gc", sub = "4Ac")
  } else if (stringr::str_detect(mono, subs_pattern)) {
    sub_loc <- stringr::str_locate(mono, subs_pattern)[1]
    sub <- stringr::str_sub(mono, sub_loc, -1)
    mono <- stringr::str_sub(mono, 1, sub_loc - 1)
    c(mono = mono, sub = sub)
  } else {
    c(mono = mono, sub = "")
  }
}
