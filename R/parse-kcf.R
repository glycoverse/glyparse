#' Parse KCF Structures
#'
#' This function parses KCF strings into a [glyrepr::glycan_structure()].
#' KCF is a graph-oriented format used by KEGG GLYCAN.
#'
#' @param x A character vector of KCF strings. NA values are allowed and will be returned as NA structures.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param progress Whether to show a progress bar while parsing.
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' kcf <- paste0(
#'   "ENTRY       G00066                      Glycan\n",
#'   "NODE        6\n",
#'   "            1   Cer        18     0\n",
#'   "            2   Glc        12     0\n",
#'   "            3   Gal         6     0\n",
#'   "            4   GlcNAc     -2     0\n",
#'   "            5   Gal       -10     0\n",
#'   "            6   GlcNAc    -18     0\n",
#'   "EDGE        5\n",
#'   "            1     2:b1    1:1\n",
#'   "            2     3:b1    2:4\n",
#'   "            3     4:b1    3:3\n",
#'   "            4     5:b1    4:4\n",
#'   "            5     6:b1    5:3\n",
#'   "///"
#' )
#' parse_kcf(kcf)
#'
#' @export
parse_kcf <- function(x, on_failure = "error", progress = FALSE) {
  struc_parser_wrapper(
    x,
    do_parse_kcf,
    on_failure = on_failure,
    progress = progress
  )
}

#' Parse one KCF string into an igraph glycan graph
#'
#' @param x A single KCF string.
#'
#' @return An igraph glycan graph.
#' @noRd
do_parse_kcf <- function(x) {
  record <- parse_kcf_record(x)
  build_kcf_graph(record$nodes, record$edges)
}

#' Check whether a string looks like KCF
#'
#' @param x A character scalar.
#'
#' @return A logical scalar.
#' @noRd
is_kcf_string <- function(x) {
  isTRUE(stringr::str_detect(
    x,
    stringr::regex("^ENTRY\\s+.+\\bGlycan\\b", multiline = TRUE)
  )) &&
    isTRUE(stringr::str_detect(
      x,
      stringr::regex("^NODE\\s+\\d+", multiline = TRUE)
    ))
}

#' Parse KCF NODE and EDGE sections
#'
#' @param x A single KCF string.
#'
#' @return A list with parsed `nodes` and `edges`.
#' @noRd
parse_kcf_record <- function(x) {
  lines <- split_kcf_lines(x)
  if (!any(stringr::str_detect(lines, "^NODE\\s+\\d+"))) {
    cli::cli_abort("No NODE section found in KCF string")
  }

  section <- NA_character_
  node_lines <- character(0)
  edge_lines <- character(0)

  for (line in lines) {
    if (identical(line, "///")) {
      break
    }
    header <- stringr::str_match(line, "^(NODE|EDGE)\\s+\\d+")
    if (!is.na(header[[1]])) {
      section <- header[[2]]
      next
    }
    if (identical(section, "NODE")) {
      node_lines <- c(node_lines, line)
    } else if (identical(section, "EDGE")) {
      edge_lines <- c(edge_lines, line)
    }
  }

  nodes <- purrr::map(node_lines, parse_kcf_node_line)
  nodes <- purrr::compact(nodes)
  names(nodes) <- purrr::map_chr(nodes, ~ as.character(.x$id))

  edges <- purrr::map(edge_lines, parse_kcf_edge_line)
  edges <- purrr::compact(edges)

  list(nodes = nodes, edges = edges)
}

#' Split a KCF record into parser lines
#'
#' @param x A KCF string.
#'
#' @return A character vector with trimmed non-empty lines.
#' @noRd
split_kcf_lines <- function(x) {
  lines <- stringr::str_split(x, "\n")[[1]]
  lines <- stringr::str_trim(lines)
  lines[lines != ""]
}

#' Parse one KCF NODE line
#'
#' @param line A KCF NODE line.
#'
#' @return A parsed node list, or `NULL` when the line is malformed.
#' @noRd
parse_kcf_node_line <- function(line) {
  parts <- stringr::str_split(line, "\\s+")[[1]]
  if (length(parts) < 2 || is.na(suppressWarnings(as.integer(parts[[1]])))) {
    return(NULL)
  }

  label <- parts[[2]]
  node <- list(
    id = as.integer(parts[[1]]),
    label = label,
    kind = "unknown",
    mono = NA_character_,
    sub = character(0)
  )

  mono <- parse_kcf_monosaccharide_label(label)
  if (!is.null(mono)) {
    node$kind <- "mono"
    node$mono <- mono$mono
    node$sub <- mono$sub
    return(node)
  }

  sub <- map_kcf_substituent_label(label)
  if (!is.na(sub)) {
    node$kind <- "sub"
    node$sub <- sub
    return(node)
  }

  if (is_kcf_aglycon_label(label) || identical(label, "*")) {
    node$kind <- "aglycon"
    return(node)
  }

  node
}

#' Parse one KCF EDGE line
#'
#' @param line A KCF EDGE line.
#'
#' @return A parsed edge list, or `NULL` when the line is malformed.
#' @noRd
parse_kcf_edge_line <- function(line) {
  parts <- stringr::str_split(line, "\\s+")[[1]]
  if (length(parts) < 3 || is.na(suppressWarnings(as.integer(parts[[1]])))) {
    return(NULL)
  }

  list(
    id = as.integer(parts[[1]]),
    left = parse_kcf_endpoint(parts[[2]]),
    right = parse_kcf_endpoint(parts[[3]])
  )
}

#' Parse a KCF edge endpoint
#'
#' @param endpoint A KCF edge endpoint such as `"2:b1"` or `"1:4"`.
#'
#' @return A list with node id, anomer, and linkage position.
#' @noRd
parse_kcf_endpoint <- function(endpoint) {
  parts <- stringr::str_split(endpoint, ":", n = 2)[[1]]
  link <- if (length(parts) == 2) parts[[2]] else NA_character_
  anomer <- stringr::str_extract(link, "^[abAB?]")
  position <- stringr::str_remove(link, "^[abAB?]")
  position <- stringr::str_replace_all(position, stringr::fixed("|"), "/")

  if (is.na(position) || identical(position, "")) {
    position <- NA_character_
  }

  list(
    id = as.integer(parts[[1]]),
    anomer = tolower(anomer),
    position = position
  )
}

#' Build a glycan graph from parsed KCF nodes and edges
#'
#' @param nodes A named list of parsed KCF nodes.
#' @param edges A list of parsed KCF edges.
#'
#' @return An igraph glycan graph.
#' @noRd
build_kcf_graph <- function(nodes, edges) {
  unknown_nodes <- nodes[purrr::map_chr(nodes, "kind") == "unknown"]
  if (length(unknown_nodes) > 0) {
    unknown_labels <- purrr::map_chr(unknown_nodes, "label")
    cli::cli_abort("Unsupported KCF node labels: {.val {unknown_labels}}")
  }

  mono_nodes <- nodes[purrr::map_chr(nodes, "kind") == "mono"]
  if (length(mono_nodes) == 0) {
    cli::cli_abort("No monosaccharides found in KCF string")
  }

  glycan_edges <- list()
  root_anomers <- list()
  extra_subs <- purrr::map(mono_nodes, ~ character(0))
  names(extra_subs) <- names(mono_nodes)

  for (edge in edges) {
    parsed <- classify_kcf_edge(edge, nodes)
    if (is.null(parsed)) {
      next
    }

    if (identical(parsed$type, "glycosidic")) {
      glycan_edges <- append(glycan_edges, list(parsed$linkage))
    } else if (identical(parsed$type, "root_anomer")) {
      root_anomers[[as.character(parsed$node_id)]] <- parsed$anomer
    } else if (identical(parsed$type, "substituent")) {
      parent_id <- as.character(parsed$parent_id)
      if (!is.null(extra_subs[[parent_id]])) {
        extra_subs[[parent_id]] <- c(extra_subs[[parent_id]], parsed$sub)
      }
    }
  }

  vertices <- purrr::imap(mono_nodes, function(node, id) {
    list(
      original_id = node$id,
      mono = node$mono,
      sub = format_kcf_substituents(c(node$sub, extra_subs[[id]]))
    )
  })

  graph <- make_kcf_igraph(vertices, glycan_edges)
  reducing_end <- find_reducing_end(vertices, glycan_edges)
  root_id <- as.character(reducing_end$original_id)
  graph$anomer <- root_anomers[[root_id]]
  if (is.null(graph$anomer)) {
    graph$anomer <- paste0("?", decide_anomer_pos(reducing_end$mono))
  }

  graph
}

#' Classify one parsed KCF edge
#'
#' @param edge A parsed KCF edge.
#' @param nodes A named list of parsed KCF nodes.
#'
#' @return A classified edge list, or `NULL` when the edge is not representable.
#' @noRd
classify_kcf_edge <- function(edge, nodes) {
  left_node <- nodes[[as.character(edge$left$id)]]
  right_node <- nodes[[as.character(edge$right$id)]]
  if (is.null(left_node) || is.null(right_node)) {
    return(NULL)
  }

  left_kind <- left_node$kind
  right_kind <- right_node$kind

  if (identical(left_kind, "mono") && identical(right_kind, "mono")) {
    return(list(
      type = "glycosidic",
      linkage = list(
        from_res = edge$right$id,
        from_pos = normalize_kcf_position(edge$right$position),
        to_res = edge$left$id,
        to_pos = normalize_kcf_position(edge$left$position),
        anomer = normalize_kcf_anomer(edge$left$anomer)
      )
    ))
  }

  if (identical(left_kind, "mono") && identical(right_kind, "aglycon")) {
    return(list(
      type = "root_anomer",
      node_id = edge$left$id,
      anomer = format_kcf_anomer(
        edge$left$anomer,
        edge$left$position,
        left_node$mono
      )
    ))
  }

  if (identical(left_kind, "aglycon") && identical(right_kind, "mono")) {
    return(list(
      type = "root_anomer",
      node_id = edge$right$id,
      anomer = format_kcf_anomer(
        edge$right$anomer,
        edge$right$position,
        right_node$mono
      )
    ))
  }

  if (identical(left_kind, "sub") && identical(right_kind, "mono")) {
    return(list(
      type = "substituent",
      parent_id = edge$right$id,
      sub = format_kcf_substituent(
        edge$right$position,
        left_node$sub
      )
    ))
  }

  if (identical(left_kind, "mono") && identical(right_kind, "sub")) {
    return(list(
      type = "substituent",
      parent_id = edge$left$id,
      sub = format_kcf_substituent(
        edge$left$position,
        right_node$sub
      )
    ))
  }

  NULL
}

#' Build the final igraph object for KCF parsing
#'
#' @param vertices A list of parsed mono vertices.
#' @param glycan_edges A list of parsed glycosidic edges.
#'
#' @return An igraph glycan graph without graph-level anomer set.
#' @noRd
make_kcf_igraph <- function(vertices, glycan_edges) {
  vertex_names <- seq_along(vertices)
  edge_indices <- integer(0)
  linkage_values <- character(0)

  for (edge in glycan_edges) {
    from_idx <- which(purrr::map_int(vertices, "original_id") == edge$from_res)
    to_idx <- which(purrr::map_int(vertices, "original_id") == edge$to_res)

    if (length(from_idx) == 1 && length(to_idx) == 1) {
      edge_indices <- c(edge_indices, from_idx, to_idx)
      linkage_values <- c(linkage_values, format_kcf_linkage(edge))
    }
  }

  if (length(edge_indices) == 0) {
    graph <- igraph::make_empty_graph(n = length(vertices), directed = TRUE)
    graph <- igraph::set_edge_attr(graph, "linkage", value = character(0))
  } else {
    graph <- igraph::make_graph(
      edge_indices,
      n = length(vertices),
      directed = TRUE
    )
    igraph::E(graph)$linkage <- linkage_values
  }

  igraph::V(graph)$name <- as.character(vertex_names)
  igraph::V(graph)$mono <- purrr::map_chr(vertices, "mono")
  igraph::V(graph)$sub <- purrr::map_chr(vertices, "sub")

  graph
}

#' Parse a KCF monosaccharide label
#'
#' @param label A KCF node label.
#'
#' @return A list with `mono` and `sub`, or `NULL` when unsupported.
#' @noRd
parse_kcf_monosaccharide_label <- function(label) {
  candidates <- label
  if (stringr::str_detect(label, "^[DL].+")) {
    candidates <- c(candidates, stringr::str_sub(label, 2))
  }

  for (candidate in candidates) {
    parsed <- parse_kcf_monosaccharide_candidate(candidate)
    if (!is.null(parsed)) {
      return(parsed)
    }
  }

  NULL
}

#' Parse a KCF monosaccharide label candidate
#'
#' @param label A KCF node label, after optional D/L prefix removal.
#'
#' @return A list with `mono` and `sub`, or `NULL` when unsupported.
#' @noRd
parse_kcf_monosaccharide_candidate <- function(label) {
  mono_names <- glyrepr::available_monosaccharides()
  if (label %in% mono_names) {
    return(list(mono = label, sub = character(0)))
  }

  mono_prefix <- find_kcf_mono_prefix(label, mono_names)
  if (is.na(mono_prefix)) {
    return(NULL)
  }

  suffix <- stringr::str_sub(label, nchar(mono_prefix) + 1)
  sub <- parse_kcf_label_substituents(suffix)
  if (is.null(sub)) {
    return(NULL)
  }

  list(mono = mono_prefix, sub = sub)
}

#' Find the longest monosaccharide prefix in a KCF label
#'
#' @param label A KCF node label.
#' @param mono_names Supported monosaccharide names.
#'
#' @return A monosaccharide name, or `NA`.
#' @noRd
find_kcf_mono_prefix <- function(label, mono_names) {
  mono_names <- mono_names[order(nchar(mono_names), decreasing = TRUE)]
  matches <- mono_names[stringr::str_starts(label, stringr::fixed(mono_names))]
  if (length(matches) == 0) {
    return(NA_character_)
  }

  matches[[1]]
}

#' Parse substituents embedded in a KCF node label
#'
#' @param suffix The part of a node label after the monosaccharide name.
#'
#' @return A character vector of formatted substituents, or `NULL` if invalid.
#' @noRd
parse_kcf_label_substituents <- function(suffix) {
  if (identical(suffix, "")) {
    return(character(0))
  }

  remaining <- suffix
  substituents <- character(0)
  pattern <- "^(\\d+|\\?)(Ac|Me|Pyr|Py|S|P|N)"

  while (!identical(remaining, "")) {
    match <- stringr::str_match(remaining, pattern)
    if (is.na(match[[1]])) {
      return(NULL)
    }

    sub_name <- if (identical(match[[3]], "Py")) "Pyr" else match[[3]]
    substituents <- c(substituents, paste0(match[[2]], sub_name))
    remaining <- stringr::str_sub(remaining, nchar(match[[1]]) + 1)
  }

  substituents
}

#' Check whether a KCF node label is an aglycon
#'
#' @param label A KCF node label.
#'
#' @return A logical scalar.
#' @noRd
is_kcf_aglycon_label <- function(label) {
  label %in%
    c(
      "LipidA",
      "R",
      "Asn",
      "Ser/Thr",
      "Ser",
      "P-Dol",
      "PP-Dol",
      "PP-Und",
      "PE",
      "Cer",
      "myo-Ino",
      "Ino(acyl)-P",
      "Ino-P",
      "Ino",
      "Sph"
    )
}

#' Map a KCF substituent node label
#'
#' @param label A KCF node label.
#'
#' @return A glyrepr substituent name, or `NA`.
#' @noRd
map_kcf_substituent_label <- function(label) {
  substituent_map <- c(
    "S" = "S",
    "P" = "P",
    "Ac" = "Ac",
    "Me" = "Me",
    "N" = "N"
  )

  unname(substituent_map[label])
}

#' Format a KCF glycosidic linkage
#'
#' @param edge A parsed glycosidic edge.
#'
#' @return A linkage string such as `"b1-4"`.
#' @noRd
format_kcf_linkage <- function(edge) {
  paste0(
    normalize_kcf_anomer(edge$anomer),
    normalize_kcf_position(edge$to_pos),
    "-",
    normalize_kcf_position(edge$from_pos)
  )
}

#' Format a reducing-end anomer from KCF edge data
#'
#' @param anomer An anomer symbol.
#' @param position An anomer position.
#' @param mono A monosaccharide name.
#'
#' @return A graph-level anomer string.
#' @noRd
format_kcf_anomer <- function(anomer, position, mono) {
  paste0(
    normalize_kcf_anomer(anomer),
    normalize_kcf_position(position, default = decide_anomer_pos(mono))
  )
}

#' Normalize a KCF anomer value
#'
#' @param anomer An anomer value.
#'
#' @return `"a"`, `"b"`, or `"?"`.
#' @noRd
normalize_kcf_anomer <- function(anomer) {
  if (is.na(anomer) || !anomer %in% c("a", "b")) {
    return("?")
  }

  anomer
}

#' Normalize a KCF linkage position
#'
#' @param position A linkage position.
#' @param default The fallback value for missing positions.
#'
#' @return A position string.
#' @noRd
normalize_kcf_position <- function(position, default = "?") {
  if (is.null(position) || is.na(position) || identical(position, "")) {
    return(default)
  }

  position
}

#' Format a KCF substituent with its parent position
#'
#' @param position The parent monosaccharide position.
#' @param sub The substituent name.
#'
#' @return A glyrepr substituent token.
#' @noRd
format_kcf_substituent <- function(position, sub) {
  paste0(normalize_kcf_position(position), sub)
}

#' Format multiple KCF substituent tokens
#'
#' @param sub A character vector of substituent tokens.
#'
#' @return A comma-separated substituent string for a graph vertex.
#' @noRd
format_kcf_substituents <- function(sub) {
  sub <- unique(sub[sub != "" & !is.na(sub)])
  if (length(sub) == 0) {
    return("")
  }

  paste(sort_kcf_substituents(sub), collapse = ",")
}

#' Sort KCF substituent tokens by position and name
#'
#' @param sub A character vector of substituent tokens.
#'
#' @return A sorted character vector.
#' @noRd
sort_kcf_substituents <- function(sub) {
  positions <- stringr::str_extract(sub, "^(?:\\d+|\\?)")
  position_order <- suppressWarnings(as.integer(positions))
  position_order[is.na(position_order)] <- Inf
  sub_names <- stringr::str_remove(sub, "^(?:\\d+|\\?)")

  sub[order(position_order, sub_names, sub)]
}
