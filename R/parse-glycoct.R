#' Parse GlycoCT Structures
#'
#' This function parses GlycoCT strings into a [glyrepr::glycan_structure()].
#' GlycoCT is a format used by databases like GlyTouCan and GlyGen.
#'
#' @details
#' GlycoCT format consists of:
#' - RES: Contains monosaccharides (lines starting with 'b:') and substituents (lines starting with 's:')
#' - LIN: Contains linkage information between residues
#' - UND: Contains floating substructures or substituents whose attachment to
#'   the main glycan is unresolved
#'
#' Main reducing-end alditol residues retain their alditol status and use an
#' unknown anomer configuration.
#'
#' For more information about GlycoCT format, see the glycoct.md documentation.
#'
#' @param x A character vector of GlycoCT strings. NA values are allowed and will be returned as NA structures.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param progress Whether to show a progress bar while parsing.
#' @param validate Whether to validate parsed glycan graphs before constructing
#'   the result.
#' @param drop_generic Whether to replace parsed generic glycans with `NA`. A
#'   message reports the number replaced. By default, mixing generic and
#'   concrete glycans raises an error.
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' glycoct <- paste0(
#'   "RES\n",
#'   "1b:a-dgal-HEX-1:5\n",
#'   "2s:n-acetyl\n",
#'   "3b:b-dgal-HEX-1:5\n",
#'   "LIN\n",
#'   "1:1d(2+1)2n\n",
#'   "2:1o(3+1)3d"
#' )
#' parse_glycoct(glycoct)
#'
#' @export
parse_glycoct <- function(
  x,
  on_failure = "error",
  progress = FALSE,
  validate = TRUE,
  drop_generic = FALSE
) {
  on_failure <- validate_struc_parser_wrapper_args(
    x,
    on_failure,
    progress,
    validate,
    drop_generic,
    call = rlang::caller_env()
  )
  struc_parser_wrapper(
    x,
    do_parse_glycoct,
    on_failure = on_failure,
    progress = progress,
    validate = validate,
    drop_generic = drop_generic
  )
}

do_parse_glycoct <- function(x) {
  lines <- split_glycoct_lines(x)
  blocks <- split_glycoct_blocks(lines)

  main <- parse_glycoct_block(blocks$main)
  floating <- purrr::map(blocks$floating, parse_glycoct_und_block)
  has_floating_alditol <- any(purrr::map_lgl(
    floating,
    ~ has_glycoct_alditol_residue(.x$residues)
  ))
  if (has_floating_alditol) {
    warn_glycoct_floating_alditol()
  }
  main_graph <- build_glycoct_graph_data(main$residues, main$linkages)

  if (length(blocks$floating) == 0) {
    return(main_graph$graph)
  }

  floating_substituents <- purrr::keep(
    floating,
    is_glycoct_floating_substituent
  )
  floating <- purrr::discard(
    floating,
    is_glycoct_floating_substituent
  )
  floating_graphs <- purrr::map(
    floating,
    ~ build_glycoct_graph_data(.x$residues, .x$linkages)
  )

  build_glycoct_floating_graph(
    main_graph,
    floating_graphs,
    floating,
    floating_substituents
  )
}

#' Split a GlycoCT record into its main and UND blocks
#'
#' @param lines Tokenized GlycoCT lines.
#'
#' @return A list containing the main block and zero or more floating blocks.
#' @noRd
split_glycoct_blocks <- function(lines) {
  und_marker <- which(lines == "UND")
  if (length(und_marker) == 0) {
    return(list(main = lines, floating = list()))
  }

  first_und <- und_marker[[1]]
  main <- lines[seq_len(first_und - 1L)]
  und_lines <- lines[(first_und + 1L):length(lines)]
  starts <- which(stringr::str_detect(und_lines, "^UND\\d+:"))
  if (length(starts) == 0) {
    cli::cli_abort("No UND block found after the GlycoCT UND section")
  }

  ends <- c(starts[-1] - 1L, length(und_lines))
  floating <- purrr::map2(
    starts,
    ends,
    ~ und_lines[.x:.y]
  )

  list(main = main, floating = floating)
}

#' Parse the structural sections of one GlycoCT block
#'
#' @param lines Lines containing one RES section and an optional LIN section.
#'
#' @return Parsed residues and linkages.
#' @noRd
parse_glycoct_block <- function(lines) {
  res_start <- which(lines == "RES")
  lin_start <- which(lines == "LIN")

  if (length(res_start) != 1) {
    cli::cli_abort("A GlycoCT block must contain exactly one RES section")
  }
  if (length(lin_start) > 1) {
    cli::cli_abort("A GlycoCT block can contain at most one LIN section")
  }

  res_end <- if (length(lin_start) == 0) {
    length(lines)
  } else {
    lin_start - 1L
  }
  res_lines <- lines[(res_start + 1L):res_end]
  lin_lines <- if (length(lin_start) == 0) {
    character()
  } else {
    lines[(lin_start + 1L):length(lines)]
  }

  list(
    residues = parse_res_section(res_lines),
    linkages = parse_lin_section(lin_lines)
  )
}

#' Parse a GlycoCT UND block
#'
#' @param lines One UND block, beginning with its UND identifier.
#'
#' @return Parsed structural data and virtual attachment metadata.
#' @noRd
parse_glycoct_und_block <- function(lines) {
  parent_line <- lines[stringr::str_detect(lines, "^ParentIDs:")]
  linkage_line <- lines[
    stringr::str_detect(lines, "^SubtreeLinkageID\\d+:")
  ]
  if (length(parent_line) != 1 || length(linkage_line) != 1) {
    cli::cli_abort(
      "A GlycoCT UND block must contain one ParentIDs and one SubtreeLinkageID field"
    )
  }

  parent_text <- stringr::str_remove(parent_line, "^ParentIDs:")
  parent_ids <- as.integer(stringr::str_split(parent_text, "\\|")[[1]])
  if (length(parent_ids) == 0 || anyNA(parent_ids)) {
    cli::cli_abort("GlycoCT UND ParentIDs must contain residue identifiers")
  }

  attachment <- stringr::str_remove(
    linkage_line,
    "^SubtreeLinkageID\\d+:"
  )
  matches <- stringr::str_match(
    attachment,
    "^[a-z]?\\((-?\\d+(?:\\|\\d+)*)\\+(-?\\d+(?:\\|\\d+)*)\\)[a-z]?$"
  )
  if (is.na(matches[1])) {
    cli::cli_abort(
      "Can't parse GlycoCT UND subtree linkage {.val {attachment}}"
    )
  }
  if (stringr::str_detect(matches[3], stringr::fixed("|"))) {
    cli::cli_abort(
      "GlycoCT UND linkages with alternative donor positions are not supported: {.val {attachment}}"
    )
  }

  block <- parse_glycoct_block(lines)
  c(
    block,
    list(
      parent_ids = parent_ids,
      parent_pos = matches[2],
      child_pos = matches[3]
    )
  )
}

#' Detect a substituent-only GlycoCT UND block
#'
#' @param block Parsed UND block data.
#'
#' @return A logical scalar.
#' @noRd
is_glycoct_floating_substituent <- function(block) {
  length(block$residues) == 1L &&
    identical(block$residues[[1]]$type, "sub")
}

#' Split a GlycoCT record into parser lines
#'
#' @param x A GlycoCT string.
#'
#' @return A character vector with section headers, residue lines, and linkage
#'   lines as separate entries.
#' @noRd
split_glycoct_lines <- function(x) {
  lines <- stringr::str_split(x, "\n")[[1]]
  lines <- stringr::str_trim(lines)
  lines <- lines[lines != ""]

  if (
    length(lines) == 1 &&
      isTRUE(stringr::str_detect(lines, "^RES\\s+"))
  ) {
    return(stringr::str_split(lines, "\\s+")[[1]])
  }

  lines
}

parse_res_section <- function(res_lines) {
  residues <- list()

  for (line in res_lines) {
    # Parse line format: "1b:a-dgal-HEX-1:5"
    parts <- stringr::str_split(line, ":")[[1]]
    if (length(parts) < 2) {
      next
    }

    id <- as.integer(stringr::str_extract(parts[1], "\\d+"))
    type <- stringr::str_extract(parts[1], "[bs]$")
    # Rejoin content in case there are multiple colons
    content <- paste(parts[2:length(parts)], collapse = ":")

    if (type == "b") {
      # Monosaccharide
      anomer <- stringr::str_extract(content, "^[abxo]")
      mono_info <- stringr::str_remove(content, "^[abxo]-")

      residues[[as.character(id)]] <- list(
        type = "mono",
        anomer = anomer,
        anomer_pos = extract_glycoct_anomer_pos(mono_info),
        content = mono_info,
        is_alditol = is_glycoct_alditol_mono(mono_info),
        substituents = list()
      )
    } else if (type == "s") {
      # Substituent
      residues[[as.character(id)]] <- list(
        type = "sub",
        content = content
      )
    }
  }

  residues
}

#' Check whether a GlycoCT monosaccharide descriptor is an alditol
#'
#' @param content GlycoCT monosaccharide content without the leading anomer.
#'
#' @return A logical scalar.
#' @noRd
is_glycoct_alditol_mono <- function(content) {
  isTRUE(stringr::str_detect(content, "\\|1:aldi"))
}

#' Check whether parsed GlycoCT residues contain an alditol
#'
#' @param residues A parsed GlycoCT residue list.
#'
#' @return A logical scalar.
#' @noRd
has_glycoct_alditol_residue <- function(residues) {
  any(purrr::map_lgl(
    residues,
    ~ identical(.x$type, "mono") && isTRUE(.x$is_alditol)
  ))
}

#' Warn about floating alditol normalization in GlycoCT parsing
#'
#' @return `NULL`, invisibly.
#' @noRd
warn_glycoct_floating_alditol <- function() {
  cli::cli_warn(c(
    "Only the main reducing-end GlycoCT residue can retain alditol status.",
    "i" = "Alditol residues in UND sections are parsed as regular residues."
  ))
  invisible(NULL)
}

#' Extract the anomeric position from a GlycoCT monosaccharide descriptor
#'
#' @param content GlycoCT monosaccharide content without the leading anomer.
#'
#' @return A character scalar with the anomeric position, or `NA`.
#' @noRd
extract_glycoct_anomer_pos <- function(content) {
  stringr::str_extract(content, "-(\\d+|x):", group = 1)
}

#' Extract the ring-bounds component from a GlycoCT monosaccharide descriptor
#'
#' @param content GlycoCT monosaccharide content without the leading anomer.
#'
#' @return A character scalar with the ring bounds, or `NA`.
#' @noRd
extract_glycoct_ring_bounds <- function(content) {
  stringr::str_extract(content, "-((?:\\d+|x):(?:\\d+|x))", group = 1)
}

#' Parse numeric GlycoCT ring bounds
#'
#' @param bounds A GlycoCT ring-bounds string.
#'
#' @return An integer vector of length two, or two missing integers.
#' @noRd
parse_glycoct_ring_bounds <- function(bounds) {
  if (is.na(bounds) || stringr::str_detect(bounds, "x")) {
    return(c(NA_integer_, NA_integer_))
  }
  as.integer(stringr::str_split_1(bounds, stringr::fixed(":")))
}

#' Check whether GlycoCT ring bounds describe a supported cyclic form
#'
#' @param bounds A GlycoCT ring-bounds string.
#'
#' @return A logical scalar.
#' @noRd
is_supported_glycoct_ring_bounds <- function(bounds) {
  parsed <- parse_glycoct_ring_bounds(bounds)
  !anyNA(parsed) && parsed[[2]] - parsed[[1]] %in% c(3L, 4L)
}

#' Check whether GlycoCT content describes a furanose ring
#'
#' @param content GlycoCT monosaccharide content without the leading anomer.
#'
#' @return A logical scalar.
#' @noRd
is_glycoct_furanose <- function(content) {
  bounds <- parse_glycoct_ring_bounds(extract_glycoct_ring_bounds(content))
  !anyNA(bounds) && identical(bounds[[2]] - bounds[[1]], 3L)
}

#' Preserve a GlycoCT furanose ring in a mapped monosaccharide
#'
#' @param mono A glyrepr monosaccharide name.
#' @param content GlycoCT monosaccharide content without the leading anomer.
#'
#' @return A glyrepr monosaccharide name.
#' @noRd
preserve_glycoct_ring <- function(mono, content) {
  if (is_glycoct_furanose(content)) {
    return(as_furanose_monosaccharide(mono))
  }
  mono
}

#' Remove the ring-bounds component from a GlycoCT monosaccharide descriptor
#'
#' @param content GlycoCT monosaccharide content without the leading anomer.
#'
#' @return A normalized descriptor used for monosaccharide matching.
#' @noRd
remove_glycoct_ring_bounds <- function(content) {
  stringr::str_remove(content, "-(?:\\d+|x):(?:\\d+|x)")
}

#' Check whether two GlycoCT monosaccharide descriptors are compatible
#'
#' @param x,y GlycoCT monosaccharide content without the leading anomer.
#'
#' @return A logical scalar.
#' @noRd
glycoct_mono_content_matches <- function(x, y) {
  x_alditol <- is_glycoct_alditol_mono(x)
  y_alditol <- is_glycoct_alditol_mono(y)
  if (!identical(x_alditol, y_alditol)) {
    return(FALSE)
  }

  same_core <- remove_glycoct_ring_bounds(x) == remove_glycoct_ring_bounds(y)
  if (!same_core) {
    return(FALSE)
  }

  x_bounds <- extract_glycoct_ring_bounds(x)
  y_bounds <- extract_glycoct_ring_bounds(y)
  if (is.na(x_bounds) || is.na(y_bounds)) {
    return(identical(x_bounds, y_bounds))
  }

  identical(x_bounds, y_bounds) ||
    stringr::str_detect(x_bounds, "x") ||
    stringr::str_detect(y_bounds, "x")
}

parse_lin_section <- function(lin_lines) {
  linkages <- list()

  for (line in lin_lines) {
    # Parse line format: "1:1d(2+1)2n"
    parts <- stringr::str_split(line, ":")[[1]]
    if (length(parts) < 2) {
      next
    }

    link_id <- as.integer(parts[1])
    link_info <- parts[2]

    # Extract components: from_res, positions, to_res
    # Updated pattern to handle negative positions like -1
    pattern <- "(\\d+)([do]?)\\((-?\\d+(?:\\|\\d+)*)\\+(-?\\d+(?:\\|\\d+)*)\\)(\\d+)([dn]?)"
    matches <- stringr::str_match(link_info, pattern)

    if (!is.na(matches[1])) {
      from_res <- as.integer(matches[2])
      from_pos <- matches[4]
      to_res <- as.integer(matches[6])
      to_pos <- matches[5]

      linkages[[as.character(link_id)]] <- list(
        from_res = from_res,
        from_pos = from_pos,
        to_res = to_res,
        to_pos = to_pos
      )
    }
  }

  linkages
}

build_glycoct_graph <- function(residues, linkages) {
  build_glycoct_graph_data(residues, linkages)$graph
}

build_glycoct_graph_data <- function(residues, linkages) {
  mono_mapping_index <- glycoct_mapping_index()

  # Consolidate monosaccharides with their substituents
  consolidated <- consolidate_residues(
    residues,
    linkages,
    mono_mapping_index
  )

  # Build igraph
  if (length(consolidated$vertices) == 0) {
    cli::cli_abort("No monosaccharides found in GlycoCT string")
  }

  # Create vertex names
  vertex_names <- seq_along(consolidated$vertices)

  # Create edges
  edges <- c()
  edge_attrs <- list(linkage = character(0))

  for (edge in consolidated$edges) {
    from_idx <- which(sapply(consolidated$vertices, function(v) {
      v$original_id == edge$from_res
    }))
    to_idx <- which(sapply(consolidated$vertices, function(v) {
      v$original_id == edge$to_res
    }))

    if (length(from_idx) == 1 && length(to_idx) == 1) {
      edges <- c(edges, from_idx, to_idx)

      to_vertex <- consolidated$vertices[[to_idx]]
      edge_attrs$linkage <- c(
        edge_attrs$linkage,
        format_glycoct_linkage(
          edge$from_pos,
          edge$to_pos,
          to_vertex$anomer
        )
      )
    }
  }

  # Create igraph
  if (length(edges) == 0) {
    # Single monosaccharide
    g <- igraph::make_empty_graph(
      n = length(consolidated$vertices),
      directed = TRUE
    )
    # Add empty linkage attribute for validation
    g <- igraph::set_edge_attr(g, "linkage", value = character(0))
  } else {
    g <- igraph::make_graph(
      edges,
      n = length(consolidated$vertices),
      directed = TRUE
    )
    igraph::E(g)$linkage <- edge_attrs$linkage
  }

  # Set vertex attributes
  igraph::V(g)$name <- as.character(vertex_names)
  igraph::V(g)$mono <- sapply(consolidated$vertices, function(v) v$mono)
  igraph::V(g)$sub <- sapply(consolidated$vertices, function(v) v$sub)

  # Set graph attributes (reducing end properties)
  reducing_end <- find_reducing_end(consolidated$vertices, consolidated$edges)
  if (!is.null(reducing_end)) {
    g$anomer <- format_glycoct_reducing_anomer(reducing_end)
  } else {
    g$anomer <- "?1"
  }
  g$alditol <- !is.null(reducing_end) && isTRUE(reducing_end$is_alditol)

  list(
    graph = g,
    original_ids = purrr::map_int(
      consolidated$vertices,
      "original_id"
    ),
    vertices = consolidated$vertices
  )
}

#' Format a GlycoCT linkage as IUPAC-condensed linkage text
#'
#' @param parent_pos Acceptor position on the parent residue.
#' @param child_pos Donor position on the child residue.
#' @param child_anomer Anomer configuration of the child residue.
#'
#' @return A linkage string such as `"a2-3"` or `"?1-?"`.
#' @noRd
format_glycoct_linkage <- function(parent_pos, child_pos, child_anomer) {
  anomer <- if (child_anomer %in% c("x", "o")) "?" else child_anomer
  donor <- if (child_pos == "-1") "?" else child_pos
  acceptor <- if (parent_pos == "-1") "?" else parent_pos
  acceptor <- stringr::str_replace_all(
    acceptor,
    stringr::fixed("|"),
    stringr::fixed("/")
  )

  paste0(anomer, donor, "-", acceptor)
}

#' Combine a main GlycoCT tree with parsed UND subtree graphs
#'
#' @param main Main-tree graph data.
#' @param floating_graphs Graph data for each floating subtree.
#' @param floating Parsed UND metadata.
#' @param floating_substituents Parsed substituent-only UND metadata.
#'
#' @return An annotated glycan forest understood by `glyrepr`.
#' @noRd
build_glycoct_floating_graph <- function(
  main,
  floating_graphs,
  floating,
  floating_substituents = list()
) {
  graph_data <- c(list(main), floating_graphs)
  sizes <- purrr::map_int(graph_data, ~ igraph::vcount(.x$graph))
  offsets <- c(0L, utils::head(cumsum(sizes), -1L))

  forest <- igraph::make_empty_graph(sum(sizes), directed = TRUE)
  edge_endpoints <- purrr::map2(
    graph_data,
    offsets,
    function(data, offset) {
      endpoints <- igraph::as_edgelist(data$graph, names = FALSE)
      if (length(endpoints) == 0) {
        return(integer())
      }
      as.integer(t(endpoints + offset))
    }
  )
  edge_endpoints <- unlist(edge_endpoints, use.names = FALSE)
  edge_linkages <- unlist(
    purrr::map(
      graph_data,
      ~ igraph::edge_attr(.x$graph, "linkage")
    ),
    use.names = FALSE
  )
  if (length(edge_endpoints) > 0) {
    forest <- igraph::add_edges(
      forest,
      edge_endpoints,
      linkage = edge_linkages
    )
  } else {
    forest <- igraph::set_edge_attr(
      forest,
      "linkage",
      value = character()
    )
  }

  igraph::V(forest)$name <- as.character(seq_len(sum(sizes)))
  igraph::V(forest)$mono <- unlist(
    purrr::map(graph_data, ~ igraph::V(.x$graph)$mono),
    use.names = FALSE
  )
  igraph::V(forest)$sub <- unlist(
    purrr::map(graph_data, ~ igraph::V(.x$graph)$sub),
    use.names = FALSE
  )
  forest$anomer <- main$graph$anomer
  forest$alditol <- isTRUE(main$graph$alditol)

  main_parent_indices <- stats::setNames(
    seq_along(main$original_ids),
    main$original_ids
  )
  occupied_slots <- definitely_occupied_acceptor_slots(
    main$graph,
    seq_along(main$original_ids)
  )
  occupied_carbon_slots <- definitely_occupied_carbon_slots(
    main$graph,
    seq_along(main$original_ids)
  )
  if (length(floating_graphs) > 0L) {
    forest$floating_parts <- purrr::map2(
      seq_along(floating_graphs),
      floating,
      function(part_id, metadata) {
        data <- floating_graphs[[part_id]]
        offset <- offsets[[part_id + 1L]]
        root <- which(igraph::degree(data$graph, mode = "in") == 0)
        parents <- map_glycoct_und_parents(
          metadata$parent_ids,
          main_parent_indices
        )

        linkage <- format_glycoct_linkage(
          metadata$parent_pos,
          metadata$child_pos,
          data$vertices[[root]]$anomer
        )
        parents <- normalize_floating_part_parents(
          parents,
          seq_along(main$original_ids),
          linkage,
          occupied_slots,
          context = "GlycoCT UND part"
        )

        list(
          root = as.integer(offset + root),
          nodes = as.integer(offset + seq_len(sizes[[part_id + 1L]])),
          linkage = linkage,
          parents = as.integer(parents)
        )
      }
    )
  }

  if (length(floating_substituents) > 0L) {
    main_vertices <- seq_along(main$original_ids)
    forest$floating_substituents <- purrr::map(
      floating_substituents,
      function(metadata) {
        parents <- map_glycoct_und_parents(
          metadata$parent_ids,
          main_parent_indices
        )
        positions <- stringr::str_split_1(
          metadata$parent_pos,
          stringr::fixed("|")
        )
        position <- collapse_floating_substituent_positions(positions)
        substituent <- metadata$residues[[1]]$content
        substituent <- paste0(
          position,
          glycoct_substituent_abbreviation(substituent)
        )
        domain <- normalize_floating_substituent_parents(
          parents,
          main_vertices,
          substituent,
          occupied_carbon_slots,
          context = "GlycoCT UND substituent"
        )

        list(
          substituent = domain$substituent,
          parents = as.integer(domain$parents)
        )
      }
    )
  }

  forest
}

#' Map GlycoCT UND parent IDs to main-tree graph indices
#'
#' @param parent_ids GlycoCT residue IDs declared by an UND block.
#' @param main_parent_indices Named main-tree graph indices.
#'
#' @return Integer main-tree graph indices.
#' @noRd
map_glycoct_und_parents <- function(parent_ids, main_parent_indices) {
  parents <- unname(main_parent_indices[as.character(parent_ids)])
  if (anyNA(parents)) {
    missing_ids <- parent_ids[is.na(parents)]
    cli::cli_abort(c(
      "GlycoCT UND candidate parents must be main-tree monosaccharides.",
      "x" = "Can't find main-tree residue ID{?s} {.val {missing_ids}}."
    ))
  }
  as.integer(parents)
}

#' Find definitely occupied acceptor slots in a glycan tree
#'
#' @param graph A parsed glycan graph.
#' @param vertices Vertices belonging to the tree of interest.
#'
#' @return Character keys combining parent vertex and acceptor position.
#' @noRd
definitely_occupied_acceptor_slots <- function(
  graph,
  vertices = seq_len(igraph::vcount(graph))
) {
  if (igraph::ecount(graph) == 0) {
    return(character())
  }

  endpoints <- igraph::as_edgelist(graph, names = FALSE)
  linkages <- igraph::edge_attr(graph, "linkage")
  positions <- purrr::map(linkages, linkage_acceptor_positions)
  definite <- lengths(positions) == 1L &
    endpoints[, 1] %in% vertices &
    endpoints[, 2] %in% vertices

  paste(
    endpoints[definite, 1],
    unlist(positions[definite], use.names = FALSE),
    sep = "\r"
  )
}

#' Extract possible acceptor positions from an IUPAC linkage
#'
#' @param linkage An IUPAC-condensed linkage.
#'
#' @return A character vector. Unknown positions return an empty vector.
#' @noRd
linkage_acceptor_positions <- function(linkage) {
  acceptor <- stringr::str_extract(linkage, "(?<=-).*$")
  if (is.na(acceptor) || acceptor == "?") {
    return(character())
  }

  stringr::str_split(acceptor, stringr::fixed("/"))[[1]]
}

#' Find definitely occupied carbon slots in a glycan tree
#'
#' @param graph A parsed glycan graph.
#' @param vertices Vertices belonging to the tree of interest.
#'
#' @return Character keys combining parent vertex and carbon position.
#' @noRd
definitely_occupied_carbon_slots <- function(
  graph,
  vertices = seq_len(igraph::vcount(graph))
) {
  edge_slots <- definitely_occupied_acceptor_slots(graph, vertices)
  substituent_slots <- purrr::map(
    vertices,
    function(vertex) {
      substituents <- igraph::vertex_attr(graph, "sub", index = vertex)
      if (identical(substituents, "")) {
        return(character())
      }
      tokens <- stringr::str_split_1(
        substituents,
        stringr::fixed(",")
      )
      positions <- purrr::map(
        tokens,
        floating_substituent_positions
      )
      definite <- lengths(positions) == 1L
      paste(
        vertex,
        unlist(positions[definite], use.names = FALSE),
        sep = "\r"
      )
    }
  )

  unique(c(
    edge_slots,
    unlist(substituent_slots, use.names = FALSE)
  ))
}

#' Extract possible carbon positions from a substituent token
#'
#' @param substituent One substituent token.
#'
#' @return A character vector. Unknown positions return an empty vector.
#' @noRd
floating_substituent_positions <- function(substituent) {
  position <- stringr::str_extract(substituent, "^[?0-9/]+")
  if (
    is.na(position) ||
      stringr::str_detect(position, stringr::fixed("?"))
  ) {
    return(character())
  }
  stringr::str_split_1(position, stringr::fixed("/"))
}

#' Collapse source-format substituent positions into one glyrepr position
#'
#' @param positions Source-format carbon positions.
#'
#' @return A canonical position string.
#' @noRd
collapse_floating_substituent_positions <- function(positions) {
  if (any(positions %in% c("?", "-1"))) {
    return("?")
  }
  paste(sort(unique(as.integer(positions))), collapse = "/")
}

#' Normalize floating-substituent candidate parents
#'
#' @param parents Declared main-tree candidate vertex indices.
#' @param main_vertices Every main-tree vertex index.
#' @param substituent A floating substituent token.
#' @param occupied_slots Definitely occupied main-tree carbon slots.
#' @param context Input-format context for error messages.
#'
#' @return A list containing the normalized substituent and candidate parents.
#'   An empty parent vector represents an implicit all-main candidate set.
#' @noRd
normalize_floating_substituent_parents <- function(
  parents,
  main_vertices,
  substituent,
  occupied_slots,
  context
) {
  positions <- floating_substituent_positions(substituent)
  if (length(positions) == 0L) {
    if (setequal(parents, main_vertices)) {
      parents <- integer()
    }
    return(list(substituent = substituent, parents = parents))
  }

  feasible_positions <- purrr::map(parents, function(parent) {
    slots <- paste(parent, positions, sep = "\r")
    positions[!slots %in% occupied_slots]
  })
  feasible_parents <- lengths(feasible_positions) > 0L
  parents <- parents[feasible_parents]
  feasible_positions <- feasible_positions[feasible_parents]
  if (length(parents) == 0L) {
    cli::cli_abort(
      "No feasible parent remains for a {context} after excluding occupied carbon positions.",
      call = rlang::caller_env()
    )
  }

  position_sets <- purrr::map(
    feasible_positions,
    ~ sort(unique(.x))
  )
  if (length(unique(position_sets)) != 1L) {
    cli::cli_abort(
      "Can't represent the feasible parent-position combinations for a {context} after excluding occupied carbon positions.",
      call = rlang::caller_env()
    )
  }

  position <- collapse_floating_substituent_positions(position_sets[[1]])
  substituent <- stringr::str_replace(
    substituent,
    "^[?0-9/]+",
    position
  )
  if (setequal(parents, main_vertices)) {
    parents <- integer()
  }
  list(substituent = substituent, parents = parents)
}

#' Normalize floating-part candidate parents
#'
#' @param parents Declared main-tree candidate vertex indices.
#' @param main_vertices Every main-tree vertex index.
#' @param linkage The floating part's attachment linkage.
#' @param occupied_slots Definitely occupied main-tree acceptor slots.
#' @param context Input-format context for error messages.
#'
#' @return An empty vector for an implicit all-main candidate set, otherwise
#'   the feasible explicit candidate parents.
#' @noRd
normalize_floating_part_parents <- function(
  parents,
  main_vertices,
  linkage,
  occupied_slots,
  context
) {
  if (setequal(parents, main_vertices)) {
    return(integer())
  }

  filter_floating_part_parents(
    parents,
    linkage,
    occupied_slots,
    context
  )
}

#' Remove infeasible candidate parents from a floating part
#'
#' @param parents Declared main-tree candidate vertex indices.
#' @param linkage The floating part's attachment linkage.
#' @param occupied_slots Definitely occupied main-tree acceptor slots.
#' @param context Input-format context for error messages.
#'
#' @return Feasible candidate parent indices.
#' @noRd
filter_floating_part_parents <- function(
  parents,
  linkage,
  occupied_slots,
  context
) {
  positions <- linkage_acceptor_positions(linkage)
  if (length(positions) == 0) {
    return(parents)
  }

  feasible <- purrr::map_lgl(parents, function(parent) {
    slots <- paste(parent, positions, sep = "\r")
    any(!slots %in% occupied_slots)
  })
  parents <- parents[feasible]
  if (length(parents) == 0) {
    cli::cli_abort(
      "No feasible parent remains for a {context} after excluding occupied acceptor positions.",
      call = rlang::caller_env()
    )
  }

  parents
}


filter_glycoct_und_parents <- function(parents, linkage, occupied_slots) {
  filter_floating_part_parents(
    parents,
    linkage,
    occupied_slots,
    context = "GlycoCT UND part"
  )
}

#' Format the reducing-end anomer stored in a GlycoCT graph
#'
#' @param reducing_end A consolidated reducing-end vertex.
#'
#' @return A character scalar such as `"a1"`, `"?1"`, or `"??"`.
#' @noRd
format_glycoct_reducing_anomer <- function(reducing_end) {
  if (isTRUE(reducing_end$is_alditol)) {
    return(paste0("?", glyrepr::get_anomer_pos(reducing_end$mono)))
  }

  anomer_config <- stringr::str_extract(reducing_end$anomer, "^[abx]")
  if (is.na(anomer_config) || anomer_config == "x") {
    anomer_config <- "?"
  }

  anomer_pos <- reducing_end$anomer_pos
  if (is.null(anomer_pos) || is.na(anomer_pos)) {
    anomer_pos <- decide_anomer_pos(reducing_end$mono)
  } else if (anomer_pos == "x") {
    anomer_pos <- "?"
  }

  paste0(anomer_config, anomer_pos)
}


#' Add unusual configurations to GlycoCT monosaccharide mappings
#'
#' @param mappings Named GlycoCT monosaccharide mappings.
#'
#' @return The mappings with configuration-inverted counterparts.
#' @noRd
add_unusual_glycoct_mappings <- function(mappings) {
  unusual_map <- unusual_configuration_monosaccharide_map()
  natural <- intersect(names(mappings), names(unusual_map))
  unusual <- mappings[natural]

  unusual <- purrr::map(unusual, function(mapping) {
    mapping$res <- stringr::str_replace_all(
      mapping$res,
      "(?<=-)[dl](?=[a-z])",
      \(configuration) invert_configuration(configuration)
    )
    mapping
  })
  names(unusual) <- unname(unusual_map[natural])
  duplicates <- purrr::map_lgl(
    unusual,
    \(mapping) any(purrr::map_lgl(mappings, identical, mapping))
  )
  unusual <- unusual[!duplicates]

  c(mappings, unusual)
}

load_mono_mappings <- function() {
  # Hardcoded GLYCOCT_MAP to avoid external file dependency
  GLYCOCT_MAP <- list(
    "Glc" = list(
      res = c("1b:a-dglc-HEX-1:5"),
      lin = NULL
    ),
    "Man" = list(
      res = c("1b:a-dman-HEX-1:5"),
      lin = NULL
    ),
    "Gal" = list(
      res = c("1b:a-dgal-HEX-1:5"),
      lin = NULL
    ),
    "Gul" = list(
      res = c("1b:a-dgul-HEX-1:5"),
      lin = NULL
    ),
    "Alt" = list(
      res = c("1b:a-lalt-HEX-1:5"),
      lin = NULL
    ),
    "All" = list(
      res = c("1b:a-dall-HEX-1:5"),
      lin = NULL
    ),
    "Tal" = list(
      res = c("1b:a-dtal-HEX-1:5"),
      lin = NULL
    ),
    "Ido" = list(
      res = c("1b:a-lido-HEX-1:5"),
      lin = NULL
    ),
    "GlcNAc" = list(
      res = c("1b:a-dglc-HEX-1:5", "2s:n-acetyl"),
      lin = c("1:1d(2+1)2n")
    ),
    "GalNAc" = list(
      res = c("1b:a-dgal-HEX-1:5", "2s:n-acetyl"),
      lin = c("1:1d(2+1)2n")
    ),
    "ManNAc" = list(
      res = c("1b:a-dman-HEX-1:5", "2s:n-acetyl"),
      lin = c("1:1d(2+1)2n")
    ),
    "GulNAc" = list(
      res = c("1b:a-dgul-HEX-1:5", "2s:n-acetyl"),
      lin = c("1:1d(2+1)2n")
    ),
    "AltNAc" = list(
      res = c("1b:a-lalt-HEX-1:5", "2s:n-acetyl"),
      lin = c("1:1d(2+1)2n")
    ),
    "AllNAc" = list(
      res = c("1b:a-dall-HEX-1:5", "2s:n-acetyl"),
      lin = c("1:1d(2+1)2n")
    ),
    "TalNAc" = list(
      res = c("1b:a-dtal-HEX-1:5", "2s:n-acetyl"),
      lin = c("1:1d(2+1)2n")
    ),
    "IdoNAc" = list(
      res = c("1b:a-lido-HEX-1:5", "2s:n-acetyl"),
      lin = c("1:1d(2+1)2n")
    ),
    "GlcN" = list(
      res = c("1b:a-dglc-HEX-1:5", "2s:amino"),
      lin = c("1:1d(2+1)2n")
    ),
    "ManN" = list(
      res = c("1b:a-dman-HEX-1:5", "2s:amino"),
      lin = c("1:1d(2+1)2n")
    ),
    "GalN" = list(
      res = c("1b:a-dgal-HEX-1:5", "2s:amino"),
      lin = c("1:1d(2+1)2n")
    ),
    "GulN" = list(
      res = c("1b:a-dgul-HEX-1:5", "2s:amino"),
      lin = c("1:1d(2+1)2n")
    ),
    "AltN" = list(
      res = c("1b:a-lalt-HEX-1:5", "2s:amino"),
      lin = c("1:1d(2+1)2n")
    ),
    "AllN" = list(
      res = c("1b:a-dall-HEX-1:5", "2s:amino"),
      lin = c("1:1d(2+1)2n")
    ),
    "TalN" = list(
      res = c("1b:a-dtal-HEX-1:5", "2s:amino"),
      lin = c("1:1d(2+1)2n")
    ),
    "IdoN" = list(
      res = c("1b:a-lido-HEX-1:5", "2s:amino"),
      lin = c("1:1d(2+1)2n")
    ),
    "GlcA" = list(
      res = c("1b:b-dglc-HEX-1:5|6:a"),
      lin = NULL
    ),
    "ManA" = list(
      res = c("1b:a-dman-HEX-1:5|6:a"),
      lin = NULL
    ),
    "GalA" = list(
      res = c("1b:a-dgal-HEX-1:5|6:a"),
      lin = NULL
    ),
    "GulA" = list(
      res = c("1b:a-dgul-HEX-1:5|6:a"),
      lin = NULL
    ),
    "AltA" = list(
      res = c("1b:a-lalt-HEX-1:5|6:a"),
      lin = NULL
    ),
    "AllA" = list(
      res = c("1b:a-dall-HEX-1:5|6:a"),
      lin = NULL
    ),
    "TalA" = list(
      res = c("1b:a-dtal-HEX-1:5|6:a"),
      lin = NULL
    ),
    "IdoA" = list(
      res = c("1b:a-lido-HEX-1:5|6:a"),
      lin = NULL
    ),
    "Fuc" = list(
      res = c("1b:a-lgal-HEX-1:5|6:d"),
      lin = NULL
    ),
    "Qui" = list(
      res = c("1b:a-dglc-HEX-1:5|6:d"),
      lin = NULL
    ),
    "Rha" = list(
      res = c("1b:a-lman-HEX-1:5|6:d"),
      lin = NULL
    ),
    "6dGul" = list(
      res = c("1b:a-dgul-HEX-1:5|6:d"),
      lin = NULL
    ),
    "6dAlt" = list(
      res = c("1b:a-lalt-HEX-1:5|6:d"),
      lin = NULL
    ),
    "6dTal" = list(
      res = c("1b:a-dtal-HEX-1:5|6:d"),
      lin = NULL
    ),
    "FucNAc" = list(
      res = c("1b:a-lgal-HEX-1:5|6:d", "2s:n-acetyl"),
      lin = c("1:1d(2+1)2n")
    ),
    "QuiNAc" = list(
      res = c("1b:a-dglc-HEX-1:5|6:d", "2s:n-acetyl"),
      lin = c("1:1d(2+1)2n")
    ),
    "RhaNAc" = list(
      res = c("1b:a-lman-HEX-1:5|6:d", "2s:n-acetyl"),
      lin = c("1:1d(2+1)2n")
    ),
    "6dAltNAc" = list(
      res = c("1b:a-lalt-HEX-1:5|6:d", "2s:n-acetyl"),
      lin = c("1:1d(2+1)2n")
    ),
    "6dTalNAc" = list(
      res = c("1b:a-dtal-HEX-1:5|6:d", "2s:n-acetyl"),
      lin = c("1:1d(2+1)2n")
    ),
    "Oli" = list(
      res = c("1b:a-dara-HEX-1:5|2:d|6:d"),
      lin = NULL
    ),
    "Tyv" = list(
      res = c("1b:a-dara-HEX-1:5|3:d|6:d"),
      lin = NULL
    ),
    "Abe" = list(
      res = c("1b:a-dxyl-HEX-1:5|3:d|6:d"),
      lin = NULL
    ),
    "Par" = list(
      res = c("1b:a-drib-HEX-1:5|3:d|6:d"),
      lin = NULL
    ),
    "Dig" = list(
      res = c("1b:a-drib-HEX-1:5|2:d|6:d"),
      lin = NULL
    ),
    "Col" = list(
      res = c("1b:a-lxyl-HEX-1:5|3:d|6:d"),
      lin = NULL
    ),
    "Ara" = list(
      res = c("1b:a-lara-PEN-1:5"),
      lin = NULL
    ),
    "Lyx" = list(
      res = c("1b:a-llyx-PEN-1:5"),
      lin = NULL
    ),
    "Xyl" = list(
      res = c("1b:a-dxyl-PEN-1:5"),
      lin = NULL
    ),
    "Rib" = list(
      res = c("1b:a-drib-PEN-1:5"),
      lin = NULL
    ),
    "Kdn" = list(
      res = c("1b:a-dgro-dgal-NON-2:6|1:a|2:keto|3:d"),
      lin = NULL
    ),
    "Neu" = list(
      res = c("1b:a-dgro-dgal-NON-2:6|1:a|2:keto|3:d", "2s:amino"),
      lin = c("1:1d(5+1)2n")
    ),
    "Neu5Ac" = list(
      res = c("1b:a-dgro-dgal-NON-2:6|1:a|2:keto|3:d", "2s:n-acetyl"),
      lin = c("1:1d(5+1)2n")
    ),
    "Neu5Gc" = list(
      res = c("1b:a-dgro-dgal-NON-2:6|1:a|2:keto|3:d", "2s:n-glycolyl"),
      lin = c("1:1d(5+1)2n")
    ),
    "Pse" = list(
      res = c("1b:a-lgro-lman-NON-2:6|2:keto|3:d|9:d", "2s:amino", "3s:amino"),
      lin = c("1:1d(5+1)2n", "2:1d(7+1)3n")
    ),
    "Leg" = list(
      res = c(
        "1b:a-dgro-dgal-NON-2:6|1:a|2:keto|3:d|9:d",
        "2s:amino",
        "3s:amino"
      ),
      lin = c("1:1d(5+1)2n", "2:1d(7+1)3n")
    ),
    "Aci" = list(
      res = c(
        "1b:a-lgro-lalt-NON-2:6|1:a|2:keto|3:d|9:d",
        "2s:amino",
        "3s:amino"
      ),
      lin = c("1:1d(5+1)2n", "2:1d(7+1)3n")
    ),
    "4eLeg" = list(
      res = c(
        "1b:a-dgro-dtal-NON-2:6|1:a|2:keto|3:d|9:d",
        "2s:amino",
        "3s:amino"
      ),
      lin = c("1:1d(5+1)2n", "2:1d(7+1)3n")
    ),
    "Bac" = list(
      res = c("1b:a-dglc-HEX-1:5|6:d", "2s:amino", "3s:amino"),
      lin = c("1:1d(2+1)2n", "2:1d(4+1)3n")
    ),
    "LDmanHep" = list(
      res = c("1b:a-lgro-dman-HEP-1:5"),
      lin = NULL
    ),
    "Kdo" = list(
      res = c("1b:a-dman-OCT-2:6|1:a|2:keto|3:d"),
      lin = NULL
    ),
    "Dha" = list(
      res = c("1b:a-dlyx-HEP-2:6|1:a|2:keto|3:d|7:a"),
      lin = NULL
    ),
    "DDmanHep" = list(
      res = c("1b:a-dgro-dman-HEP-1:5"),
      lin = NULL
    ),
    "MurNAc" = list(
      res = c("1b:a-dglc-HEX-1:5", "2s:n-acetyl", "3s:(r)-carboxyethyl"),
      lin = c("1:1d(2+1)2n", "2:1o(3+1)3n")
    ),
    "MurNGc" = list(
      res = c("1b:a-dglc-HEX-1:5", "2s:n-glycolyl", "3s:(r)-carboxyethyl"),
      lin = c("1:1d(2+1)2n", "2:1o(3+1)3n")
    ),
    "Mur" = list(
      res = c("1b:a-dglc-HEX-1:5", "2s:(r)-carboxyethyl"),
      lin = c("1:1o(3+1)2n")
    ),
    "Fru" = list(
      res = c("1b:a-dara-HEX-2:6|2:keto"),
      lin = NULL
    ),
    "Tag" = list(
      res = c("1b:a-dlyx-HEX-2:6|2:keto"),
      lin = NULL
    ),
    "Sor" = list(
      res = c("1b:a-lxyl-HEX-2:6|2:keto"),
      lin = NULL
    ),
    "Psi" = list(
      res = c("1b:a-drib-HEX-2:6|2:keto"),
      lin = NULL
    )
  )

  add_unusual_glycoct_mappings(GLYCOCT_MAP)
}

#' Load GlycoCT alditol monosaccharide mappings
#'
#' @return A named list of GlycoCT alditol residue mappings.
#' @noRd
load_alditol_mono_mappings <- function() {
  # GlycanFormatConverter represents GlycoCT alditols as open-chain residues
  # with 0:0 ring bounds and a 1:aldi modification.
  GLYCOCT_ALDITOL_MAP <- list(
    "Glc" = list(
      res = c("1b:o-dglc-HEX-0:0|1:aldi"),
      lin = NULL
    ),
    "Man" = list(
      res = c("1b:o-dman-HEX-0:0|1:aldi"),
      lin = NULL
    ),
    "Gal" = list(
      res = c("1b:o-dgal-HEX-0:0|1:aldi"),
      lin = NULL
    ),
    "Gul" = list(
      res = c("1b:o-dgul-HEX-0:0|1:aldi"),
      lin = NULL
    ),
    "Alt" = list(
      res = c("1b:o-ltal-HEX-0:0|1:aldi"),
      lin = NULL
    ),
    "All" = list(
      res = c("1b:o-dall-HEX-0:0|1:aldi"),
      lin = NULL
    ),
    "Tal" = list(
      res = c("1b:o-dalt-HEX-0:0|1:aldi"),
      lin = NULL
    ),
    "Ido" = list(
      res = c("1b:o-lido-HEX-0:0|1:aldi"),
      lin = NULL
    ),
    "GlcNAc" = list(
      res = c("1b:o-dglc-HEX-0:0|1:aldi", "2s:n-acetyl"),
      lin = c("1:1d(2+1)2n")
    ),
    "GalNAc" = list(
      res = c("1b:o-dgal-HEX-0:0|1:aldi", "2s:n-acetyl"),
      lin = c("1:1d(2+1)2n")
    ),
    "ManNAc" = list(
      res = c("1b:o-dman-HEX-0:0|1:aldi", "2s:n-acetyl"),
      lin = c("1:1d(2+1)2n")
    ),
    "GulNAc" = list(
      res = c("1b:o-dgul-HEX-0:0|1:aldi", "2s:n-acetyl"),
      lin = c("1:1d(2+1)2n")
    ),
    "AltNAc" = list(
      res = c("1b:o-ltal-HEX-0:0|1:aldi", "2s:n-acetyl"),
      lin = c("1:1d(5+1)2n")
    ),
    "AllNAc" = list(
      res = c("1b:o-dall-HEX-0:0|1:aldi", "2s:n-acetyl"),
      lin = c("1:1d(2+1)2n")
    ),
    "TalNAc" = list(
      res = c("1b:o-dalt-HEX-0:0|1:aldi", "2s:n-acetyl"),
      lin = c("1:1d(5+1)2n")
    ),
    "IdoNAc" = list(
      res = c("1b:o-lido-HEX-0:0|1:aldi", "2s:n-acetyl"),
      lin = c("1:1d(2+1)2n")
    ),
    "GlcN" = list(
      res = c("1b:o-dglc-HEX-0:0|1:aldi", "2s:amino"),
      lin = c("1:1d(2+1)2n")
    ),
    "ManN" = list(
      res = c("1b:o-dman-HEX-0:0|1:aldi", "2s:amino"),
      lin = c("1:1d(2+1)2n")
    ),
    "GalN" = list(
      res = c("1b:o-dgal-HEX-0:0|1:aldi", "2s:amino"),
      lin = c("1:1d(2+1)2n")
    ),
    "GulN" = list(
      res = c("1b:o-dgul-HEX-0:0|1:aldi", "2s:amino"),
      lin = c("1:1d(2+1)2n")
    ),
    "AltN" = list(
      res = c("1b:o-ltal-HEX-0:0|1:aldi", "2s:amino"),
      lin = c("1:1d(5+1)2n")
    ),
    "AllN" = list(
      res = c("1b:o-dall-HEX-0:0|1:aldi", "2s:amino"),
      lin = c("1:1d(2+1)2n")
    ),
    "TalN" = list(
      res = c("1b:o-dalt-HEX-0:0|1:aldi", "2s:amino"),
      lin = c("1:1d(5+1)2n")
    ),
    "IdoN" = list(
      res = c("1b:o-lido-HEX-0:0|1:aldi", "2s:amino"),
      lin = c("1:1d(2+1)2n")
    ),
    "Fuc" = list(
      res = c("1b:o-lgal-HEX-0:0|1:aldi|6:d"),
      lin = NULL
    ),
    "Qui" = list(
      res = c("1b:o-dglc-HEX-0:0|1:aldi|6:d"),
      lin = NULL
    ),
    "Rha" = list(
      res = c("1b:o-lman-HEX-0:0|1:aldi|6:d"),
      lin = NULL
    ),
    "6dGul" = list(
      res = c("1b:o-dgul-HEX-0:0|1:aldi|6:d"),
      lin = NULL
    ),
    "6dAlt" = list(
      res = c("1b:o-lalt-HEX-0:0|1:aldi|6:d"),
      lin = NULL
    ),
    "6dTal" = list(
      res = c("1b:o-dtal-HEX-0:0|1:aldi|6:d"),
      lin = NULL
    ),
    "FucNAc" = list(
      res = c("1b:o-lgal-HEX-0:0|1:aldi|6:d", "2s:n-acetyl"),
      lin = c("1:1d(2+1)2n")
    ),
    "QuiNAc" = list(
      res = c("1b:o-dglc-HEX-0:0|1:aldi|6:d", "2s:n-acetyl"),
      lin = c("1:1d(2+1)2n")
    ),
    "RhaNAc" = list(
      res = c("1b:o-lman-HEX-0:0|1:aldi|6:d", "2s:n-acetyl"),
      lin = c("1:1d(2+1)2n")
    ),
    "6dAltNAc" = list(
      res = c("1b:o-lalt-HEX-0:0|1:aldi|6:d", "2s:n-acetyl"),
      lin = c("1:1d(2+1)2n")
    ),
    "6dTalNAc" = list(
      res = c("1b:o-dtal-HEX-0:0|1:aldi|6:d", "2s:n-acetyl"),
      lin = c("1:1d(2+1)2n")
    ),
    "Oli" = list(
      res = c("1b:o-dara-HEX-0:0|1:aldi|2:d|6:d"),
      lin = NULL
    ),
    "Tyv" = list(
      res = c("1b:o-dara-HEX-0:0|1:aldi|3:d|6:d"),
      lin = NULL
    ),
    "Abe" = list(
      res = c("1b:o-dxyl-HEX-0:0|1:aldi|3:d|6:d"),
      lin = NULL
    ),
    "Par" = list(
      res = c("1b:o-drib-HEX-0:0|1:aldi|3:d|6:d"),
      lin = NULL
    ),
    "Dig" = list(
      res = c("1b:o-drib-HEX-0:0|1:aldi|2:d|6:d"),
      lin = NULL
    ),
    "Col" = list(
      res = c("1b:o-lxyl-HEX-0:0|1:aldi|3:d|6:d"),
      lin = NULL
    ),
    "Lyx" = list(
      res = c("1b:o-llyx-PEN-0:0|1:aldi"),
      lin = NULL
    ),
    "Xyl" = list(
      res = c("1b:o-dxyl-PEN-0:0|1:aldi"),
      lin = NULL
    ),
    "Rib" = list(
      res = c("1b:o-drib-PEN-0:0|1:aldi"),
      lin = NULL
    ),
    "Bac" = list(
      res = c(
        "1b:o-dglc-HEX-0:0|1:aldi|6:d",
        "2s:amino",
        "3s:amino"
      ),
      lin = c("1:1d(2+1)2n", "2:1d(4+1)3n")
    ),
    "LDmanHep" = list(
      res = c("1b:o-dgro-dgal-HEP-0:0|1:aldi"),
      lin = NULL
    ),
    "DDmanHep" = list(
      res = c("1b:o-dgro-dman-HEP-0:0|1:aldi"),
      lin = NULL
    ),
    "MurNAc" = list(
      res = c(
        "1b:o-dglc-HEX-0:0|1:aldi",
        "2s:n-acetyl",
        "3s:(r)-carboxyethyl"
      ),
      lin = c("1:1d(2+1)2n", "2:1o(3+1)3n")
    ),
    "MurNGc" = list(
      res = c(
        "1b:o-dglc-HEX-0:0|1:aldi",
        "2s:n-glycolyl",
        "3s:(r)-carboxyethyl"
      ),
      lin = c("1:1d(2+1)2n", "2:1o(3+1)3n")
    ),
    "Mur" = list(
      res = c("1b:o-dglc-HEX-0:0|1:aldi", "2s:(r)-carboxyethyl"),
      lin = c("1:1o(3+1)2n")
    )
  )

  add_unusual_glycoct_mappings(GLYCOCT_ALDITOL_MAP)
}

compile_glycoct_mapping <- function(name, mapping) {
  mono_lines <- mapping$res[grepl("^\\d+b:", mapping$res)]
  pattern_mono <- NULL
  if (length(mono_lines) > 0) {
    pattern_mono <- sub("^\\d+b:[abxo]-", "", mono_lines[[1]])
  }

  sub_lines <- mapping$res[grepl("^\\d+s:", mapping$res)]
  pattern_subs <- sort(sub("^\\d+s:", "", sub_lines))

  pattern_linkages <- character()
  if (!is.null(mapping$lin)) {
    linkage_matches <- regexec(
      "\\d+:(\\d+)[do]?\\((\\d+)\\+(\\d+)\\)(\\d+)[dn]?",
      mapping$lin
    )
    linkage_parts <- regmatches(mapping$lin, linkage_matches)
    pattern_linkages <- sort(vapply(
      linkage_parts,
      function(parts) {
        if (length(parts) == 0) {
          return(NA_character_)
        }
        paste0(parts[[3]], "+", parts[[4]])
      },
      character(1)
    ))
    pattern_linkages <- pattern_linkages[!is.na(pattern_linkages)]
  }

  c(
    list(name = name),
    glycoct_mono_signature(pattern_mono),
    list(
      subs = pattern_subs,
      linkages = pattern_linkages,
      single = length(mapping$res) == 1L
    )
  )
}

glycoct_mono_signature <- function(content) {
  bounds <- extract_glycoct_ring_bounds(content)
  list(
    mono = content,
    core = remove_glycoct_ring_bounds(content),
    bounds = bounds,
    wildcard_bounds = !is.na(bounds) && grepl("x", bounds, fixed = TRUE),
    alditol = is_glycoct_alditol_mono(content)
  )
}

glycoct_signature_mono_matches <- function(signature, entry) {
  if (!identical(signature$alditol, entry$alditol)) {
    return(FALSE)
  }
  if (is.na(signature$bounds) || is.na(entry$bounds)) {
    return(identical(signature$bounds, entry$bounds))
  }
  same_ring_family <- is_supported_glycoct_ring_bounds(signature$bounds) &&
    is_supported_glycoct_ring_bounds(entry$bounds) &&
    parse_glycoct_ring_bounds(signature$bounds)[[1]] ==
      parse_glycoct_ring_bounds(entry$bounds)[[1]]
  identical(signature$bounds, entry$bounds) ||
    signature$wildcard_bounds ||
    entry$wildcard_bounds ||
    same_ring_family
}

glycoct_signature_key <- function(core, subs, linkages) {
  paste(
    core,
    paste(subs, collapse = "\r"),
    paste(linkages, collapse = "\r"),
    sep = "\n"
  )
}

compile_glycoct_mapping_index <- function(mappings) {
  entries <- Map(
    compile_glycoct_mapping,
    names(mappings),
    unname(mappings)
  )
  cores <- vapply(entries, `[[`, character(1), "core")
  keys <- vapply(
    entries,
    function(entry) {
      glycoct_signature_key(
        entry$core,
        entry$subs,
        entry$linkages
      )
    },
    character(1)
  )

  list(
    entries = entries,
    exact = split(seq_along(entries), keys),
    by_core = split(seq_along(entries), cores)
  )
}

glycoct_mapping_index <- local({
  regular <- NULL
  alditol_index <- NULL

  function(alditol = FALSE) {
    if (alditol) {
      if (is.null(alditol_index)) {
        alditol_index <<- compile_glycoct_mapping_index(
          load_alditol_mono_mappings()
        )
      }
      return(alditol_index)
    }

    if (is.null(regular)) {
      regular <<- compile_glycoct_mapping_index(load_mono_mappings())
    }
    regular
  }
})

glycoct_group_signature <- function(group, residues, linkages) {
  group_mono <- NULL
  group_subs <- character()
  group_linkages <- character()

  for (id in group) {
    res <- residues[[as.character(id)]]
    if (is.null(res)) {
      next
    }
    if (res$type == "mono") {
      group_mono <- res$content
    } else if (res$type == "sub") {
      group_subs <- c(group_subs, res$content)
    }
  }

  for (linkage in linkages) {
    if (!(linkage$from_res %in% group && linkage$to_res %in% group)) {
      next
    }
    from_res <- residues[[as.character(linkage$from_res)]]
    to_res <- residues[[as.character(linkage$to_res)]]
    if (
      !is.null(from_res) &&
        !is.null(to_res) &&
        from_res$type == "mono" &&
        to_res$type == "sub"
    ) {
      group_linkages <- c(
        group_linkages,
        paste0(linkage$from_pos, "+", linkage$to_pos)
      )
    }
  }

  mono_signature <- if (is.null(group_mono)) {
    list(
      mono = NULL,
      core = NULL,
      bounds = NA_character_,
      wildcard_bounds = FALSE,
      alditol = FALSE
    )
  } else {
    glycoct_mono_signature(group_mono)
  }

  c(
    mono_signature,
    list(
      subs = sort(group_subs),
      linkages = sort(group_linkages)
    )
  )
}

consolidate_residues <- function(residues, linkages, mono_mapping_index) {
  has_alditol <- has_glycoct_alditol_residue(residues)
  alditol_mapping_index <- if (has_alditol) {
    glycoct_mapping_index(alditol = TRUE)
  } else {
    NULL
  }

  # Group residues by their linkage relationships to identify composite structures
  composite_groups <- find_composite_groups(residues, linkages)

  vertices <- list()
  edges <- list()

  for (group in composite_groups) {
    if (length(group) == 1) {
      # Single monosaccharide
      res <- residues[[as.character(group[1])]]
      if (!is.null(res) && res$type == "mono") {
        mono_name <- map_single_mono(res$content)
        vertices <- append(
          vertices,
          list(list(
            original_id = group[1],
            mono = mono_name,
            sub = "",
            anomer = res$anomer,
            anomer_pos = res$anomer_pos,
            is_alditol = isTRUE(res$is_alditol)
          ))
        )
      }
    } else {
      group_signature <- glycoct_group_signature(
        group,
        residues,
        linkages
      )

      # Composite structure - try to match with known patterns
      matched <- match_composite_structure(
        group_signature,
        alditol_mapping_index
      )
      if (is.null(matched)) {
        matched <- match_composite_structure(
          group_signature,
          mono_mapping_index
        )
      }

      # Find the main monosaccharide (the one with type "mono")
      main_mono_id <- NULL
      main_mono <- NULL
      for (id in group) {
        res <- residues[[as.character(id)]]
        if (!is.null(res) && res$type == "mono") {
          main_mono_id <- id
          main_mono <- res
          break
        }
      }

      if (!is.null(main_mono)) {
        if (!is.null(matched)) {
          # Exact match found
          vertices <- append(
            vertices,
            list(list(
              original_id = main_mono_id,
              mono = preserve_glycoct_ring(matched, main_mono$content),
              sub = "",
              anomer = main_mono$anomer,
              anomer_pos = main_mono$anomer_pos,
              is_alditol = isTRUE(main_mono$is_alditol)
            ))
          )
        } else {
          # No exact match - try partial matching with extra substituents
          partial_mapping_index <- if (isTRUE(main_mono$is_alditol)) {
            alditol_mapping_index
          } else {
            mono_mapping_index
          }
          partial_result <- match_partial_composite_structure(
            group,
            residues,
            linkages,
            group_signature,
            partial_mapping_index
          )
          vertices <- append(
            vertices,
            list(list(
              original_id = main_mono_id,
              mono = preserve_glycoct_ring(
                partial_result$mono,
                main_mono$content
              ),
              sub = partial_result$sub,
              anomer = main_mono$anomer,
              anomer_pos = main_mono$anomer_pos,
              is_alditol = isTRUE(main_mono$is_alditol)
            ))
          )
        }
      }
    }
  }

  # Build edges for inter-group connections
  for (linkage in linkages) {
    from_group <- find_group_containing(composite_groups, linkage$from_res)
    to_group <- find_group_containing(composite_groups, linkage$to_res)

    # Only add edge if it connects different groups
    if (!identical(from_group, to_group)) {
      edges <- append(edges, list(linkage))
    }
  }

  list(vertices = vertices, edges = edges)
}

find_composite_groups <- function(residues, linkages) {
  # Create groups of residues that are linked together
  groups <- list()

  # Start with each monosaccharide as its own group
  mono_ids <- names(residues)[sapply(residues, function(r) r$type == "mono")]
  for (id in mono_ids) {
    groups[[length(groups) + 1]] <- as.integer(id)
  }

  # Add substituents to their parent monosaccharides
  for (linkage in linkages) {
    from_res <- residues[[as.character(linkage$from_res)]]
    to_res <- residues[[as.character(linkage$to_res)]]

    if (
      !is.null(from_res) &&
        !is.null(to_res) &&
        from_res$type == "mono" &&
        to_res$type == "sub"
    ) {
      # Add substituent to monosaccharide group
      for (i in seq_along(groups)) {
        if (linkage$from_res %in% groups[[i]]) {
          groups[[i]] <- c(groups[[i]], linkage$to_res)
          break
        }
      }
    }
  }

  groups
}

match_composite_structure <- function(
  group_signature,
  mapping_index
) {
  if (is.null(mapping_index) || is.null(group_signature$core)) {
    return(NULL)
  }

  key <- glycoct_signature_key(
    group_signature$core,
    group_signature$subs,
    group_signature$linkages
  )
  candidates <- mapping_index$exact[[key]]
  if (is.null(candidates)) {
    return(NULL)
  }

  for (idx in candidates) {
    entry <- mapping_index$entries[[idx]]
    if (glycoct_signature_mono_matches(group_signature, entry)) {
      return(entry$name)
    }
  }

  NULL
}

match_partial_composite_structure <- function(
  group,
  residues,
  linkages,
  group_signature,
  mapping_index
) {
  generic_result <- match_generic_glycoct_composite(group, residues, linkages)
  if (!is.null(generic_result)) {
    return(generic_result)
  }

  # Try to find the best partial match and identify extra substituents
  best_match <- NULL
  best_extra_subs <- c()
  group_subs <- group_signature$subs
  group_mono <- group_signature$mono

  candidate_indices <- if (
    is.null(mapping_index) || is.null(group_signature$core)
  ) {
    integer()
  } else {
    mapping_index$by_core[[group_signature$core]]
  }

  # Try to find a known composite that uses a subset of our substituents
  for (idx in candidate_indices) {
    entry <- mapping_index$entries[[idx]]
    pattern_subs <- entry$subs

    # Check if pattern substituents are a subset of group substituents
    if (length(pattern_subs) > 0 && all(pattern_subs %in% group_subs)) {
      if (
        !is.null(group_mono) &&
          glycoct_signature_mono_matches(group_signature, entry)
      ) {
        exact_match <- identical(pattern_subs, group_signature$subs) &&
          identical(entry$linkages, group_signature$linkages)
        if (exact_match) {
          next
        }

        # For partial matching, only consider if there are extra substituents
        if (length(pattern_subs) < length(group_subs)) {
          extra_subs <- setdiff(group_subs, pattern_subs)

          if (
            is.null(best_match) ||
              length(pattern_subs) > length(best_match$pattern_subs)
          ) {
            best_match <- list(
              mono_name = entry$name,
              pattern_subs = pattern_subs
            )
            best_extra_subs <- extra_subs
          }
        }
      }
    }
  }

  if (!is.null(best_match)) {
    # Format extra substituents with position information
    formatted_extra_subs <- format_extra_substituents(
      best_extra_subs,
      group,
      residues,
      linkages
    )

    return(list(
      mono = best_match$mono_name,
      sub = formatted_extra_subs
    ))
  } else {
    # No partial match found, return base monosaccharide with all substituents
    if (!is.null(group_mono)) {
      base_mono <- map_single_mono(group_mono)
      formatted_subs <- format_extra_substituents(
        group_subs,
        group,
        residues,
        linkages
      )
      return(list(
        mono = base_mono,
        sub = formatted_subs
      ))
    }
  }

  return(list(mono = "Unk", sub = ""))
}

#' Match generic GlycoCT monosaccharide composites
#'
#' @param group A vector of residue IDs in one composite group.
#' @param residues A parsed GlycoCT residue list.
#' @param linkages A parsed GlycoCT linkage list.
#'
#' @return A list with `mono` and `sub`, or `NULL` if no generic composite
#'   matches.
#' @noRd
match_generic_glycoct_composite <- function(group, residues, linkages) {
  group_mono <- NULL
  group_subs <- c()

  for (id in group) {
    res <- residues[[as.character(id)]]
    if (is.null(res)) {
      next
    }
    if (res$type == "mono") {
      group_mono <- res
    } else if (res$type == "sub") {
      group_subs <- c(group_subs, res$content)
    }
  }

  if (is.null(group_mono)) {
    return(NULL)
  }

  if (
    "n-sulfate" %in%
      group_subs &&
      has_glycoct_substituent_at(group, residues, linkages, "n-sulfate", "2")
  ) {
    amino_mono <- map_glycoct_n_sulfate_mono(group_mono$content)
    if (!is.na(amino_mono)) {
      extra_subs <- group_subs[group_subs != "n-sulfate"]
      extra_sub <- format_extra_substituents(
        extra_subs,
        group,
        residues,
        linkages
      )
      sub <- paste(c("2S", extra_sub[extra_sub != ""]), collapse = ",")
      return(list(mono = amino_mono, sub = sub))
    }
  }

  if (
    is_generic_glycoct_hex(group_mono$content) &&
      "n-acetyl" %in% group_subs &&
      has_glycoct_substituent_at(group, residues, linkages, "n-acetyl", "2")
  ) {
    mono <- if (is_generic_glycoct_dhex(group_mono$content)) {
      "dHexNAc"
    } else {
      "HexNAc"
    }
    extra_subs <- group_subs[group_subs != "n-acetyl"]
    extra_sub <- format_extra_substituents(
      extra_subs,
      group,
      residues,
      linkages
    )

    return(list(mono = mono, sub = extra_sub))
  }

  sialic_identity_sub <- find_generic_glycoct_sialic_identity_sub(
    group,
    residues,
    linkages,
    group_mono$content,
    group_subs
  )
  if (!is.na(sialic_identity_sub)) {
    mono <- dplyr::case_when(
      sialic_identity_sub == "n-acetyl" ~ "Neu5Ac",
      sialic_identity_sub == "n-glycolyl" ~ "Neu5Gc"
    )
    extra_subs <- group_subs[group_subs != sialic_identity_sub]
    extra_sub <- format_extra_substituents(
      extra_subs,
      group,
      residues,
      linkages
    )

    return(list(mono = mono, sub = extra_sub))
  }

  NULL
}

#' Find the identity-defining substituent for generic GlycoCT sialic acids
#'
#' @param group A vector of residue IDs in one composite group.
#' @param residues A parsed GlycoCT residue list.
#' @param linkages A parsed GlycoCT linkage list.
#' @param mono_content GlycoCT monosaccharide content without the leading
#'   anomer.
#' @param group_subs Substituent content strings in the group.
#'
#' @return A character scalar with the identity-defining substituent, or `NA`
#'   when the group is not a supported generic sialic acid.
#' @noRd
find_generic_glycoct_sialic_identity_sub <- function(
  group,
  residues,
  linkages,
  mono_content,
  group_subs
) {
  if (!is_generic_glycoct_non(mono_content)) {
    return(NA_character_)
  }

  candidate_subs <- c("n-acetyl", "n-glycolyl")
  matched_subs <- candidate_subs[
    candidate_subs %in%
      group_subs &
      purrr::map_lgl(
        candidate_subs,
        ~ has_glycoct_substituent_at(
          group,
          residues,
          linkages,
          .x,
          "5"
        )
      )
  ]

  if (length(matched_subs) != 1) {
    return(NA_character_)
  }

  matched_subs
}

#' Map a GlycoCT monosaccharide with direct N-sulfation to an amino sugar
#'
#' @param content GlycoCT monosaccharide content without the leading anomer.
#'
#' @return A monosaccharide name, or `NA` if unsupported.
#' @noRd
map_glycoct_n_sulfate_mono <- function(content) {
  base_mono <- map_single_mono(content)
  if (identical(base_mono, "Unk")) {
    return(NA_character_)
  }

  if (identical(base_mono, "Hex")) {
    return("HexN")
  }

  paste0(base_mono, "N")
}

#' Check whether a GlycoCT group contains a linked substituent
#'
#' @param group A vector of residue IDs in one composite group.
#' @param residues A parsed GlycoCT residue list.
#' @param linkages A parsed GlycoCT linkage list.
#' @param substituent A substituent content string.
#' @param position A monosaccharide position string.
#'
#' @return A logical scalar.
#' @noRd
has_glycoct_substituent_at <- function(
  group,
  residues,
  linkages,
  substituent,
  position
) {
  any(purrr::map_lgl(linkages, function(linkage) {
    from_res <- residues[[as.character(linkage$from_res)]]
    to_res <- residues[[as.character(linkage$to_res)]]
    !is.null(from_res) &&
      !is.null(to_res) &&
      linkage$from_res %in% group &&
      linkage$to_res %in% group &&
      identical(from_res$type, "mono") &&
      identical(to_res$type, "sub") &&
      identical(to_res$content, substituent) &&
      identical(linkage$from_pos, position)
  }))
}

format_extra_substituents <- function(extra_subs, group, residues, linkages) {
  # Format substituents with position information (e.g., "6S" for sulfate at position 6)
  formatted <- c()

  for (sub_content in extra_subs) {
    # Find the position of this substituent
    position <- "?"

    for (linkage in linkages) {
      if (linkage$to_res %in% group) {
        to_res <- residues[[as.character(linkage$to_res)]]
        if (
          !is.null(to_res) &&
            to_res$type == "sub" &&
            to_res$content == sub_content
        ) {
          position <- as.character(linkage$from_pos)
          break
        }
      }
    }

    # Handle unknown position
    if (position == "-1") {
      position <- "?"
    }

    # Format the substituent with position
    formatted <- c(
      formatted,
      paste0(position, glycoct_substituent_abbreviation(sub_content))
    )
  }

  # Join multiple substituents
  paste(formatted, collapse = ",")
}

glycoct_substituent_abbreviation <- function(x) {
  switch(
    x,
    sulfate = "S",
    `n-acetyl` = "Ac",
    acetyl = "Ac",
    methyl = "Me",
    amino = "N",
    phosphate = "P",
    `phospho-ethanolamine` = "PEtn",
    `diphospho-ethanolamine` = "PPEtn",
    x
  )
}

matches_glycoct_pattern <- function(group, residues, linkages, mapping) {
  # Extract the group's structure
  group_mono <- NULL
  group_subs <- c()
  group_linkages <- c()

  for (id in group) {
    res <- residues[[as.character(id)]]
    if (!is.null(res)) {
      if (res$type == "mono") {
        group_mono <- res$content
      } else if (res$type == "sub") {
        group_subs <- c(group_subs, res$content)
      }
    }
  }

  # Get linkages within this group
  for (linkage in linkages) {
    if (linkage$from_res %in% group && linkage$to_res %in% group) {
      from_res <- residues[[as.character(linkage$from_res)]]
      to_res <- residues[[as.character(linkage$to_res)]]
      if (
        !is.null(from_res) &&
          !is.null(to_res) &&
          from_res$type == "mono" &&
          to_res$type == "sub"
      ) {
        group_linkages <- c(
          group_linkages,
          paste0(linkage$from_pos, "+", linkage$to_pos)
        )
      }
    }
  }

  # Parse the mapping pattern
  pattern_mono <- NULL
  pattern_subs <- c()
  pattern_linkages <- c()

  # Extract monosaccharide from pattern
  mono_lines <- mapping$res[stringr::str_detect(mapping$res, "^\\d+b:")]
  if (length(mono_lines) > 0) {
    mono_content <- stringr::str_remove(mono_lines[1], "^\\d+b:")
    pattern_mono <- stringr::str_remove(mono_content, "^[abxo]-")
  }

  # Extract substituents from pattern
  sub_lines <- mapping$res[stringr::str_detect(mapping$res, "^\\d+s:")]
  for (sub_line in sub_lines) {
    sub_content <- stringr::str_remove(sub_line, "^\\d+s:")
    pattern_subs <- c(pattern_subs, sub_content)
  }

  # Extract linkages from pattern
  if (!is.null(mapping$lin)) {
    for (lin_line in mapping$lin) {
      link_pattern <- stringr::str_match(
        lin_line,
        "\\d+:(\\d+)[do]?\\((\\d+)\\+(\\d+)\\)(\\d+)[dn]?"
      )
      if (!is.na(link_pattern[1])) {
        from_pos <- link_pattern[3]
        to_pos <- link_pattern[4]
        pattern_linkages <- c(pattern_linkages, paste0(from_pos, "+", to_pos))
      }
    }
  }

  # Compare structures - exact match for monosaccharide content
  mono_match <- !is.null(group_mono) &&
    !is.null(pattern_mono) &&
    glycoct_mono_content_matches(group_mono, pattern_mono)

  subs_match <- length(group_subs) == length(pattern_subs) &&
    all(sort(group_subs) == sort(pattern_subs))

  linkages_match <- length(group_linkages) == length(pattern_linkages) &&
    all(sort(group_linkages) == sort(pattern_linkages))

  return(mono_match && subs_match && linkages_match)
}

map_single_mono <- function(content) {
  preserve_glycoct_ring(map_single_mono_ringless(content), content)
}

map_single_mono_ringless <- function(content) {
  # Extract monosaccharide name from content like "dglc-HEX-1:5" or "lgal-HEX-1:5|6:d"

  is_alditol <- is_glycoct_alditol_mono(content)
  mapping_index <- glycoct_mapping_index(alditol = is_alditol)
  mono_signature <- glycoct_mono_signature(content)
  candidates <- mapping_index$by_core[[mono_signature$core]]
  if (!is.null(candidates)) {
    for (idx in candidates) {
      entry <- mapping_index$entries[[idx]]
      if (
        entry$single &&
          length(entry$subs) == 0L &&
          glycoct_signature_mono_matches(mono_signature, entry)
      ) {
        return(entry$name)
      }
    }
  }
  if (is_alditol) {
    return("Unk")
  }

  if (is_generic_glycoct_dhex(content)) {
    return("dHex")
  }
  if (is_generic_glycoct_hex(content)) {
    return("Hex")
  }
  if (is_generic_glycoct_pen(content)) {
    return("Pen")
  }

  # If no exact match, fall back to basic parsing
  parts <- stringr::str_split(content, "-")[[1]]
  if (length(parts) >= 2) {
    stereo_name <- parts[1]
    class_name <- parts[2]

    # Handle specific stereochemistry patterns
    if (stringr::str_detect(stereo_name, "dglc")) {
      return("Glc")
    }
    if (stringr::str_detect(stereo_name, "dgal")) {
      return("Gal")
    }
    if (stringr::str_detect(stereo_name, "dman")) {
      return("Man")
    }
    if (stringr::str_detect(stereo_name, "dxyl")) {
      return("Xyl")
    }
    if (stringr::str_detect(stereo_name, "lgal.*6:d")) {
      return("Fuc")
    } # Fucose pattern

    # Basic patterns without D/L prefix
    if (stringr::str_detect(stereo_name, "glc")) {
      return("Glc")
    }
    if (stringr::str_detect(stereo_name, "gal")) {
      return("Gal")
    }
    if (stringr::str_detect(stereo_name, "man")) {
      return("Man")
    }
    if (stringr::str_detect(stereo_name, "xyl")) {
      return("Xyl")
    }

    # Handle GlcA (glucuronic acid) pattern
    if (stringr::str_detect(content, "dglc.*6:a")) {
      return("GlcA")
    }

    # Fallback to class name
    if (class_name == "HEX") {
      return("Hex")
    }
    if (class_name == "PEN") return("Pen")
  }

  return("Unk")
}

#' Check whether a GlycoCT content string is a generic hexose
#'
#' @param content GlycoCT monosaccharide content without the leading anomer.
#'
#' @return A logical scalar.
#' @noRd
is_generic_glycoct_hex <- function(content) {
  isTRUE(stringr::str_detect(
    content,
    "^HEX-(?:\\d+|x):(?:\\d+|x)(?:\\|6:d)?$"
  ))
}

#' Check whether a GlycoCT content string is a generic pentose
#'
#' @param content GlycoCT monosaccharide content without the leading anomer.
#'
#' @return A logical scalar.
#' @noRd
is_generic_glycoct_pen <- function(content) {
  isTRUE(stringr::str_detect(
    content,
    "^PEN-(?:\\d+|x):(?:\\d+|x)$"
  ))
}

#' Check whether a GlycoCT content string is a generic deoxyhexose
#'
#' @param content GlycoCT monosaccharide content without the leading anomer.
#'
#' @return A logical scalar.
#' @noRd
is_generic_glycoct_dhex <- function(content) {
  isTRUE(stringr::str_detect(
    content,
    "^HEX-(?:\\d+|x):(?:\\d+|x)\\|6:d$"
  ))
}

#' Check whether a GlycoCT content string is a generic sialic acid backbone
#'
#' @param content GlycoCT monosaccharide content without the leading anomer.
#'
#' @return A logical scalar.
#' @noRd
is_generic_glycoct_non <- function(content) {
  isTRUE(identical(content, "NON-2:6|1:a|2:keto|3:d"))
}

find_group_containing <- function(groups, res_id) {
  for (group in groups) {
    if (res_id %in% group) {
      return(group)
    }
  }
  return(NULL)
}

find_reducing_end <- function(vertices, edges) {
  # The reducing end is typically the vertex that is not a target of any edge
  if (length(edges) == 0) {
    return(vertices[[1]])
  }

  target_ids <- sapply(edges, function(e) e$to_res)

  for (vertex in vertices) {
    if (!(vertex$original_id %in% target_ids)) {
      return(vertex)
    }
  }

  # Fallback to first vertex
  return(vertices[[1]])
}
