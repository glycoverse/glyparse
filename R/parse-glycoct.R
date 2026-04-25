#' Parse GlycoCT Structures
#'
#' This function parses GlycoCT strings into a [glyrepr::glycan_structure()].
#' GlycoCT is a format used by databases like GlyTouCan and GlyGen.
#'
#' @details
#' GlycoCT format consists of two parts:
#' - RES: Contains monosaccharides (lines starting with 'b:') and substituents (lines starting with 's:')
#' - LIN: Contains linkage information between residues
#' - REP: Contains fixed-count repeated units
#'
#' For more information about GlycoCT format, see the glycoct.md documentation.
#'
#' @param x A character vector of GlycoCT strings. NA values are allowed and will be returned as NA structures.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
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
parse_glycoct <- function(x, on_failure = "error") {
  struc_parser_wrapper(x, do_parse_glycoct, on_failure = on_failure)
}

do_parse_glycoct <- function(x) {
  # Split the input into lines
  lines <- stringr::str_split(x, "\n")[[1]]
  lines <- stringr::str_trim(lines)
  lines <- lines[lines != ""]

  sections <- parse_glycoct_sections(lines)
  expanded <- expand_glycoct_repeats(sections)

  residues <- expanded$residues
  linkages <- expanded$linkages

  # Build the graph
  build_glycoct_graph(residues, linkages)
}

#' Split a GlycoCT input into main and repeated-unit sections.
#'
#' @param lines A character vector of trimmed GlycoCT lines.
#'
#' @return A list with `main` and `repeats` section data.
#'
#' @noRd
parse_glycoct_sections <- function(lines) {
  rep_start <- match("REP", lines)
  main_lines <- if (is.na(rep_start)) {
    lines
  } else {
    lines[seq_len(rep_start - 1)]
  }

  sections <- list(
    main = parse_glycoct_res_lin_sections(main_lines),
    repeats = list()
  )

  if (is.na(rep_start)) {
    return(sections)
  }

  i <- rep_start
  while (i <= length(lines)) {
    if (lines[[i]] != "REP") {
      cli::cli_abort("Expected REP section in GlycoCT string")
    }
    i <- i + 1

    header_lines <- character(0)
    while (i <= length(lines) && lines[[i]] != "RES") {
      header_lines <- c(header_lines, lines[[i]])
      i <- i + 1
    }

    if (length(header_lines) == 0) {
      cli::cli_abort("No REP header found in GlycoCT string")
    }

    block_start <- i
    while (i <= length(lines) && lines[[i]] != "REP") {
      i <- i + 1
    }
    block_lines <- lines[block_start:(i - 1)]

    sections$repeats <- append(
      sections$repeats,
      list(list(
        header = parse_glycoct_rep_header(header_lines[[1]]),
        sections = parse_glycoct_res_lin_sections(block_lines)
      ))
    )
  }

  sections
}

#' Extract RES and LIN lines from one GlycoCT section block.
#'
#' @param lines A character vector containing one `RES` block and optionally one
#'   `LIN` block.
#'
#' @return A list with `res_lines` and `lin_lines`.
#'
#' @noRd
parse_glycoct_res_lin_sections <- function(lines) {
  res_start <- match("RES", lines)
  lin_start <- match("LIN", lines)

  if (is.na(res_start)) {
    cli::cli_abort("No RES section found in GlycoCT string")
  }

  res_end <- if (is.na(lin_start)) {
    length(lines)
  } else {
    lin_start - 1
  }

  res_lines <- if (res_start < res_end) {
    lines[(res_start + 1):res_end]
  } else {
    character(0)
  }

  lin_lines <- if (!is.na(lin_start) && lin_start < length(lines)) {
    lines[(lin_start + 1):length(lines)]
  } else {
    character(0)
  }

  list(res_lines = res_lines, lin_lines = lin_lines)
}

#' Parse a GlycoCT repeated-unit header.
#'
#' @param header_line A GlycoCT REP header such as
#'   `"REP1:13o(4+1)12d=7-7"`.
#'
#' @return A list describing the repeat id, boundary linkage, and count.
#'
#' @noRd
parse_glycoct_rep_header <- function(header_line) {
  pattern <- paste0(
    "^REP(\\d+):(\\d+)([a-z]?)\\(",
    "(-?\\d+(?:\\|\\d+)*)\\+(-?\\d+(?:\\|\\d+)*)",
    "\\)(\\d+)([a-z]?)=(\\d+)-(\\d+)$"
  )
  matches <- stringr::str_match(header_line, pattern)

  if (is.na(matches[[1]])) {
    cli::cli_abort("Invalid REP header in GlycoCT string: {.val {header_line}}")
  }

  min_count <- as.integer(matches[[9]])
  max_count <- as.integer(matches[[10]])
  if (!identical(min_count, max_count)) {
    cli::cli_abort(
      "Variable GlycoCT repeat ranges are not supported: {.val {header_line}}"
    )
  }
  if (min_count < 1) {
    cli::cli_abort(
      "GlycoCT repeated units must have a positive repeat count: {.val {header_line}}"
    )
  }

  list(
    rep_id = as.integer(matches[[2]]),
    from_res = as.integer(matches[[3]]),
    from_pos = matches[[5]],
    to_res = as.integer(matches[[7]]),
    to_pos = matches[[6]],
    count = min_count
  )
}

#' Expand fixed GlycoCT repeated units into ordinary residues and linkages.
#'
#' @param sections A parsed GlycoCT section list from
#'   `parse_glycoct_sections()`.
#'
#' @return A list with parsed `residues` and `linkages`.
#'
#' @noRd
expand_glycoct_repeats <- function(sections) {
  residues <- parse_res_section(sections$main$res_lines)
  linkages <- parse_lin_section(sections$main$lin_lines)

  if (length(sections$repeats) == 0) {
    return(list(residues = residues, linkages = linkages))
  }

  placeholders <- parse_glycoct_repeat_placeholders(sections$main$res_lines)
  if (length(placeholders) == 0) {
    cli::cli_abort("No repeated-unit placeholder found in GlycoCT string")
  }

  for (repeat_block in sections$repeats) {
    placeholder <- find_glycoct_repeat_placeholder(
      placeholders,
      repeat_block$header$rep_id
    )
    if (is.null(placeholder)) {
      cli::cli_abort(
        "No placeholder found for GlycoCT repeated unit {.val REP{repeat_block$header$rep_id}}"
      )
    }

    expanded <- expand_one_glycoct_repeat(
      residues,
      linkages,
      placeholder,
      repeat_block
    )
    residues <- expanded$residues
    linkages <- expanded$linkages
  }

  list(residues = residues, linkages = linkages)
}

#' Parse repeated-unit placeholder residues from a GlycoCT RES section.
#'
#' @param res_lines A character vector of RES section lines.
#'
#' @return A list of placeholder descriptors.
#'
#' @noRd
parse_glycoct_repeat_placeholders <- function(res_lines) {
  placeholders <- list()
  pattern <- "^(\\d+)r:r(\\d+)$"

  for (line in res_lines) {
    matches <- stringr::str_match(line, pattern)
    if (!is.na(matches[[1]])) {
      placeholders <- append(
        placeholders,
        list(list(
          id = as.integer(matches[[2]]),
          rep_id = as.integer(matches[[3]])
        ))
      )
    }
  }

  placeholders
}

#' Find the placeholder for a repeated-unit id.
#'
#' @param placeholders A list from `parse_glycoct_repeat_placeholders()`.
#' @param rep_id The numeric repeated-unit id.
#'
#' @return A placeholder descriptor or `NULL`.
#'
#' @noRd
find_glycoct_repeat_placeholder <- function(placeholders, rep_id) {
  for (placeholder in placeholders) {
    if (identical(placeholder$rep_id, rep_id)) {
      return(placeholder)
    }
  }

  NULL
}

#' Expand one fixed GlycoCT repeated-unit block.
#'
#' @param residues A parsed residue list.
#' @param linkages A parsed linkage list.
#' @param placeholder The placeholder descriptor for this repeat.
#' @param repeat_block Parsed repeated-unit section data.
#'
#' @return Updated `residues` and `linkages`.
#'
#' @noRd
expand_one_glycoct_repeat <- function(
  residues,
  linkages,
  placeholder,
  repeat_block
) {
  repeat_residues <- parse_res_section(repeat_block$sections$res_lines)
  repeat_linkages <- parse_lin_section(repeat_block$sections$lin_lines)
  header <- repeat_block$header

  if (is.null(repeat_residues[[as.character(header$to_res)]])) {
    cli::cli_abort("REP start residue is missing from GlycoCT repeat block")
  }
  if (is.null(repeat_residues[[as.character(header$from_res)]])) {
    cli::cli_abort("REP end residue is missing from GlycoCT repeat block")
  }

  boundary_linkages <- split_glycoct_repeat_boundary_linkages(
    linkages,
    placeholder$id
  )
  copy_maps <- build_glycoct_repeat_copies(
    residues,
    repeat_residues,
    header$count
  )

  residues <- copy_maps$residues
  linkages <- boundary_linkages$kept

  for (copy_map in copy_maps$maps) {
    linkages <- append(
      linkages,
      map_glycoct_linkages(repeat_linkages, copy_map)
    )
  }

  if (length(copy_maps$maps) > 1) {
    for (i in seq_len(length(copy_maps$maps) - 1)) {
      linkages <- append(
        linkages,
        list(list(
          from_res = unname(copy_maps$maps[[i]][[as.character(header$from_res)]]),
          from_pos = header$from_pos,
          to_res = unname(copy_maps$maps[[i + 1]][[as.character(header$to_res)]]),
          to_pos = header$to_pos
        ))
      )
    }
  }

  first_start <- unname(copy_maps$maps[[1]][[as.character(header$to_res)]])
  last_end <- unname(
    copy_maps$maps[[length(copy_maps$maps)]][[as.character(header$from_res)]]
  )

  for (linkage in boundary_linkages$incoming) {
    linkage$to_res <- first_start
    linkages <- append(linkages, list(linkage))
  }
  for (linkage in boundary_linkages$outgoing) {
    linkage$from_res <- last_end
    linkages <- append(linkages, list(linkage))
  }

  list(residues = residues, linkages = linkages)
}

#' Split linkages that enter or leave a repeated-unit placeholder.
#'
#' @param linkages A list of parsed GlycoCT linkages.
#' @param placeholder_id The placeholder residue id.
#'
#' @return A list with kept, incoming, and outgoing linkages.
#'
#' @noRd
split_glycoct_repeat_boundary_linkages <- function(linkages, placeholder_id) {
  kept <- list()
  incoming <- list()
  outgoing <- list()

  for (linkage in linkages) {
    if (linkage$to_res == placeholder_id) {
      incoming <- append(incoming, list(linkage))
    } else if (linkage$from_res == placeholder_id) {
      outgoing <- append(outgoing, list(linkage))
    } else {
      kept <- append(kept, list(linkage))
    }
  }

  list(kept = kept, incoming = incoming, outgoing = outgoing)
}

#' Create residue copies for a repeated unit.
#'
#' @param residues The existing parsed residue list.
#' @param repeat_residues The repeated-unit residue list.
#' @param count The fixed repeat count.
#'
#' @return A list with updated residues and old-to-new id maps.
#'
#' @noRd
build_glycoct_repeat_copies <- function(residues, repeat_residues, count) {
  maps <- list()
  next_id <- max(as.integer(c(names(residues), names(repeat_residues)))) + 1L
  repeat_ids <- as.integer(names(repeat_residues))

  for (copy in seq_len(count)) {
    copy_map <- stats::setNames(integer(length(repeat_ids)), repeat_ids)

    for (old_id in repeat_ids) {
      new_id <- next_id
      next_id <- next_id + 1L
      residues[[as.character(new_id)]] <- repeat_residues[[as.character(old_id)]]
      copy_map[[as.character(old_id)]] <- new_id
    }

    maps <- append(maps, list(copy_map))
  }

  list(residues = residues, maps = maps)
}

#' Map repeated-unit linkages through an old-to-new residue id map.
#'
#' @param linkages A list of parsed GlycoCT linkages.
#' @param copy_map A named integer vector mapping original ids to copied ids.
#'
#' @return A list of mapped linkages.
#'
#' @noRd
map_glycoct_linkages <- function(linkages, copy_map) {
  mapped <- list()

  for (linkage in linkages) {
    linkage$from_res <- unname(copy_map[[as.character(linkage$from_res)]])
    linkage$to_res <- unname(copy_map[[as.character(linkage$to_res)]])
    mapped <- append(mapped, list(linkage))
  }

  mapped
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

    if (!is.na(type) && type == "b") {
      # Monosaccharide
      anomer <- stringr::str_extract(content, "^[abx]")
      mono_info <- stringr::str_remove(content, "^[abx]-")

      residues[[as.character(id)]] <- list(
        type = "mono",
        anomer = anomer,
        content = mono_info,
        substituents = list()
      )
    } else if (!is.na(type) && type == "s") {
      # Substituent
      residues[[as.character(id)]] <- list(
        type = "sub",
        content = content
      )
    }
  }

  residues
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
    pattern <- "(\\d+)([don]?)\\((-?\\d+(?:\\|\\d+)*)\\+(-?\\d+(?:\\|\\d+)*)\\)(\\d+)([dn]?)"
    matches <- stringr::str_match(link_info, pattern)

    if (!is.na(matches[1])) {
      from_res <- as.integer(matches[2])
      from_pos <- matches[4]
      to_res <- as.integer(matches[6])
      to_pos <- matches[5]

      linkages <- append(
        linkages,
        list(list(
          from_res = from_res,
          from_pos = from_pos,
          to_res = to_res,
          to_pos = to_pos
        ))
      )
    }
  }

  linkages
}

build_glycoct_graph <- function(residues, linkages) {
  # Load monosaccharide mappings
  mono_mappings <- load_mono_mappings()

  # Consolidate monosaccharides with their substituents
  consolidated <- consolidate_residues(residues, linkages, mono_mappings)

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

      # Build linkage string: anomer + to_pos + "-" + from_pos
      to_vertex <- consolidated$vertices[[to_idx]]

      # Handle unknown anomer and positions
      anomer_char <- if (to_vertex$anomer == "x") "?" else to_vertex$anomer
      to_pos_str <- if (edge$to_pos == "-1") "?" else as.character(edge$to_pos)
      from_pos_str <- if (edge$from_pos == "-1") {
        "?"
      } else {
        as.character(edge$from_pos)
      }
      if (stringr::str_detect(from_pos_str, stringr::fixed("|"))) {
        from_pos_str <- stringr::str_replace_all(
          from_pos_str,
          stringr::fixed("|"),
          stringr::fixed("/")
        )
      }

      linkage_str <- paste0(anomer_char, to_pos_str, "-", from_pos_str)
      edge_attrs$linkage <- c(edge_attrs$linkage, linkage_str)
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
    # Get the anomer position for this monosaccharide
    anomer_pos <- decide_anomer_pos(reducing_end$mono)
    # Combine anomer configuration with position
    anomer_config <- stringr::str_extract(reducing_end$anomer, "^[abx]")
    if (is.na(anomer_config)) {
      anomer_config <- "?"
    }
    # Handle unknown anomer configuration
    if (anomer_config == "x") {
      anomer_config <- "?"
    }
    g$anomer <- paste0(anomer_config, anomer_pos)
  } else {
    g$anomer <- "?1"
  }
  g$alditol <- FALSE

  g
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

  return(GLYCOCT_MAP)
}

consolidate_residues <- function(residues, linkages, mono_mappings) {
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
            anomer = res$anomer
          ))
        )
      }
    } else {
      # Composite structure - try to match with known patterns
      matched <- match_composite_structure(
        group,
        residues,
        linkages,
        mono_mappings
      )

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
              mono = matched,
              sub = "",
              anomer = main_mono$anomer
            ))
          )
        } else {
          # No exact match - try partial matching with extra substituents
          partial_result <- match_partial_composite_structure(
            group,
            residues,
            linkages,
            mono_mappings
          )
          vertices <- append(
            vertices,
            list(list(
              original_id = main_mono_id,
              mono = partial_result$mono,
              sub = partial_result$sub,
              anomer = main_mono$anomer
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
  group,
  residues,
  linkages,
  mono_mappings
) {
  # Try to match the composite structure with known monosaccharides
  for (mono_name in names(mono_mappings)) {
    mapping <- mono_mappings[[mono_name]]
    if (matches_glycoct_pattern(group, residues, linkages, mapping)) {
      return(mono_name)
    }
  }

  return(NULL)
}

match_partial_composite_structure <- function(
  group,
  residues,
  linkages,
  mono_mappings
) {
  # Try to find the best partial match and identify extra substituents
  best_match <- NULL
  best_extra_subs <- c()

  # Get all substituents in this group
  group_subs <- c()
  group_mono <- NULL

  for (id in group) {
    res <- residues[[as.character(id)]]
    if (!is.null(res)) {
      if (res$type == "mono") {
        group_mono <- res
      } else if (res$type == "sub") {
        group_subs <- c(group_subs, res$content)
      }
    }
  }

  # Try to find a known composite that uses a subset of our substituents
  for (mono_name in names(mono_mappings)) {
    mapping <- mono_mappings[[mono_name]]

    # Get the pattern's substituents
    pattern_subs <- c()
    for (res_line in mapping$res) {
      if (stringr::str_detect(res_line, "^\\d+s:")) {
        sub_content <- stringr::str_remove(res_line, "^\\d+s:")
        pattern_subs <- c(pattern_subs, sub_content)
      }
    }

    # Check if pattern substituents are a subset of group substituents
    if (length(pattern_subs) > 0 && all(pattern_subs %in% group_subs)) {
      # Get the pattern's monosaccharide
      mono_lines <- mapping$res[stringr::str_detect(mapping$res, "^\\d+b:")]
      if (length(mono_lines) > 0) {
        pattern_mono_content <- stringr::str_remove(
          mono_lines[1],
          "^\\d+b:[abx]-"
        )

        # Check if monosaccharide matches
        if (
          !is.null(group_mono) && group_mono$content == pattern_mono_content
        ) {
          # Verify that the pattern substituents actually match with correct linkages
          # by using the full pattern matching function
          if (matches_glycoct_pattern(group, residues, linkages, mapping)) {
            # Exact match - shouldn't be in partial matching function
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
                mono_name = mono_name,
                pattern_subs = pattern_subs
              )
              best_extra_subs <- extra_subs
            }
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
      base_mono <- map_single_mono(group_mono$content)
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
    if (sub_content == "sulfate") {
      formatted <- c(formatted, paste0(position, "S"))
    } else if (sub_content == "n-acetyl") {
      formatted <- c(formatted, paste0(position, "Ac"))
    } else if (sub_content == "acetyl") {
      formatted <- c(formatted, paste0(position, "Ac"))
    } else if (sub_content == "methyl") {
      formatted <- c(formatted, paste0(position, "Me"))
    } else if (sub_content == "amino") {
      formatted <- c(formatted, paste0(position, "N"))
    } else if (sub_content == "phosphate") {
      formatted <- c(formatted, paste0(position, "P"))
    } else if (sub_content == "phospho-ethanolamine") {
      formatted <- c(formatted, paste0(position, "PEtn"))
    } else if (sub_content == "diphospho-ethanolamine") {
      formatted <- c(formatted, paste0(position, "PPEtn"))
    } else {
      # Generic format - for unknown substituents, use the raw name
      formatted <- c(formatted, paste0(position, sub_content))
    }
  }

  # Join multiple substituents
  paste(formatted, collapse = ",")
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
    pattern_mono <- stringr::str_remove(mono_content, "^[abx]-")
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
    group_mono == pattern_mono

  subs_match <- length(group_subs) == length(pattern_subs) &&
    all(sort(group_subs) == sort(pattern_subs))

  linkages_match <- length(group_linkages) == length(pattern_linkages) &&
    all(sort(group_linkages) == sort(pattern_linkages))

  return(mono_match && subs_match && linkages_match)
}

map_single_mono <- function(content) {
  # Extract monosaccharide name from content like "dglc-HEX-1:5" or "lgal-HEX-1:5|6:d"

  # First try to match against known patterns from mono_glycoct.txt
  mono_mappings <- load_mono_mappings()
  for (mono_name in names(mono_mappings)) {
    mapping <- mono_mappings[[mono_name]]
    if (length(mapping$res) == 1) {
      # Single monosaccharide without substituents
      mono_line <- mapping$res[[1]]
      if (stringr::str_detect(mono_line, "^\\d+b:")) {
        pattern_content <- stringr::str_remove(mono_line, "^\\d+b:[abx]-")
        if (content == pattern_content) {
          return(mono_name)
        }
      }
    }
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
