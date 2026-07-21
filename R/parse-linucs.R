#' Parse LINUCS Structures
#'
#' Parse LINUCS strings into a [glyrepr::glycan_structure()].
#' LINUCS is a tree-oriented glycan format that writes each residue as a
#' linkage token, a residue token, and a braced child list, for example
#' `"[][Hexp]{[(4+1)][Hexp]{}}"`.
#'
#' @details
#' LINUCS linkages are written as `"(parent+child)"`, where `parent` is the
#' linkage position on the parent residue and `child` is the anomeric linkage
#' position on the child residue. Residue labels are normalized to the
#' monosaccharide and substituent vocabulary used by [glyrepr].
#'
#' @param x A character vector of LINUCS strings. NA values are allowed and
#'   will be returned as NA structures.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param progress Whether to show a progress bar while parsing.
#' @param validate Whether to validate parsed glycan graphs before constructing
#'   the result.
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' linucs <- "[][b-D-Glcp]{[(4+1)][b-D-Galp]{}}"
#' parse_linucs(linucs)
#'
#' @export
parse_linucs <- function(
  x,
  on_failure = "error",
  progress = FALSE,
  validate = TRUE
) {
  struc_parser_wrapper(
    x,
    do_parse_linucs,
    on_failure = on_failure,
    progress = progress,
    validate = validate
  )
}


#' Parse one LINUCS string
#'
#' @param x A single LINUCS string.
#'
#' @return A glycan graph.
#' @noRd
do_parse_linucs <- function(x) {
  record <- parse_linucs_record(x)
  build_linucs_graph(record)
}


#' Detect whether a string looks like LINUCS
#'
#' @param x A character scalar.
#'
#' @return A logical scalar.
#' @noRd
is_linucs_string <- function(x) {
  stringr::str_detect(x, "^\\[\\]\\[[^\\]]+\\]\\{")
}


#' Parse a complete LINUCS record
#'
#' @param x A single LINUCS string.
#'
#' @return A parsed LINUCS node.
#' @noRd
parse_linucs_record <- function(x) {
  parsed <- parse_linucs_node(x, pos = 1)
  if (parsed$next_pos != nchar(x) + 1) {
    cli::cli_abort("Unexpected trailing LINUCS content.")
  }
  if (!is.na(parsed$node$linkage$parent_pos)) {
    cli::cli_abort("The LINUCS root must have an empty linkage token.")
  }
  if (!identical(parsed$node$residue$kind, "mono")) {
    cli::cli_abort("The LINUCS root must be a monosaccharide.")
  }
  parsed$node
}


#' Parse one LINUCS node
#'
#' @param x A single LINUCS string.
#' @param pos Current 1-based parsing position.
#'
#' @return A list with `node` and `next_pos`.
#' @noRd
parse_linucs_node <- function(x, pos) {
  link <- parse_linucs_bracket_token(x, pos)
  residue <- parse_linucs_bracket_token(x, link$next_pos)
  pos <- residue$next_pos

  if (!identical(substr(x, pos, pos), "{")) {
    cli::cli_abort("Expected a LINUCS child-list opening brace.")
  }
  pos <- pos + 1

  children <- list()
  while (pos <= nchar(x) && !identical(substr(x, pos, pos), "}")) {
    child <- parse_linucs_node(x, pos)
    children <- append(children, list(child$node))
    pos <- child$next_pos
  }

  if (pos > nchar(x)) {
    cli::cli_abort("Unclosed LINUCS child list.")
  }

  list(
    node = list(
      linkage = parse_linucs_linkage(link$token),
      residue = parse_linucs_residue(residue$token),
      children = children
    ),
    next_pos = pos + 1
  )
}


#' Parse one square-bracket LINUCS token
#'
#' @param x A single LINUCS string.
#' @param pos Current 1-based parsing position.
#'
#' @return A list with `token` and `next_pos`.
#' @noRd
parse_linucs_bracket_token <- function(x, pos) {
  if (!identical(substr(x, pos, pos), "[")) {
    cli::cli_abort("Expected a LINUCS square-bracket token.")
  }

  remaining <- stringr::str_sub(x, pos)
  close_pos <- stringr::str_locate(remaining, stringr::fixed("]"))[1, 1]
  if (is.na(close_pos)) {
    cli::cli_abort("Unclosed LINUCS square-bracket token.")
  }

  token_start <- pos + 1
  token_end <- pos + close_pos - 2
  token <- if (token_start <= token_end) {
    substr(x, token_start, token_end)
  } else {
    ""
  }

  list(token = token, next_pos = pos + close_pos)
}


#' Parse a LINUCS linkage token
#'
#' @param x A linkage token without square brackets.
#'
#' @return A list with parent and child linkage positions.
#' @noRd
parse_linucs_linkage <- function(x) {
  if (identical(x, "")) {
    return(list(parent_pos = NA_character_, child_pos = NA_character_))
  }

  match <- stringr::str_match(x, "^\\((\\d+|\\?)\\+(\\d+|\\?)\\)$")
  if (is.na(match[1, 1])) {
    cli::cli_abort("Invalid LINUCS linkage token: {.val {x}}")
  }

  list(parent_pos = match[1, 2], child_pos = match[1, 3])
}


#' Parse a LINUCS residue token
#'
#' @param x A residue token without square brackets.
#'
#' @return A list describing either a monosaccharide or substituent.
#' @noRd
parse_linucs_residue <- function(x) {
  if (is_implicit_linucs_residue_token(x)) {
    return(list(kind = "implicit_sub"))
  }

  substituent <- map_linucs_substituent_token(x)
  if (!is.na(substituent)) {
    return(list(kind = "sub", sub = substituent))
  }

  anomer <- parse_linucs_anomer(x)
  normalized <- normalize_linucs_residue_label(anomer$label)
  stem_match <- match_linucs_mono_stem(normalized)
  mono <- unname(linucs_mono_stem_map()[[stem_match$stem]])
  sub <- parse_linucs_residue_substituents(
    stem_match$suffix,
    mono = mono
  )

  list(
    kind = "mono",
    mono = mono,
    anomer = paste0(anomer$value, decide_anomer_pos(mono)),
    anomer_value = anomer$value,
    sub = sub
  )
}


#' Check whether a standalone LINUCS token is implicit in the parent residue
#'
#' @param x A residue token.
#'
#' @return A logical scalar.
#' @noRd
is_implicit_linucs_residue_token <- function(x) {
  identical(x, "1(R)-carboxyethoxy")
}


#' Parse an optional LINUCS residue anomer prefix
#'
#' @param x A residue token.
#'
#' @return A list with anomer value and remaining label.
#' @noRd
parse_linucs_anomer <- function(x) {
  match <- stringr::str_match(x, "^([ab\\?])-?(.*)$")
  if (!is.na(match[1, 1])) {
    return(list(value = match[1, 2], label = match[1, 3]))
  }

  embedded_match <- stringr::str_match(x, "^(6-deoxy-)([ab\\?])-?(.*)$")
  if (!is.na(embedded_match[1, 1])) {
    return(
      list(
        value = embedded_match[1, 3],
        label = paste0(embedded_match[1, 2], embedded_match[1, 4])
      )
    )
  }

  list(value = "?", label = x)
}


#' Normalize LINUCS residue label prefixes
#'
#' @param x A residue label after removing anomer notation.
#'
#' @return A label ready for stem matching.
#' @noRd
normalize_linucs_residue_label <- function(x) {
  x |>
    stringr::str_remove("^(?:keto|aldehydo)-") |>
    stringr::str_remove("-onic$") |>
    stringr::str_replace("^LDMan", "LDman") |>
    stringr::str_replace("^DDMan", "DDman") |>
    stringr::str_remove("-ol$")
}


#' Match a normalized LINUCS monosaccharide stem
#'
#' @param x A normalized residue label.
#'
#' @return A list with matched `stem` and remaining `suffix`.
#' @noRd
match_linucs_mono_stem <- function(x) {
  stems <- names(linucs_mono_stem_map())
  stems <- stems[order(nchar(stems), decreasing = TRUE)]
  stem <- purrr::detect(
    stems,
    ~ stringr::str_starts(x, stringr::fixed(.x))
  )

  if (is.null(stem)) {
    cli::cli_abort("Unknown LINUCS residue token: {.val {x}}")
  }

  list(
    stem = stem,
    suffix = stringr::str_sub(x, nchar(stem) + 1)
  )
}


#' Map LINUCS monosaccharide stems to glyrepr monosaccharide names
#'
#' @return A named character vector.
#' @noRd
linucs_mono_stem_map <- function() {
  c(
    `6-deoxy-HexpNAc` = "dHexNAc",
    `6-deoxy-HexNAc` = "dHexNAc",
    HexpNAc = "HexNAc",
    HexNAc = "HexNAc",
    HexpN = "HexN",
    HexN = "HexN",
    HexpA = "HexA",
    HexA = "HexA",
    `6-deoxy-Hexp` = "dHex",
    `6-deoxy-Hexf` = "dHex",
    `6-deoxy-Hex` = "dHex",
    `6-deoxy-L-AltpNAc` = "6dAltNAc",
    `6-deoxy-D-TalpNAc` = "6dTalNAc",
    `6-deoxy-L-Altp` = "6dAlt",
    `6-deoxy-D-Gulp` = "6dGul",
    `6-deoxy-D-Talp` = "6dTal",
    Hexp = "Hex",
    Hexf = "Hex",
    Hex = "Hex",
    Penp = "Pen",
    Penf = "Pen",
    Pen = "Pen",
    `D-GlcpNAc` = "GlcNAc",
    `D-ManpNAc` = "ManNAc",
    `D-GalpNAc` = "GalNAc",
    `D-GulpNAc` = "GulNAc",
    `L-AltpNAc` = "AltNAc",
    `D-AllpNAc` = "AllNAc",
    `D-TalpNAc` = "TalNAc",
    `L-IdopNAc` = "IdoNAc",
    `D-GlcpN` = "GlcN",
    `D-ManpN` = "ManN",
    `D-GalpN` = "GalN",
    `D-GulpN` = "GulN",
    `L-AltpN` = "AltN",
    `D-AllpN` = "AllN",
    `D-TalpN` = "TalN",
    `L-IdopN` = "IdoN",
    `D-GlcpA` = "GlcA",
    `D-ManpA` = "ManA",
    `D-GalpA` = "GalA",
    `D-GulpA` = "GulA",
    `L-AltpA` = "AltA",
    `D-AllpA` = "AllA",
    `D-TalpA` = "TalA",
    `L-IdopA` = "IdoA",
    `D-Glcp` = "Glc",
    `D-Glc` = "Glc",
    `D-Manp` = "Man",
    `D-Galp` = "Gal",
    `D-Gulp` = "Gul",
    `L-Altp` = "Alt",
    `D-Allp` = "All",
    `D-Talp` = "Tal",
    `L-Idop` = "Ido",
    `L-FucpNAc` = "FucNAc",
    `D-QuipNAc` = "QuiNAc",
    `L-RhapNAc` = "RhaNAc",
    `L-Fucp` = "Fuc",
    `D-Fucp` = "dHex",
    `D-Quip` = "Qui",
    `L-Rhap` = "Rha",
    `D-Olip` = "Oli",
    `D-Tyvp` = "Tyv",
    `D-Abep` = "Abe",
    `D-Parp` = "Par",
    `D-Digp` = "Dig",
    `L-Colp` = "Col",
    `L-Arap` = "Ara",
    `L-Araf` = "Ara",
    `L-Lyxp` = "Lyx",
    `L-Lyxf` = "Lyx",
    `D-Xylp` = "Xyl",
    `D-Xylf` = "Xyl",
    `D-Ribp` = "Rib",
    `D-Ribf` = "Rib",
    Neup5Ac = "Neu5Ac",
    NeupAc = "Neu5Ac",
    Neup5Gc = "Neu5Gc",
    NeupGc = "Neu5Gc",
    Neup = "Neu",
    Kdnp = "Kdn",
    `D-Kdop` = "Kdo",
    `D-Dhap` = "Dha",
    Psep = "Pse",
    Legp = "Leg",
    Acip = "Aci",
    `4eLegp` = "4eLeg",
    Bacp = "Bac",
    `D-Bacp` = "Bac",
    LDmanHepp = "LDmanHep",
    DDmanHepp = "DDmanHep",
    MurpNAc = "MurNAc",
    MurpNGc = "MurNGc",
    Murp = "Mur",
    `D-Apif` = "Api",
    `D-Apip` = "Api",
    `D-Frup` = "Fru",
    `D-Tagp` = "Tag",
    `L-Sorp` = "Sor",
    `D-Psip` = "Psi"
  )
}


#' Parse LINUCS residue suffix substituents
#'
#' @param x Residue suffix after the monosaccharide stem.
#' @param mono Parsed glyrepr monosaccharide name.
#'
#' @return A comma-separated substituent string.
#' @noRd
parse_linucs_residue_substituents <- function(x, mono) {
  if (identical(x, "")) {
    return("")
  }

  substituents <- character()
  if (mono %in% linucs_n_sulfate_monos() && stringr::str_starts(x, "S")) {
    substituents <- c(substituents, "2S")
    x <- stringr::str_sub(x, 2)
  }
  if (stringr::str_starts(x, "NAc")) {
    substituents <- c(substituents, "2NAc")
    x <- stringr::str_sub(x, 4)
  } else if (stringr::str_starts(x, "NGc")) {
    substituents <- c(substituents, "2NGc")
    x <- stringr::str_sub(x, 4)
  } else if (stringr::str_starts(x, "N")) {
    substituents <- c(substituents, "2N")
    x <- stringr::str_sub(x, 2)
  }

  sub_names <- glyrepr::available_substituents()
  sub_names <- sub_names[order(nchar(sub_names), decreasing = TRUE)]
  pattern <- stringr::str_glue(
    "^(\\d+|\\?)({stringr::str_c(sub_names, collapse = '|')})"
  )

  while (!identical(x, "")) {
    match <- stringr::str_match(x, pattern)
    if (is.na(match[1, 1])) {
      cli::cli_abort(
        "Unsupported LINUCS residue substituent suffix: {.val {x}}"
      )
    }
    substituents <- c(substituents, paste0(match[1, 2], match[1, 3]))
    x <- stringr::str_sub(x, nchar(match[1, 1]) + 1)
  }

  paste(substituents, collapse = ",")
}


#' Return monosaccharides whose LINUCS `NS` suffix means N-sulfate
#'
#' @return A character vector of glyrepr monosaccharide names.
#' @noRd
linucs_n_sulfate_monos <- function() {
  c(
    "HexN",
    "GlcN",
    "ManN",
    "GalN",
    "GulN",
    "AltN",
    "AllN",
    "TalN",
    "IdoN"
  )
}


#' Map standalone LINUCS substituent tokens
#'
#' @param x A residue token.
#'
#' @return A glyrepr substituent name, or `NA_character_`.
#' @noRd
map_linucs_substituent_token <- function(x) {
  substituent_map <- c(
    S = "S",
    SO3H = "S",
    P = "P",
    Me = "Me",
    Ac = "Ac",
    NAc = "NAc",
    NGc = "NGc"
  )

  if (!x %in% names(substituent_map)) {
    return(NA_character_)
  }
  unname(substituent_map[[x]])
}


#' Build a glycan graph from a parsed LINUCS tree
#'
#' @param root A parsed LINUCS root node.
#'
#' @return An igraph glycan graph.
#' @noRd
build_linucs_graph <- function(root) {
  state <- new.env(parent = emptyenv())
  state$vertices <- list()
  state$edges <- list()

  add_linucs_mono_node(root, state)
  vertex_df <- data.frame(
    name = purrr::map_chr(state$vertices, "name"),
    mono = purrr::map_chr(state$vertices, "mono"),
    sub = purrr::map_chr(state$vertices, "sub"),
    stringsAsFactors = FALSE
  )
  edge_df <- if (length(state$edges) == 0) {
    data.frame(
      from = integer(),
      to = integer(),
      linkage = character()
    )
  } else {
    purrr::list_rbind(purrr::map(state$edges, data.frame))
  }

  graph <- igraph::graph_from_data_frame(edge_df, vertices = vertex_df)
  graph$anomer <- root$residue$anomer
  graph$alditol <- FALSE
  graph
}


#' Add one LINUCS monosaccharide node to graph state
#'
#' @param node A parsed LINUCS node.
#' @param state Mutable graph-building state.
#'
#' @return The numeric vertex id.
#' @noRd
add_linucs_mono_node <- function(node, state) {
  if (!identical(node$residue$kind, "mono")) {
    cli::cli_abort("Expected a LINUCS monosaccharide node.")
  }

  node_id <- length(state$vertices) + 1
  state$vertices[[node_id]] <- list(
    name = as.character(node_id),
    mono = node$residue$mono,
    sub = node$residue$sub
  )

  purrr::walk(node$children, function(child) {
    handle_linucs_child_node(child, parent_id = node_id, state = state)
  })

  node_id
}


#' Handle a child of a LINUCS monosaccharide node
#'
#' @param child A parsed LINUCS child node.
#' @param parent_id Parent vertex id.
#' @param state Mutable graph-building state.
#'
#' @return `NULL`, invisibly.
#' @noRd
handle_linucs_child_node <- function(child, parent_id, state) {
  validate_linucs_child_linkage(child$linkage)

  if (identical(child$residue$kind, "sub")) {
    if (length(child$children) > 0) {
      cli::cli_abort("LINUCS substituent nodes cannot have child residues.")
    }
    add_linucs_vertex_substituent(
      state,
      vertex_id = parent_id,
      sub = paste0(child$linkage$parent_pos, child$residue$sub)
    )
    return(invisible(NULL))
  }
  if (identical(child$residue$kind, "implicit_sub")) {
    if (length(child$children) > 0) {
      cli::cli_abort("Implicit LINUCS substituent nodes cannot have children.")
    }
    return(invisible(NULL))
  }

  child_id <- add_linucs_mono_node(child, state)
  state$edges[[length(state$edges) + 1]] <- list(
    from = parent_id,
    to = child_id,
    linkage = paste0(
      child$residue$anomer_value,
      child$linkage$child_pos,
      "-",
      child$linkage$parent_pos
    )
  )
  invisible(NULL)
}


#' Validate a non-root LINUCS linkage
#'
#' @param linkage A parsed LINUCS linkage.
#'
#' @return `NULL`, invisibly.
#' @noRd
validate_linucs_child_linkage <- function(linkage) {
  if (is.na(linkage$parent_pos) || is.na(linkage$child_pos)) {
    cli::cli_abort("LINUCS child nodes must have a linkage token.")
  }
  invisible(NULL)
}


#' Add a substituent to a graph-state vertex
#'
#' @param state Mutable graph-building state.
#' @param vertex_id Numeric vertex id.
#' @param sub Substituent token with position.
#'
#' @return `NULL`, invisibly.
#' @noRd
add_linucs_vertex_substituent <- function(state, vertex_id, sub) {
  old_sub <- state$vertices[[vertex_id]]$sub
  state$vertices[[vertex_id]]$sub <- if (identical(old_sub, "")) {
    sub
  } else {
    paste(old_sub, sub, sep = ",")
  }
  invisible(NULL)
}
