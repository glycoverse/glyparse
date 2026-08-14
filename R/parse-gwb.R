#' Parse GlycoWorkbench Structures
#'
#' Parse GlycoWorkbench (GWB/GWS) structure strings into a
#' [glyrepr::glycan_structure()].
#'
#' @details
#' GlycoWorkbench writes glycans from the reducing end towards the
#' non-reducing ends. Residues include their anomer, configuration, and ring
#' form, for example `"--4b1D-Gal,p"`. Branches are enclosed in parentheses,
#' and the structure is followed by mass options after `$`.
#'
#' The parser normalizes the glycan tree to IUPAC-condensed notation before
#' constructing the glycan structure. GlycoWorkbench substituent nodes such as
#' `"--6S"` and `"--9Ac"` are retained as monosaccharide substituents. Mass
#' options are ignored because they are not part of the glycan graph.
#' Explicit open-chain residues (`,o`) are supported only for a reduced
#' `redEnd` root; other open-chain forms cannot be represented by `glyrepr`.
#'
#' @param x A character vector of GlycoWorkbench strings. NA values are allowed
#'   and will be returned as NA structures.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param progress Whether to show a progress bar while parsing.
#' @param drop_generic Whether to replace parsed generic glycans with `NA`. A
#'   message reports the number replaced. By default, mixing generic and
#'   concrete glycans raises an error.
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' gwb <- paste0(
#'   "freeEnd--1b1D-GlcNAc,p(--6a1L-Fuc,p)",
#'   "--4b1D-Gal,p--3a2D-NeuAc,p$MONO,Und,0,0,freeEnd"
#' )
#' parse_gwb(gwb)
#'
#' @seealso [parse_iupac_condensed()]
#'
#' @export
parse_gwb <- function(
  x,
  on_failure = "error",
  progress = FALSE,
  drop_generic = FALSE
) {
  normalized_struc_parser_wrapper(
    x,
    convert_gwb_to_condensed,
    on_failure = on_failure,
    progress = progress,
    drop_generic = drop_generic
  )
}


#' Parse one GlycoWorkbench string
#'
#' @param x A single GlycoWorkbench string.
#'
#' @return A glycan graph.
#' @noRd
do_parse_gwb <- function(x) {
  do_parse_iupac_condensed(convert_gwb_to_condensed(x))
}


#' Convert GlycoWorkbench strings to IUPAC-condensed notation
#'
#' @param x A character vector of GlycoWorkbench strings.
#'
#' @return A character vector containing IUPAC-condensed notation.
#' @noRd
convert_gwb_to_condensed <- function(x) {
  purrr::map_chr(x, convert_one_gwb_to_condensed)
}


#' Convert one GlycoWorkbench string to IUPAC-condensed notation
#'
#' @param x A single GlycoWorkbench string.
#'
#' @return A single IUPAC-condensed string.
#' @noRd
convert_one_gwb_to_condensed <- function(x) {
  fields <- stringr::str_split(x, stringr::fixed("$"), n = 2)[[1]]
  structure <- fields[[1]]
  reducing_end <- stringr::str_match(structure, "^(freeEnd|redEnd)")
  if (is.na(reducing_end[[1]])) {
    cli::cli_abort(
      "A GlycoWorkbench structure must start with a reducing-end marker."
    )
  }

  bracket_pos <- stringr::str_locate(structure, stringr::fixed("}"))[[1]]
  main_structure <- if (is.na(bracket_pos)) {
    structure
  } else {
    substr(structure, 1L, bracket_pos - 1L)
  }
  parsed <- parse_gwb_linked_subtree(
    main_structure,
    nchar(reducing_end[[1]]) + 1L
  )
  if (parsed$next_pos != nchar(main_structure) + 1L) {
    cli::cli_abort("Unexpected trailing GlycoWorkbench structure content.")
  }

  main_iupac <- format_gwb_iupac(
    parsed$node,
    root = TRUE,
    alditol = identical(reducing_end[[2]], "redEnd")
  )
  if (is.na(bracket_pos)) {
    return(main_iupac)
  }

  floating <- parse_gwb_children(structure, bracket_pos + 1L)
  if (length(floating$children) == 0L) {
    cli::cli_abort(
      "A GlycoWorkbench uncertain-antenna container cannot be empty."
    )
  }
  if (floating$next_pos != nchar(structure) + 1L) {
    cli::cli_abort("Unexpected trailing GlycoWorkbench floating content.")
  }
  floating_sizes <- purrr::map_int(
    floating$children,
    count_gwb_monosaccharides
  )
  main_nodes <- collect_gwb_iupac_nodes(parsed$node)
  main_indices <- sum(floating_sizes) + seq_along(main_nodes)
  floating_mono_count <- sum(floating_sizes > 0L)
  floating_iupac <- purrr::map_chr(
    floating$children,
    function(node) {
      parents <- filter_gwb_floating_parents(
        node,
        main_nodes,
        main_indices
      )
      needs_explicit_parents <- !identical(parents, main_indices) ||
        if (identical(node$residue$kind, "mono")) {
          floating_mono_count > 1L
        } else {
          floating_mono_count > 0L
        }
      format_gwb_floating_iupac(
        node,
        parents = parents,
        explicit_parents = needs_explicit_parents
      )
    }
  )
  paste0(paste0("{", floating_iupac, "}", collapse = ""), main_iupac)
}


#' Parse a linked GlycoWorkbench subtree
#'
#' @param x A single GlycoWorkbench structure without mass options.
#' @param pos Current 1-based parsing position.
#'
#' @return A parsed node and the next unread position.
#' @noRd
parse_gwb_linked_subtree <- function(x, pos) {
  linkage <- parse_gwb_linkage(x, pos)
  parsed <- parse_gwb_subtree(x, linkage$next_pos)
  parsed$node$linkage <- linkage$linkage
  parsed
}


#' Parse a GlycoWorkbench subtree
#'
#' @param x A single GlycoWorkbench structure without mass options.
#' @param pos Current 1-based parsing position.
#'
#' @return A parsed node and the next unread position.
#' @noRd
parse_gwb_subtree <- function(x, pos) {
  residue <- parse_gwb_residue(x, pos)
  parsed_children <- parse_gwb_children(x, residue$next_pos)

  list(
    node = list(
      residue = residue$residue,
      children = parsed_children$children
    ),
    next_pos = parsed_children$next_pos
  )
}


#' Parse the children of a GlycoWorkbench residue or bracket
#'
#' @param x A single GlycoWorkbench structure without mass options.
#' @param pos Current 1-based parsing position.
#'
#' @return Parsed child nodes and the next unread position.
#' @noRd
parse_gwb_children <- function(x, pos) {
  branch_count <- 0L
  while (identical(substr(x, pos, pos), "(")) {
    branch_count <- branch_count + 1L
    pos <- pos + 1L
  }

  children <- list()
  if (branch_count > 0L) {
    for (branch_index in seq_len(branch_count)) {
      child <- parse_gwb_linked_subtree(x, pos)
      pos <- child$next_pos
      if (!identical(substr(x, pos, pos), ")")) {
        cli::cli_abort("Unclosed GlycoWorkbench branch.")
      }
      children <- append(children, list(child$node))
      pos <- pos + 1L
    }
  }

  if (starts_with_at(x, "--", pos)) {
    child <- parse_gwb_linked_subtree(x, pos)
    children <- append(children, list(child$node))
    pos <- child$next_pos
  }

  list(
    children = children,
    next_pos = pos
  )
}


#' Parse a GlycoWorkbench linkage
#'
#' @param x A single GlycoWorkbench structure without mass options.
#' @param pos Current 1-based parsing position.
#'
#' @return A parsed linkage and the next unread position.
#' @noRd
parse_gwb_linkage <- function(x, pos) {
  remaining <- substr(x, pos, nchar(x))
  pattern <- paste0(
    "^--",
    "((?:(?:[1-9N?]/)*[1-9N?]=[1-9N?],)*",
    "(?:[1-9N?]/)*[1-9N?])"
  )
  matched <- stringr::str_match(remaining, pattern)
  if (is.na(matched[[1]])) {
    cli::cli_abort("Invalid GlycoWorkbench linkage.")
  }

  bonds <- stringr::str_split(matched[[2]], stringr::fixed(","))[[1]]
  if (length(bonds) != 1L || stringr::str_detect(bonds, stringr::fixed("="))) {
    cli::cli_abort("Multi-bond GlycoWorkbench linkages are not supported.")
  }

  parent_pos <- stringr::str_replace_all(bonds[[1]], "N", "?")
  list(
    linkage = list(parent_pos = parent_pos),
    next_pos = pos + nchar(matched[[1]])
  )
}


#' Parse a GlycoWorkbench residue
#'
#' @param x A single GlycoWorkbench structure without mass options.
#' @param pos Current 1-based parsing position.
#'
#' @return A parsed residue and the next unread position.
#' @noRd
parse_gwb_residue <- function(x, pos) {
  remaining <- substr(x, pos, nchar(x))
  boundary <- stringr::str_locate(remaining, "\\(|\\)|--")
  token_length <- if (is.na(boundary[[1]])) {
    nchar(remaining)
  } else {
    boundary[[1]] - 1L
  }
  if (token_length == 0L) {
    cli::cli_abort("Missing GlycoWorkbench residue.")
  }

  token <- substr(remaining, 1L, token_length)
  list(
    residue = parse_gwb_residue_token(token),
    next_pos = pos + token_length
  )
}


#' Parse one GlycoWorkbench residue token
#'
#' Parsed tokens are cached because real corpora repeat a small residue
#' vocabulary many times.
#'
#' @param token A complete GlycoWorkbench residue token.
#'
#' @return A parsed residue.
#' @noRd
parse_gwb_residue_token <- local({
  cache <- new.env(parent = emptyenv())
  substituent_names <- glyrepr::available_substituents()
  pattern <- paste0(
    "^([abo?][1-9N?])?",
    "([DL]-)?",
    "([A-Za-z0-9_#=.]+)",
    "(?:,([?opf]))?$"
  )

  function(token) {
    if (exists(token, envir = cache, inherits = FALSE)) {
      return(cache[[token]])
    }

    matched <- stringr::str_match(token, pattern)
    if (is.na(matched[[1]])) {
      cli::cli_abort("Invalid GlycoWorkbench residue: {.val {token}}.")
    }

    anomer <- matched[[2]]
    configuration <- stringr::str_remove(matched[[3]], stringr::fixed("-"))
    source_name <- matched[[4]]
    ring <- matched[[5]]
    substituent <- normalize_gwb_substituent(source_name, substituent_names)
    kind <- if (
      identical(source_name, "U") &&
        is.na(anomer) &&
        is.na(configuration) &&
        is.na(ring)
    ) {
      "ulosonic"
    } else if (
      !is.na(substituent) &&
        is.na(anomer) &&
        is.na(configuration) &&
        is.na(ring)
    ) {
      "substituent"
    } else if (
      identical(source_name, "m") &&
        is.na(anomer) &&
        is.na(configuration) &&
        is.na(ring)
    ) {
      "deoxy"
    } else {
      "mono"
    }

    residue <- list(
      kind = kind,
      source_name = source_name,
      mono = if (identical(kind, "mono")) {
        normalize_gwb_monosaccharide(source_name, configuration, ring)
      } else {
        NA_character_
      },
      substituent = if (identical(kind, "substituent")) {
        substituent
      } else {
        NA_character_
      },
      anomer = normalize_gwb_anomer(anomer),
      ring = ring
    )
    cache[[token]] <- residue
    residue
  }
})


#' Normalize a GlycoWorkbench substituent name
#'
#' @param source_name A GlycoWorkbench residue type.
#' @param supported The glyrepr substituent vocabulary.
#'
#' @return A glyrepr substituent name or `NA`.
#' @noRd
normalize_gwb_substituent <- function(source_name, supported) {
  aliases <- c("NS" = "S")
  substituent <- unname(aliases[source_name])
  if (is.na(substituent) && source_name %in% supported) {
    substituent <- source_name
  }
  substituent
}


#' Normalize a GlycoWorkbench monosaccharide name
#'
#' @param source_name A GlycoWorkbench residue type.
#' @param configuration An explicit D/L configuration or `NA`.
#' @param ring A GlycoWorkbench ring marker or `NA`.
#'
#' @return A glyrepr monosaccharide name.
#' @noRd
normalize_gwb_monosaccharide <- local({
  aliases <- c("NeuAc" = "Neu5Ac", "NeuGc" = "Neu5Gc")
  supported <- glyrepr::available_monosaccharides()

  function(source_name, configuration, ring) {
    mono <- unname(aliases[source_name])
    if (is.na(mono)) {
      mono <- source_name
    }

    if (identical(ring, "f")) {
      mono <- as_furanose_monosaccharide(mono)
    }
    mono <- apply_monosaccharide_configuration(mono, configuration)

    if (!mono %in% supported) {
      cli::cli_abort(
        "Unsupported GlycoWorkbench monosaccharide: {.val {source_name}}."
      )
    }
    mono
  }
})


#' Normalize a GlycoWorkbench anomer token
#'
#' @param x A GlycoWorkbench anomer token or `NA`.
#'
#' @return A two-character IUPAC anomer token or `NA`.
#' @noRd
normalize_gwb_anomer <- function(x) {
  if (is.na(x)) {
    return(NA_character_)
  }
  if (startsWith(x, "o")) {
    cli::cli_abort("Open-chain GlycoWorkbench residues are not supported.")
  }
  stringr::str_replace_all(x, "N", "?")
}


#' Format a parsed GlycoWorkbench subtree as IUPAC-condensed
#'
#' @param node A parsed GlycoWorkbench node.
#' @param root Whether the node is the reducing-end monosaccharide.
#' @param alditol Whether the reducing end is an alditol.
#'
#' @return An IUPAC-condensed subtree.
#' @noRd
format_gwb_iupac <- function(node, root = FALSE, alditol = FALSE) {
  if (!identical(node$residue$kind, "mono")) {
    cli::cli_abort("A GlycoWorkbench glycan root must be a monosaccharide.")
  }
  if (identical(node$residue$ring, "o") && !(root && alditol)) {
    cli::cli_abort(
      "A non-alditol open-chain GlycoWorkbench residue is not representable."
    )
  }

  substituent_children <- purrr::keep(
    node$children,
    ~ identical(.x$residue$kind, "substituent")
  )
  if (any(lengths(purrr::map(substituent_children, "children")) > 0L)) {
    cli::cli_abort("GlycoWorkbench substituent nodes must be terminal.")
  }
  substituents <- purrr::map_chr(
    substituent_children,
    ~ paste0(.x$linkage$parent_pos, .x$residue$substituent)
  )

  deoxy_children <- purrr::keep(
    node$children,
    ~ identical(.x$residue$kind, "deoxy")
  )
  if (length(deoxy_children) > 0L) {
    valid_deoxy <- length(deoxy_children) == 1L &&
      length(deoxy_children[[1]]$children) == 0L &&
      identical(node$residue$mono, "Hex") &&
      identical(deoxy_children[[1]]$linkage$parent_pos, "6")
    if (!valid_deoxy) {
      cli::cli_abort("Unsupported GlycoWorkbench deoxy modification.")
    }
    node$residue$mono <- "dHex"
  }

  ulosonic_children <- purrr::keep(
    node$children,
    ~ identical(.x$residue$kind, "ulosonic")
  )
  if (length(ulosonic_children) > 0L) {
    valid_ulosonic <- length(ulosonic_children) == 1L &&
      length(ulosonic_children[[1]]$children) == 0L &&
      node$residue$mono %in% c("Fru", "Kdn") &&
      identical(ulosonic_children[[1]]$linkage$parent_pos, "2")
    if (!valid_ulosonic) {
      cli::cli_abort("Unsupported GlycoWorkbench ulosonic modification.")
    }
    alditol <- alditol || identical(node$residue$mono, "Fru")
  }

  glycan_children <- purrr::discard(
    node$children,
    ~ .x$residue$kind %in% c("substituent", "deoxy", "ulosonic")
  )
  child_iupac <- purrr::map_chr(glycan_children, format_gwb_child_iupac)
  child_prefix <- if (length(child_iupac) == 0L) {
    ""
  } else if (length(child_iupac) == 1L) {
    child_iupac[[1]]
  } else {
    paste0(
      child_iupac[[1]],
      paste0("[", child_iupac[-1], "]", collapse = "")
    )
  }

  mono <- paste0(node$residue$mono, paste0(substituents, collapse = ""))
  if (!root) {
    return(paste0(child_prefix, mono))
  }

  if (alditol) {
    anomer_pos <- decide_anomer_pos(node$residue$mono)
    return(paste0(child_prefix, mono, "-ol(?", anomer_pos, "-"))
  }
  if (is.na(node$residue$anomer)) {
    return(paste0(child_prefix, mono))
  }
  paste0(child_prefix, mono, "(", node$residue$anomer, "-")
}


#' Format a parsed GlycoWorkbench child as IUPAC-condensed
#'
#' @param node A parsed GlycoWorkbench child node.
#'
#' @return An IUPAC-condensed child subtree including its linkage.
#' @noRd
format_gwb_child_iupac <- function(node) {
  if (!identical(node$residue$kind, "mono")) {
    cli::cli_abort("Unexpected GlycoWorkbench substituent placement.")
  }
  anomer <- node$residue$anomer
  if (is.na(anomer)) {
    anomer <- paste0("?", decide_anomer_pos(node$residue$mono))
  }
  paste0(
    format_gwb_iupac(node),
    "(",
    anomer,
    "-",
    node$linkage$parent_pos,
    ")"
  )
}


#' Format a GlycoWorkbench uncertain antenna as floating IUPAC
#'
#' @param node A parsed child of the GlycoWorkbench bracket container.
#' @param parents Complete-sequence indices of candidate main-tree parents.
#' @param explicit_parents Whether to serialize the candidate indices.
#'
#' @return A floating glycan or substituent token without braces.
#' @noRd
format_gwb_floating_iupac <- function(
  node,
  parents,
  explicit_parents = TRUE
) {
  parent_suffix <- if (explicit_parents) {
    paste0("|", paste0(parents, collapse = ","))
  } else {
    ""
  }
  if (identical(node$residue$kind, "substituent")) {
    if (length(node$children) > 0L) {
      cli::cli_abort("GlycoWorkbench substituent nodes must be terminal.")
    }
    return(paste0(
      node$linkage$parent_pos,
      node$residue$substituent,
      parent_suffix
    ))
  }
  if (identical(node$residue$kind, "deoxy")) {
    cli::cli_abort(
      "A GlycoWorkbench deoxy modification must have a parent monosaccharide."
    )
  }
  if (identical(node$residue$kind, "ulosonic")) {
    cli::cli_abort(
      "A GlycoWorkbench ulosonic modification must have a parent monosaccharide."
    )
  }
  paste0(format_gwb_child_iupac(node), parent_suffix)
}


#' Filter candidate main-tree parents for a floating GWB node
#'
#' @param node A parsed floating node.
#' @param main_nodes Main-tree nodes in IUPAC source order.
#' @param main_indices Complete-sequence source indices for `main_nodes`.
#'
#' @return Feasible main-tree parent indices.
#' @noRd
filter_gwb_floating_parents <- function(node, main_nodes, main_indices) {
  positions <- stringr::str_split_1(
    node$linkage$parent_pos,
    stringr::fixed("/")
  )
  if (any(positions == "?")) {
    return(main_indices)
  }

  feasible <- purrr::map_lgl(main_nodes, function(main_node) {
    occupied <- gwb_node_occupied_positions(main_node)
    any(!positions %in% occupied)
  })
  parents <- main_indices[feasible]
  if (length(parents) == 0L) {
    cli::cli_abort(
      "No feasible main-tree parent remains for a GlycoWorkbench uncertain antenna."
    )
  }
  parents
}


#' Collect parsed GWB nodes in IUPAC source order
#'
#' @param node A parsed GlycoWorkbench node.
#'
#' @return A list of monosaccharide nodes in left-to-right IUPAC order.
#' @noRd
collect_gwb_iupac_nodes <- function(node) {
  if (!identical(node$residue$kind, "mono")) {
    return(list())
  }
  glycan_children <- purrr::keep(
    node$children,
    ~ identical(.x$residue$kind, "mono")
  )
  c(
    unlist(
      purrr::map(glycan_children, collect_gwb_iupac_nodes),
      recursive = FALSE
    ),
    list(node)
  )
}


#' Find definitely occupied positions on one parsed GWB node
#'
#' @param node A parsed GlycoWorkbench monosaccharide node.
#'
#' @return Character positions known to be occupied.
#' @noRd
gwb_node_occupied_positions <- function(node) {
  positions <- purrr::map_chr(node$children, ~ .x$linkage$parent_pos)
  positions <- stringr::str_split(positions, stringr::fixed("/"))
  definite <- lengths(positions) == 1L &
    purrr::map_chr(positions, 1L) != "?"
  purrr::map_chr(positions[definite], 1L)
}


#' Count monosaccharides in a parsed GlycoWorkbench subtree
#'
#' @param node A parsed GlycoWorkbench node.
#'
#' @return The number of monosaccharides in the subtree.
#' @noRd
count_gwb_monosaccharides <- function(node) {
  if (!identical(node$residue$kind, "mono")) {
    return(0L)
  }
  1L + sum(purrr::map_int(node$children, count_gwb_monosaccharides))
}


#' Test whether a string starts with a token at a position
#'
#' @param x A character scalar.
#' @param token A token to match.
#' @param pos A 1-based position.
#'
#' @return A logical scalar.
#' @noRd
starts_with_at <- function(x, token, pos) {
  identical(substr(x, pos, pos + nchar(token) - 1L), token)
}
