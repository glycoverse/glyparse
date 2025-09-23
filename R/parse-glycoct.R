#' Parse GlycoCT Structures
#'
#' This function parses GlycoCT strings into a [glyrepr::glycan_structure()].
#' GlycoCT is a format used by databases like GlyTouCan and GlyGen.
#'
#' @details
#' GlycoCT format consists of two parts:
#' - RES: Contains monosaccharides (lines starting with 'b:') and substituents (lines starting with 's:')
#' - LIN: Contains linkage information between residues
#'
#' For more information about GlycoCT format, see the glycoct.md documentation.
#'
#' @param x A character vector of GlycoCT strings.
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
parse_glycoct <- function(x) {
  struc_parser_wrapper(x, do_parse_glycoct)
}

do_parse_glycoct <- function(x) {
  # Split the input into lines
  lines <- stringr::str_split(x, "\n")[[1]]
  lines <- stringr::str_trim(lines)
  lines <- lines[lines != ""]
  
  # Find RES and LIN sections
  res_start <- which(lines == "RES")
  lin_start <- which(lines == "LIN")
  
  if (length(res_start) == 0) {
    cli::cli_abort("No RES section found in GlycoCT string")
  }
  
  # Parse RES section
  if (length(lin_start) == 0) {
    res_lines <- lines[(res_start + 1):length(lines)]
  } else {
    res_lines <- lines[(res_start + 1):(lin_start - 1)]
  }
  
  # Parse LIN section (if exists)
  if (length(lin_start) > 0) {
    lin_lines <- lines[(lin_start + 1):length(lines)]
  } else {
    lin_lines <- character(0)
  }
  
  # Parse residues (monosaccharides and substituents)
  residues <- parse_res_section(res_lines)
  
  # Parse linkages
  linkages <- parse_lin_section(lin_lines)
  
  # Build the graph
  build_glycoct_graph(residues, linkages)
}

parse_res_section <- function(res_lines) {
  residues <- list()
  
  for (line in res_lines) {
    # Parse line format: "1b:a-dgal-HEX-1:5"
    parts <- stringr::str_split(line, ":")[[1]]
    if (length(parts) < 2) next
    
    id <- as.integer(stringr::str_extract(parts[1], "\\d+"))
    type <- stringr::str_extract(parts[1], "[bs]$")
    # Rejoin content in case there are multiple colons
    content <- paste(parts[2:length(parts)], collapse = ":")
    
    if (type == "b") {
      # Monosaccharide
      anomer <- stringr::str_extract(content, "^[abx]")
      mono_info <- stringr::str_remove(content, "^[abx]-")
      
      residues[[as.character(id)]] <- list(
        type = "mono",
        anomer = anomer,
        content = mono_info,
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

parse_lin_section <- function(lin_lines) {
  linkages <- list()
  
  for (line in lin_lines) {
    # Parse line format: "1:1d(2+1)2n"
    parts <- stringr::str_split(line, ":")[[1]]
    if (length(parts) < 2) next
    
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
      
      linkages[[link_id]] <- list(
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
    from_idx <- which(sapply(consolidated$vertices, function(v) v$original_id == edge$from_res))
    to_idx <- which(sapply(consolidated$vertices, function(v) v$original_id == edge$to_res))
    
    if (length(from_idx) == 1 && length(to_idx) == 1) {
      edges <- c(edges, from_idx, to_idx)
      
      # Build linkage string: anomer + to_pos + "-" + from_pos
      to_vertex <- consolidated$vertices[[to_idx]]
      
      # Handle unknown anomer and positions
      anomer_char <- if (to_vertex$anomer == "x") "?" else to_vertex$anomer
      to_pos_str <- if (edge$to_pos == "-1") "?" else as.character(edge$to_pos)
      from_pos_str <- if (edge$from_pos == "-1") "?" else as.character(edge$from_pos)
      if (stringr::str_detect(from_pos_str, stringr::fixed("|"))) {
        from_pos_str <- stringr::str_replace_all(from_pos_str, stringr::fixed("|"), stringr::fixed("/"))
      }
      
      linkage_str <- paste0(anomer_char, to_pos_str, "-", from_pos_str)
      edge_attrs$linkage <- c(edge_attrs$linkage, linkage_str)
    }
  }
  
  # Create igraph
  if (length(edges) == 0) {
    # Single monosaccharide
    g <- igraph::make_empty_graph(n = length(consolidated$vertices), directed = TRUE)
    # Add empty linkage attribute for validation
    g <- igraph::set_edge_attr(g, "linkage", value = character(0))
  } else {
    g <- igraph::make_graph(edges, n = length(consolidated$vertices), directed = TRUE)
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
    if (is.na(anomer_config)) anomer_config <- "?"
    # Handle unknown anomer configuration
    if (anomer_config == "x") anomer_config <- "?"
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
      res = c("1b:a-dgro-dgal-NON-2:6|1:a|2:keto|3:d|9:d", "2s:amino", "3s:amino"),
      lin = c("1:1d(5+1)2n", "2:1d(7+1)3n")
    ),
    "Aci" = list(
      res = c("1b:a-lgro-lalt-NON-2:6|1:a|2:keto|3:d|9:d", "2s:amino", "3s:amino"),
      lin = c("1:1d(5+1)2n", "2:1d(7+1)3n")
    ),
    "4eLeg" = list(
      res = c("1b:a-dgro-dtal-NON-2:6|1:a|2:keto|3:d|9:d", "2s:amino", "3s:amino"),
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
        vertices <- append(vertices, list(list(
          original_id = group[1],
          mono = mono_name,
          sub = "",
          anomer = res$anomer
        )))
      }
    } else {
      # Composite structure - try to match with known patterns
      matched <- match_composite_structure(group, residues, linkages, mono_mappings)
      
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
          vertices <- append(vertices, list(list(
            original_id = main_mono_id,
            mono = matched,
            sub = "",
            anomer = main_mono$anomer
          )))
        } else {
          # No exact match - try partial matching with extra substituents
          partial_result <- match_partial_composite_structure(group, residues, linkages, mono_mappings)
          vertices <- append(vertices, list(list(
            original_id = main_mono_id,
            mono = partial_result$mono,
            sub = partial_result$sub,
            anomer = main_mono$anomer
          )))
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
    
    if (!is.null(from_res) && !is.null(to_res) && 
        from_res$type == "mono" && to_res$type == "sub") {
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

match_composite_structure <- function(group, residues, linkages, mono_mappings) {
  # Try to match the composite structure with known monosaccharides
  for (mono_name in names(mono_mappings)) {
    mapping <- mono_mappings[[mono_name]]
    if (matches_glycoct_pattern(group, residues, linkages, mapping)) {
      return(mono_name)
    }
  }
  
  return(NULL)
}

match_partial_composite_structure <- function(group, residues, linkages, mono_mappings) {
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
        pattern_mono_content <- stringr::str_remove(mono_lines[1], "^\\d+b:[abx]-")
        
        # Check if monosaccharide matches
        if (!is.null(group_mono) && group_mono$content == pattern_mono_content) {
          # Verify that the pattern substituents actually match with correct linkages
          # by using the full pattern matching function
          if (matches_glycoct_pattern(group, residues, linkages, mapping)) {
            # Exact match - shouldn't be in partial matching function
            next
          }
          
          # For partial matching, only consider if there are extra substituents
          if (length(pattern_subs) < length(group_subs)) {
            extra_subs <- setdiff(group_subs, pattern_subs)
            
            if (is.null(best_match) || length(pattern_subs) > length(best_match$pattern_subs)) {
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
    formatted_extra_subs <- format_extra_substituents(best_extra_subs, group, residues, linkages)
    
    return(list(
      mono = best_match$mono_name,
      sub = formatted_extra_subs
    ))
  } else {
    # No partial match found, return base monosaccharide with all substituents
    if (!is.null(group_mono)) {
      base_mono <- map_single_mono(group_mono$content)
      formatted_subs <- format_extra_substituents(group_subs, group, residues, linkages)
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
        if (!is.null(to_res) && to_res$type == "sub" && to_res$content == sub_content) {
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
      if (!is.null(from_res) && !is.null(to_res) &&
          from_res$type == "mono" && to_res$type == "sub") {
        group_linkages <- c(group_linkages, paste0(linkage$from_pos, "+", linkage$to_pos))
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
       link_pattern <- stringr::str_match(lin_line, "\\d+:(\\d+)[do]?\\((\\d+)\\+(\\d+)\\)(\\d+)[dn]?")
       if (!is.na(link_pattern[1])) {
         from_pos <- link_pattern[3]
         to_pos <- link_pattern[4]
         pattern_linkages <- c(pattern_linkages, paste0(from_pos, "+", to_pos))
       }
     }
   }
  
  # Compare structures - exact match for monosaccharide content
  mono_match <- !is.null(group_mono) && !is.null(pattern_mono) && 
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
    if (stringr::str_detect(stereo_name, "dglc")) return("Glc")
    if (stringr::str_detect(stereo_name, "dgal")) return("Gal") 
    if (stringr::str_detect(stereo_name, "dman")) return("Man")
    if (stringr::str_detect(stereo_name, "dxyl")) return("Xyl")
    if (stringr::str_detect(stereo_name, "lgal.*6:d")) return("Fuc")  # Fucose pattern
    
    # Basic patterns without D/L prefix
    if (stringr::str_detect(stereo_name, "glc")) return("Glc")
    if (stringr::str_detect(stereo_name, "gal")) return("Gal") 
    if (stringr::str_detect(stereo_name, "man")) return("Man")
    if (stringr::str_detect(stereo_name, "xyl")) return("Xyl")
    
    # Handle GlcA (glucuronic acid) pattern
    if (stringr::str_detect(content, "dglc.*6:a")) return("GlcA")
    
    # Fallback to class name
    if (class_name == "HEX") return("Hex")
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
