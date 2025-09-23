#' Parse WURCS Structures
#'
#' This function parses WURCS strings into a [glyrepr::glycan_structure()].
#' Currently, only WURCS 2.0 is supported.
#' For more information about WURCS, see [WURCS](https://github.com/glycoinfo/WURCS/wiki).
#'
#' @param x A character vector of WURCS strings.
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' wurcs <- paste0(
#'   "WURCS=2.0/3,5,4/",
#'   "[a2122h-1b_1-5_2*NCC/3=O][a1122h-1b_1-5][a1122h-1a_1-5]/",
#'   "1-1-2-3-3/a4-b1_b4-c1_c3-d1_c6-e1"
#' )
#' parse_wurcs(wurcs)
#'
#' @export
parse_wurcs <- function(x) {
  struc_parser_wrapper(x, do_parse_wurcs)
}


WURCS_MONO_REGEX <- c(
  "Glc" = "^a2122h-1[abx]_1-5(?!_2\\*N(CC/3=O)?|_3\\*OC\\^RCO/4=O/3C)",
  # Explaination to the regex:
  # - "^": This has to be the beginning of the string.
  # - "a2122h": The WURCS code for Glc.
  # - "-1[abx]": The anomeric carbon is C1,
  #    and the anomer is either "a", "b" or "x" (unknown).
  # - "_1-5": C1 and C5 are connected by a glycosidic bond to form a ring.
  # - "(?!_2\\*N(CC/3=O)?|_3\\*OC\\^RCO/4=O/3C)":
  #   This is for excluding GlcNAc, GlcN, and Mur.
  #   The code for "NAc" is "_2*NCC/3=O", and the code for "N" is "_2*N".
  #   Therefore, "_2\\*N(CC/3=O)?" excludes both.

  # From Man to Ido, the regex is similar to Glc.
  "Man" = "^a1122h-1[abx]_1-5(?!_2\\*N(CC/3=O)?)",
  "Gal" = "^a2112h-1[abx]_1-5(?!_2\\*N(CC/3=O)?)",
  "Gul" = "^a2212h-1[abx]_1-5(?!_2\\*N(CC/3=O)?)",
  "Alt" = "^a2111h-1[abx]_1-5(?!_2\\*N(CC/3=O)?)",
  "All" = "^a2222h-1[abx]_1-5(?!_2\\*N(CC/3=O)?)",
  "Tal" = "^a1112h-1[abx]_1-5(?!_2\\*N(CC/3=O)?)",
  "Ido" = "^a2121h-1[abx]_1-5(?!_2\\*N(CC/3=O)?)",

  # From GlcNAc to IdoNAc, the regex is almost the same to the original WRUCS code.
  # For GlcNAc, we have to differentiate it from MurNAc.
  "GlcNAc" = "^a2122h-1[abx]_1-5_2\\*NCC/3=O(?!_3\\*OC\\^RCO/4=O/3C)",
  "GalNAc" = "^a2112h-1[abx]_1-5_2\\*NCC/3=O",
  "ManNAc" = "^a1122h-1[abx]_1-5_2\\*NCC/3=O",
  "GulNAc" = "^a2212h-1[abx]_1-5_2\\*NCC/3=O",
  "AltNAc" = "^a2111h-1[abx]_1-5_2\\*NCC/3=O",
  "AllNAc" = "^a2222h-1[abx]_1-5_2\\*NCC/3=O",
  "TalNAc" = "^a1112h-1[abx]_1-5_2\\*NCC/3=O",
  "IdoNAc" = "^a2121h-1[abx]_1-5_2\\*NCC/3=O",

  # From GlcN to IdoN, we have to exclude the "Ac" part.
  # For GlcN, we have to differentiate it from NurNAc.
  "GlcN" = "^a2122h-1[abx]_1-5_2\\*N(?!CCO?/3=O(_3\\*OC\\^RCO/4=O/3C)?)",
  "ManN" = "^a1122h-1[abx]_1-5_2\\*N(?!CC/3=O)",
  "GalN" = "^a2112h-1[abx]_1-5_2\\*N(?!CC/3=O)",
  "GulN" = "^a2212h-1[abx]_1-5_2\\*N(?!CC/3=O)",
  "AltN" = "^a2111h-1[abx]_1-5_2\\*N(?!CC/3=O)",
  "AllN" = "^a2222h-1[abx]_1-5_2\\*N(?!CC/3=O)",
  "TalN" = "^a1112h-1[abx]_1-5_2\\*N(?!CC/3=O)",
  "IdoN" = "^a2121h-1[abx]_1-5_2\\*N(?!CC/3=O)",

  # From GlcA to IdoA.
  "GlcA" = "^a2122A-1[abx]_1-5",
  "ManA" = "^a1122A-1[abx]_1-5",
  "GalA" = "^a2112A-1[abx]_1-5",
  "GulA" = "^a2212A-1[abx]_1-5",
  "AltA" = "^a2111A-1[abx]_1-5",
  "AllA" = "^a2222A-1[abx]_1-5",
  "TalA" = "^a1112A-1[abx]_1-5",
  "IdoA" = "^a2121A-1[abx]_1-5",

  # From Fuc to 6dTal, we have to exclude the "NAc" part.
  "Fuc" = "^a1221m-1[abx]_1-5(?!_2\\*NCC/3=O)",
  # For Qui, we have to differentiate it from Bac.
  "Qui" = "^a2122m-1[abx]_1-5(?!_2\\*NCC/3=O|_2\\*N_4\\*N)",
  "Rha" = "^a2211m-1[abx]_1-5(?!_2\\*NCC/3=O)",
  "6dGul" = "^a2212m-1[abx]_1-5(?!_2\\*NCC/3=O)",
  "6dAlt" = "^a2111m-1[abx]_1-5(?!_2\\*NCC/3=O)",
  "6dTal" = "^a1112m-1[abx]_1-5(?!_2\\*NCC/3=O)",

  # From FucNAc to 6dTalNAc.
  "FucNAc" = "^a1221m-1[abx]_1-5_2\\*NCC/3=O",
  "QuiNAc" = "^a2122m-1[abx]_1-5_2\\*NCC/3=O",
  "RhaNAc" = "^a2211m-1[abx]_1-5_2\\*NCC/3=O",
  "6dAltNAc" = "^a2111m-1[abx]_1-5_2\\*NCC/3=O",
  "6dTalNAc" = "^a1112m-1[abx]_1-5_2\\*NCC/3=O",

  # From Oli to Rib
  "Oli" = "^ad122m-1[abx]_1-5",
  "Tyv" = "^a1d22m-1[abx]_1-5",
  "Abe" = "^a2d12m-1[abx]_1-5",
  "Par" = "^a2d22m-1[abx]_1-5",
  "Dig" = "^ad222m-1[abx]_1-5",
  "Col" = "^a1d21m-1[abx]_1-5",
  "Ara" = "^a211h-1[abx]_1-5",
  "Lyx" = "^a221h-1[abx]_1-5",
  "Xyl" = "^a212h-1[abx]_1-5",
  "Rib" = "^a222h-1[abx]_1-5",

  # Neu5Ac and Neu5Gc - match if contains _5*NCC/3=O or _5*NCCO/3=O anywhere in the string
  # These must come before Kdn since they are more specific
  "Neu5Ac" = "^Aad21122h-2[abx]_2-6.*_5\\*NCC/3=O",
  "Neu5Gc" = "^Aad21122h-2[abx]_2-6.*_5\\*NCCO/3=O",

  # Kdn: exclude N, Ac, and Gc
  "Kdn" = "^Aad21122h-2[abx]_2-6(?!_5\\*N(CC(O)?/3=O)?)",

  # Neu: exclude Ac and Gc
  "Neu" = "^Aad21122h-2[abx]_2-6_5\\*N(?!CC(O)?/3=O)",

  # Rest of the monosaccharides are themselves.
  "Pse" = "^had22111m-2[abx]_2-6_5\\*N_7\\*N",
  "Leg" = "^Aad21122m-2[abx]_2-6_5\\*N_7\\*N",
  "Aci" = "^Aad21111m-2[abx]_2-6_5\\*N_7\\*N",
  "4eLeg" = "^Aad11122m-2[abx]_2-6_5\\*N_7\\*N",
  "Bac" = "^a2122m-1[abx]_1-5_2\\*N_4\\*N",
  "LDmanHep" = "^a11221h-1[abx]_1-5",
  "Kdo" = "^Aad1122h-2[abx]_2-6",
  "Dha" = "^Aad112A-2[abx]_2-6",
  "DDmanHep" = "^a11222h-1[abx]_1-5",
  "MurNAc" = "^a2122h-1[abx]_1-5_2\\*NCC/3=O_3\\*OC\\^RCO/4=O/3C",
  "MurNGc" = "^a2122h-1[abx]_1-5_2\\*NCCO/3=O_3\\*OC\\^RCO/4=O/3C",
  "Mur" = "^a2122h-1[abx]_1-5_3\\*OC\\^RCO/4=O/3C",
  "Fru" = "^ha122h-2[abx]_2-6",
  "Tag" = "^ha112h-2[abx]_2-6",
  "Sor" = "^ha121h-2[abx]_2-6",
  "Psi" = "^ha222h-2[abx]_2-6"
)


WURCS_SUB_REGEX <- c(
  "Me" = "OC",
  "Ac" = "OCC/3=O",
  "NAc" = "NCC/3=O",
  "P" = "OPO/3O/3=O",
  "S" = "OSO/3=O/3=O",
  "Pyr" = "OCCC/4=O/3=O",
  "PC" = "OP(\\^X)?OCCNC/7C/7C/3O/3=O",
  "PPEtn" = "OP(\\^X)?OP(\\^X)?OCCN/5O/5=O/3O/3=O",
  "PEtn" = "OP(\\^X)?OCCN/3O/3=O",
  "N" = "N"
)


parse_residue <- function(residue) {
  # This function accepts a WURCS residue (something in "[]"),
  # and returns a named vector of c(mono, anomer, sub)
  # `mono`: the IUPAC monosaccharide name
  # `anomer`: the anomer, e.g. "a1", "b2", "?1"
  # `sub`: the substituent, e.g. "3Me", "2Ac", "4NAc", "6P", "?P"
  #        for multiple substituents, they are separated by commas, e.g. "3Me,6S"

  # Get monosaacharide name
  mono_idx <- purrr::detect_index(WURCS_MONO_REGEX, ~ stringr::str_detect(residue, .x))
  if (mono_idx == 0) {
    cli::cli_abort("Unable to parse residue: {.str {residue}}")
  }
  mono <- names(WURCS_MONO_REGEX)[[mono_idx]]

  # Get anomeric carbon and anomer
  anomer_code <- stringr::str_extract(residue, "-(\\d+[abx])_", group = 1)
  anomer <- stringr::str_replace(anomer_code, "x", "?")
  anomer <- paste0(stringr::str_sub(anomer, 2), stringr::str_sub(anomer, 1, 1))

  # Get substituent(s)
  # For Neu5Ac and Neu5Gc, we need special handling since the 5-position NAc/NGc
  # is part of the monosaccharide itself, not an additional substituent
  if (mono %in% c("Neu5Ac", "Neu5Gc")) {
    # For Neu5Ac/Neu5Gc, remove the base Kdn structure and the characteristic 5-position modification
    base_kdn_pattern <- "^Aad21122h-2[abx]_2-6"
    if (mono == "Neu5Ac") {
      # Remove the base Kdn pattern and the 5*NCC/3=O
      sub_code <- stringr::str_remove(residue, base_kdn_pattern)
      sub_code <- stringr::str_remove(sub_code, "_5\\*NCC/3=O")
    } else { # Neu5Gc
      # Remove the base Kdn pattern and the 5*NCCO/3=O
      sub_code <- stringr::str_remove(residue, base_kdn_pattern)
      sub_code <- stringr::str_remove(sub_code, "_5\\*NCCO/3=O")
    }
  } else {
    # For other monosaccharides, use the standard approach
    sub_code <- stringr::str_remove(residue, WURCS_MONO_REGEX[[mono_idx]])
  }
  
  if (sub_code == "") {
    sub <- ""
  } else {
    # Split multiple substituents by "_" and process each one
    sub_parts <- stringr::str_split_1(sub_code, "_")
    # Remove empty strings (from leading "_")
    sub_parts <- sub_parts[sub_parts != ""]
    
    # Process each substituent part
    substituents <- purrr::map_chr(sub_parts, function(sub_part) {
      # Add back the leading "_" for pattern matching
      sub_part_with_underscore <- paste0("_", sub_part)
      
      sub_patterns <- stringr::str_glue("^_(\\d+|\\?)\\*{WURCS_SUB_REGEX}$")
      sub_idx <- purrr::detect_index(sub_patterns, ~ stringr::str_detect(sub_part_with_underscore, .x))
      if (sub_idx == 0) {
        cli::cli_abort("Unable to parse substituent: {.str {sub_part_with_underscore}}")
      }
      sub_name <- names(WURCS_SUB_REGEX)[[sub_idx]]
      sub_pos <- stringr::str_extract(sub_part_with_underscore, "_(\\d+|\\?)", group = 1)
      paste0(sub_pos, sub_name)
    })
    
    # Join multiple substituents with commas
    sub <- paste(substituents, collapse = ",")
  }

  c(mono = mono, anomer = anomer, sub = sub)
}


parse_unique_residues <- function(x) {
  # Input: a string of WURCS unique residues part
  # Output: a list of named vectors, each vector contains `mono`, `anomer`, and `sub`
  residues <- stringr::str_extract_all(x, "\\[.*?\\]")[[1]]
  residues <- stringr::str_sub(residues, 2, -2)
  purrr::map(residues, parse_residue)
}


parse_residue_sequence <- function(x) {
  # Input: a string of WURCS residue sequence part
  # Output: a vector of monosaccharide names
  as.integer(stringr::str_split_1(x, "-"))
}


parse_linkages <- function(x) {
  # Input: a string of WURCS linkages part
  # Output: a list of named lists, each list contains `from`, `to`, and `linkage`
  # `from` and `to` are the indices of monosaccharides in the sequence
  # `linkage` is the linkage strings without anomer, e.g. "1-2", "1-3"
  linkages <- stringr::str_split_1(x, "_")
  purrr::map(linkages, parse_one_linkage)
}


parse_one_linkage <- function(x) {
  # Input: a string of one WURCS linkage, e.g. "a4-b1"
  # Output: a named list of `from`, `to`, and `linkage`
  spl <- stringr::str_split_1(x, "-")

  handle_parallel_pos <- function(part) {
    if (stringr::str_detect(part, "|")) {
      parts <- stringr::str_split_1(part, stringr::fixed("|"))
      pos <- stringr::str_sub(parts, 2, -1)
      idx_part <- stringr::str_sub(part, 1, 1)
      pos_part <- stringr::str_c(pos, collapse = "/")
      stringr::str_c(idx_part, pos_part)
    } else {
      part
    }
  }

  from_part <- handle_parallel_pos(spl[[1]])
  to_part <- handle_parallel_pos(spl[[2]])
  if (stringr::str_detect(to_part, stringr::fixed("/"))) {
    # WURCS has a strange linkage rule:
    # In the normal case, the linkage positions are opposite to the orders of IUPAC.
    # For example, "a4-b1" means "1-4" in IUPAC.
    # However, when second position is a parallel position, e.g. "b2|b2",
    # the linkage positions are the same as the orders of IUPAC.
    # For example, "f2-g3|g6" means "2-3/6" in IUPAC.
    # In this case, we need to swap from_part and to_part.
    temp <- from_part
    from_part <- to_part
    to_part <- temp
  }

  from_idx <- letter_to_int(stringr::str_sub(from_part, 1, 1))
  to_idx <- letter_to_int(stringr::str_sub(to_part, 1, 1))
  linkage <- paste0(
    stringr::str_sub(to_part, 2, -1),
    "-",
    stringr::str_sub(from_part, 2, -1)
  )
  list(from = from_idx, to = to_idx, linkage = linkage)
}

letter_to_int <- function(letter) {
  utf8ToInt(letter) - utf8ToInt("a") + 1
}


prepare_graph_dfs <- function(residues, linkages) {
  # Generate edgelist dataframe and vertex dataframe.
  # `edgelist_df`: "from", "to", "linkage".
  # `vertex_df`: "name", "mono", "anomer", "sub".
  # Note that the "anomer" column is not need in a `glycan_graph` object.
  vertex_df <- purrr::list_rbind(purrr::map(residues, ~ data.frame(as.list(.x))))
  if (is.null(linkages)) {
    edgelist_df <- data.frame(from = integer(), to = integer(), linkage = character())
  } else {
    edgelist_df <- purrr::list_rbind(purrr::map(linkages, data.frame))
    # Add anomer to "linkage" column in `edgelist_df`.
    edgelist_df$linkage <- stringr::str_c(
      stringr::str_sub(vertex_df$anomer[edgelist_df$to], 1, 1),
      edgelist_df$linkage
    )
  }
  vertex_df$name <- rownames(vertex_df)
  list(edgelist = edgelist_df, vertex = vertex_df)
}


build_glycan_graph <- function(edgelist_df, vertex_df) {
  # For format of input values, see `prepare_graph_dfs`.
  graph <- igraph::graph_from_data_frame(
    edgelist_df,
    vertices = vertex_df[c("name", "mono", "sub")]
  )
  core_node <- igraph::V(graph)[igraph::degree(graph, mode = "in") == 0]
  core_anomer <- vertex_df$anomer[as.numeric(core_node)]
  graph$anomer <- core_anomer
  graph$alditol <- FALSE  # Not implemented yet
  graph
}


do_parse_wurcs <- function(x) {
  wurcs_regex <- stringr::regex("
    ^WURCS=2\\.0         # WURCS version
    /\\d+,\\d+,\\d+      # unique residue count, residue count, linkage count
    /((?:\\[.*?\\])+)    # unique residues
    /((?:\\d+-)*\\d+)    # residue sequence
    (?:/(.*))?           # linkages, omitted for one residue sequence
    ", comments = TRUE)
  # Here we assume all characters after "residue sequence" are valid linkages.

  if (!stringr::str_detect(x, wurcs_regex)) {
    cli::cli_abort("Invalid WURCS string: {.str {x}}")
  }

  # unique_residues: a list of named character vectors,
  # each vector contains `mono` ("GlcNAc"), `anomer` ("b1"), and `sub` ("3Me")
  unique_residue_part <- stringr::str_extract(x, wurcs_regex, group = 1)
  unique_residues <- parse_unique_residues(x)

  # residue_sequence: an integer vector of monosaccharide indices,
  # referring to the order of unique_residues, repeated monosaccharides allowed.
  # e.g. c(1, 1, 2, 3, 3)
  residue_sequence_part <- stringr::str_extract(x, wurcs_regex, group = 2)
  residue_sequence <- parse_residue_sequence(residue_sequence_part)

  # linkages: a list of named lists, each list contains `from`, `to`, and `linkage`.
  # `from` and `to` are the indices of monosaccharides in the sequence.
  # `linkage` is the linkage strings without anomer, e.g. "1-2", "1-3"
  linkage_part <- stringr::str_extract(x, wurcs_regex, group = 3)
  if (linkage_part == "") {
    linkages <- NULL
  } else {
    linkages <- parse_linkages(linkage_part)
  }

  residues <- unique_residues[residue_sequence]
  graph_dfs <- prepare_graph_dfs(residues, linkages)
  graph <- build_glycan_graph(graph_dfs$edgelist, graph_dfs$vertex)
  graph
}