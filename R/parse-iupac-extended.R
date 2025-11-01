#' Parse IUPAC-extended Structures
#'
#' Parse IUPAC-extended-style structure characters into a [glyrepr::glycan_structure()].
#' For more information about IUPAC-extended format, see \doi{10.1351/pac199668101919}.
#'
#' @param x A character vector of IUPAC-extended strings.
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' iupac <- "\u03b2-D-Galp-(1\u21923)-\u03b1-D-GalpNAc-(1\u2192"
#' parse_iupac_extended(iupac)
#'
#' @seealso [parse_iupac_condensed()], [parse_iupac_short()]
#'
#' @export
parse_iupac_extended <- function(x) {
  struc_parser_wrapper(x, do_parse_iupac_extended)
}


do_parse_iupac_extended <- function(x) {
  do_parse_iupac_condensed(convert_ext_to_con(x))
}


IUPAC_EXT_TO_CON <- local({
  monos <- glyrepr::available_monosaccharides(mono_type = "all")
  monos <- setdiff(monos, "Sia")
  # Deal with general rules
  ext_monos <- dplyr::case_when(
    stringr::str_detect(monos, "NAc$") ~ stringr::str_replace(monos, "NAc$", "pNAc"),
    stringr::str_detect(monos, "N$") ~ stringr::str_replace(monos, "N$", "pN"),
    stringr::str_detect(monos, "A$") ~ stringr::str_replace(monos, "A$", "pA"),
    .default = paste0(monos, "p")
  )
  names(ext_monos) <- monos
  # Deal with special cases
  ext_monos["Neu5Ac"] <- "Neup5Ac"
  ext_monos["Neu5Gc"] <- "Neup5Gc"
  ext_monos["MurNGc"] <- "MurpN2Gc"
  ext_monos["Api"] <- "Eryp3CMeOH"
  # LDmanHep and DDmanHep are specially treated in the parser
  ext_monos["LDmanHep"] <- "LDmanHep"
  ext_monos["DDmanHep"] <- "DDmanHep"

  rlang::set_names(names(ext_monos), ext_monos)
})

# Regex pattern of a IUPAC-extended residue:
# - "-?([\\u03b1\\u03b2\\\\?])": anomer (α, β, or ?), with optional leading "-". Group 1.
# - "-[DL\\?]-": configuration (D or L, or ?), with leading and trailing "-".
# - "([:alnum:]+?)": monosaccharide name, with non-greedy matching. Group 2.
# - "-\\((\\d+(?:/\\d+)*|\\?)\\u2192(?:(\\d+(?:/\\d+)*|\\?)\\))?": position information, e.g. -(1→2). \\u2192 is →.
#   Leading "-". Group 3 is the first position, and group 4 is the second position.
RESIDUE_PATTERN <- "-?([\\u03b1\\u03b2\\?])-[DL\\?]-([[:alnum:]\\?]+?)-\\((\\d+(?:/\\d+)*|\\?)\\u2192(?:(\\d+(?:/\\d+)*|\\?)\\))?"

# Regex pattern of a IUPAC-extended DDmanHep or LDmanHep residue:
# "D-gro-α-D-manHepp-(1→" for "DDmanHep(a1-"
# "L-gro-α-D-manHepp-(1→" for "LDmanHep(a1-"
DMANHEP_PATTERN <- "-?[DL]-gro-[\\u03b1\\u03b2\\?]-D-manHepp(?:[[:alnum:]\\?]+?)?-\\((\\d+(?:/\\d+)*|\\?)\\u2192(?:(\\d+(?:/\\d+)*|\\?)\\))?"


convert_ext_to_con <- function(x) {
  # Convert IUPAC-extended format to IUPAC-condensed format
  token_pattern <- paste(RESIDUE_PATTERN, DMANHEP_PATTERN, "\\[", "\\]", sep = "|")
  tokens <- stringr::str_extract_all(x, token_pattern)[[1]]
  new_tokens <- purrr::map_chr(tokens, convert_token)
  paste(new_tokens, collapse = "")
}


convert_token <- function(token) {
  # Convert a single token (either a residue or a bracket).
  if (token %in% c("[", "]")) {
    return(token)
  }

  # Special case: LDmanHep and DDmanHep
  # Here we convert the residue to meet `RESIDUE_PATTERN`,
  # by removing [DL]-gro and adding converting manHepp to [DL]DmanHep.
  if (stringr::str_detect(token, DMANHEP_PATTERN)) {
    maphep_dl <- stringr::str_extract(token, "([DL])-gro", group = 1)
    token <- stringr::str_replace(token, "([DL])-gro", "")
    token <- stringr::str_replace(token, "manHepp", paste0(maphep_dl, "DmanHep"))
  }

  # General case
  anomer <- stringr::str_extract(token, RESIDUE_PATTERN, group = 1)
  new_anomer <- dplyr::case_match(anomer, "?" ~ "?", "\u03b1" ~ "a", "\u03b2" ~ "b")
  mono <- stringr::str_extract(token, RESIDUE_PATTERN, group = 2)
  new_mono <- convert_mono(mono)
  pos1 <- stringr::str_extract(token, RESIDUE_PATTERN, group = 3)
  # pos2 might be missing for the core monosaccharide
  # e.g. GlcNAc(b1-6)]GalNAc(a1-
  pos2 <- stringr::str_extract(token, RESIDUE_PATTERN, group = 4)
  if (is.na(pos2)) {
    stringr::str_glue("{new_mono}({new_anomer}{pos1}-")
  } else {
    stringr::str_glue("{new_mono}({new_anomer}{pos1}-{pos2})")
  }
}


convert_mono <- function(mono) {
  # One monosaccharide might have multiple matches.
  # For example, "GlcpNAc" can match "Glcp", "GlcpN", and "GlcpNAc".
  # We choose the longest match, which is the correct one.
  candidates <- IUPAC_EXT_TO_CON[
    stringr::str_detect(mono, stringr::fixed(names(IUPAC_EXT_TO_CON)))
  ]
  matched <- candidates[which.max(nchar(candidates))]
  new_mono <- stringr::str_replace(mono, stringr::fixed(names(matched)), matched)
  if (is.na(new_mono)) {
    cli::cli_abort(paste0("Unknown monosaccharide: ", mono))
  }
  new_mono
}
