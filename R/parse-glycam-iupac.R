#' Parse GlyCAM IUPAC Structures
#'
#' Parse GlyCAM IUPAC-style structure strings into a
#' [glyrepr::glycan_structure()].
#'
#' @details
#' GlyCAM IUPAC is similar to IUPAC-condensed notation, but monosaccharides
#' include configuration and ring markers such as `"DGlcp"` and `"LFucp"`,
#' terminal reducing-end residues end in `"-OH"`, and residue modifiers are
#' written in brackets, such as `"DGalp[6S]b1-4"`.
#'
#' The parser normalizes GlyCAM IUPAC into IUPAC-condensed notation, then uses
#' the IUPAC-condensed parser to construct the glycan structure. Explicit
#' reducing-end moieties, such as `"-OH"` or `"-OME"`, are normalized to the
#' regular reducing-end IUPAC-condensed form because glyrepr does not represent
#' the terminal moiety separately.
#'
#' @param x A character vector of GlyCAM IUPAC strings. NA values are allowed
#'   and will be returned as NA structures.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param progress Whether to show a progress bar while parsing.
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' glycam <- "DManpa1-3[DManpa1-6]DManpb1-4DGlcpNAcb1-OH"
#' parse_glycam_iupac(glycam)
#'
#' @seealso [parse_iupac_condensed()]
#'
#' @export
parse_glycam_iupac <- function(x, on_failure = "error", progress = FALSE) {
  struc_parser_wrapper(
    x,
    do_parse_glycam_iupac,
    on_failure = on_failure,
    progress = progress
  )
}


#' Parse one GlyCAM IUPAC string
#'
#' @param x A single GlyCAM IUPAC string.
#'
#' @return A glycan graph.
#' @noRd
do_parse_glycam_iupac <- function(x) {
  do_parse_iupac_condensed(convert_glycam_iupac_to_condensed(x))
}


#' Convert GlyCAM IUPAC to IUPAC-condensed notation
#'
#' @param x A single GlyCAM IUPAC string.
#'
#' @return A character scalar containing IUPAC-condensed notation.
#' @noRd
convert_glycam_iupac_to_condensed <- function(x) {
  token_pattern <- paste(
    glycam_iupac_residue_pattern(),
    "\\[",
    "\\]",
    sep = "|"
  )
  tokens <- stringr::str_extract_all(x, token_pattern)[[1]]

  if (length(tokens) == 0 || paste0(tokens, collapse = "") != x) {
    cli::cli_abort("Failed to parse the GlyCAM IUPAC string.")
  }

  paste0(purrr::map_chr(tokens, convert_glycam_iupac_token), collapse = "")
}


#' Convert one GlyCAM IUPAC token to IUPAC-condensed notation
#'
#' @param token A GlyCAM residue token or branch bracket.
#'
#' @return A character scalar containing the converted token.
#' @noRd
convert_glycam_iupac_token <- function(token) {
  if (token %in% c("[", "]")) {
    return(token)
  }

  residue_pattern <- paste0("^", glycam_iupac_residue_pattern(), "$")
  residue_match <- stringr::str_match(token, residue_pattern)
  if (is.na(residue_match[1, 1])) {
    cli::cli_abort("Failed to parse GlyCAM IUPAC residue: {.val {token}}")
  }

  mono <- convert_glycam_iupac_mono(residue_match[1, 2])
  modifiers <- convert_glycam_iupac_modifiers(
    stringr::str_extract_all(
      token,
      glycam_iupac_modifier_pattern()
    )[[1]]
  )
  anomer <- residue_match[1, 3]
  anomer_pos <- residue_match[1, 4]
  linked_pos <- residue_match[1, 5]

  if (is_glycam_iupac_reducing_end_moiety(linked_pos)) {
    return(stringr::str_glue("{mono}{modifiers}({anomer}{anomer_pos}-"))
  }

  stringr::str_glue("{mono}{modifiers}({anomer}{anomer_pos}-{linked_pos})")
}


#' Create the GlyCAM IUPAC residue regex
#'
#' @return A regex pattern for one GlyCAM IUPAC residue.
#' @noRd
glycam_iupac_residue_pattern <- function() {
  paste0(
    "([A-Za-z0-9]+?)",
    "(?:",
    glycam_iupac_modifier_pattern(),
    ")*",
    "([ab\\?])",
    "([0-9\\?])-",
    "([0-9\\?]|[A-Za-z][A-Za-z0-9]*)"
  )
}


#' Create the GlyCAM IUPAC modifier regex
#'
#' @return A regex pattern for one bracketed GlyCAM modifier group.
#' @noRd
glycam_iupac_modifier_pattern <- function() {
  "\\[[0-9\\?][A-Za-z]+(?:,[0-9\\?][A-Za-z]+)*\\]"
}


#' Check whether a GlyCAM token suffix is a reducing-end moiety
#'
#' @param x A residue suffix captured after the linkage dash.
#'
#' @return A logical scalar.
#' @noRd
is_glycam_iupac_reducing_end_moiety <- function(x) {
  isTRUE(stringr::str_detect(x, "^[A-Za-z][A-Za-z0-9]*$"))
}


#' Convert a GlyCAM monosaccharide name to a glyrepr monosaccharide name
#'
#' @param mono A GlyCAM monosaccharide label, such as `"DGlcpNAc"`.
#'
#' @return A glyrepr monosaccharide name.
#' @noRd
convert_glycam_iupac_mono <- function(mono) {
  mono_map <- glycam_iupac_mono_map()

  converted <- unname(mono_map[[mono]])
  if (is.null(converted)) {
    cli::cli_abort("Unknown GlyCAM IUPAC monosaccharide: {.val {mono}}")
  }

  converted
}


#' Map GlyCAM monosaccharide labels to glyrepr monosaccharide labels
#'
#' @return A named character vector mapping GlyCAM labels to glyrepr labels.
#' @noRd
glycam_iupac_mono_map <- function() {
  c(
    Hexp = "Hex",
    DFucp = "Fuc",
    DGalp = "Gal",
    DGalpA = "GalA",
    DGalpN = "GalN",
    DGalpNAc = "GalNAc",
    DGlcp = "Glc",
    DGlcpA = "GlcA",
    DGlcpN = "GlcN",
    DGlcpNAc = "GlcNAc",
    DGulp = "Gul",
    DGulpA = "GulA",
    DGulpN = "GulN",
    DGulpNAc = "GulNAc",
    DManp = "Man",
    DManpA = "ManA",
    DManpN = "ManN",
    DManpNAc = "ManNAc",
    DAllp = "All",
    DAllpA = "AllA",
    DAllpN = "AllN",
    DAllpNAc = "AllNAc",
    DTalp = "Tal",
    DTalpA = "TalA",
    DTalpN = "TalN",
    DTalpNAc = "TalNAc",
    LAltp = "Alt",
    LAltpA = "AltA",
    LAltpN = "AltN",
    LAltpNAc = "AltNAc",
    LIdop = "Ido",
    LIdopA = "IdoA",
    LIdopN = "IdoN",
    LIdopNAc = "IdoNAc",
    DQuip = "Qui",
    DQuipNAc = "QuiNAc",
    D6dGulp = "6dGul",
    D6dTalp = "6dTal",
    L6dAltp = "6dAlt",
    L6dAltpNAc = "6dAltNAc",
    D6dTalpNAc = "6dTalNAc",
    LFucp = "Fuc",
    LFucpNAc = "FucNAc",
    LRhap = "Rha",
    LRhapNAc = "RhaNAc",
    DOlip = "Oli",
    DTyvp = "Tyv",
    DAbep = "Abe",
    DParp = "Par",
    DDigp = "Dig",
    LColp = "Col",
    LArap = "Ara",
    LLyxp = "Lyx",
    DRibp = "Rib",
    DXylf = "Xyl",
    DXylp = "Xyl",
    DNeup5Ac = "Neu5Ac",
    DNeup5Gc = "Neu5Gc",
    Neup = "Neu",
    Neup5Ac = "Neu5Ac",
    Neup5Gc = "Neu5Gc",
    DKDNp = "Kdn",
    Kdnp = "Kdn",
    Psep = "Pse",
    Legp = "Leg",
    Acip = "Aci",
    `4eLegp` = "4eLeg",
    DBacp = "Bac",
    LDmanHepp = "LDmanHep",
    DKdop = "Kdo",
    DDhap = "Dha",
    DDmanHepp = "DDmanHep",
    DMurp = "Mur",
    DMurpNAc = "MurNAc",
    DMurpNGc = "MurNGc",
    DApif = "Api",
    DApip = "Api",
    DFrup = "Fru",
    DTagp = "Tag",
    LSorp = "Sor",
    DPsip = "Psi"
  )
}


#' Convert GlyCAM residue modifiers to IUPAC-condensed suffixes
#'
#' @param modifiers A character vector of GlyCAM modifier tokens, including
#'   brackets.
#'
#' @return A character scalar with concatenated IUPAC-condensed modifiers.
#' @noRd
convert_glycam_iupac_modifiers <- function(modifiers) {
  if (length(modifiers) == 0) {
    return("")
  }

  converted <- modifiers |>
    stringr::str_remove_all("\\[|\\]") |>
    stringr::str_split(",") |>
    unlist(use.names = FALSE) |>
    stringr::str_replace("A$", "Ac")

  paste0(converted, collapse = "")
}
