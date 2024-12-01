#' Parse IUPAC-short Structures
#'
#' Parse IUPAC-short-style structure characters into glycan graphs.
#' For more information about IUPAC-short format, see
#' https://doi.org/10.1351/pac199668101919.
#'
#' @param x A character vector of IUPAC-short strings.
#' @param mode A character string, either "ne" or "dn". Default is "ne".
#' For more information about "ne" and "dn", see [glyrepr::as_glycan_graph()].
#'
#' @return A glycan graph if `x` is a single character,
#' or a list of glycan graphs if `x` is a character vector.
#'
#' @examples
#' iupac <- "Neu5Aca3Gala3(Fuca3)GlcNAcb-"
#' parse_iupac_short(iupac)
#'
#' @export
parse_iupac_short <- function(x, mode = "ne") {
  struc_parser_wrapper(x, do_parse_iupac_short, mode = mode)
}


do_parse_iupac_short <- function(x) {
  do_parse_iupac_condensed(convert_short_to_condensed(x))
}


convert_short_to_condensed <- function(x) {
  # The pattern for monosaccharide matching is constructed by concatenating
  # all monosaccharides with "|". This is because in the IUPAC-short format,
  # no parentheses or brackets are used to separate monosaccharides, so a linear
  # sequence like "Neu5Aca3Gala3GlcNAcb-" cannot be easily split.
  # Hardcoded pattern prevents potential errors. This is acceptable because
  # the monosaacharide list is actually not fixed, and can be updated along
  # with glyrepr. The same applies to substituents.
  mono_pattern <- paste(glyrepr::available_monosaccharides("concrete"), collapse = "|")
  sub_pattern <- paste(glyrepr::available_substituents(), collapse = "|")
  full_mono_pattern <- stringr::str_glue("(?:{mono_pattern})(?:(?:\\d+|\\?)(?:{sub_pattern}))?")
  residue_pattern <- stringr::str_glue("({full_mono_pattern})([ab\\?])-?(\\d+|\\?)")
  token_pattern <- paste(residue_pattern, "\\(", "\\)", sep = "|")
  tokens <- stringr::str_extract_all(x, token_pattern)[[1]]

  # The last residue is special because it doesn't have pos 2, e.g. "Mana-"
  # It may even only have the monosaccharide name, e.g. "Man".
  last_residue_pattern <- stringr::str_glue("({full_mono_pattern})([ab\\?])?-?$")
  last_token <- stringr::str_extract(x, last_residue_pattern)

  # Validate if x has been thoroughly parsed into tokens.
  if (paste0(c(tokens, last_token), collapse = "") != x) {
    rlang::abort("Failed to parse the IUPAC-short string.")
  }

  # The anomer positions are fix for known monosaccharides.
  # Except for the monosaccharides with C2 anomer, all others are on C1.
  decide_pos1 <- function(mono) {
    anomer_on_pos2 <- c(
      "Neu5Ac", "Neu5Gc", "Neu", "Kdn", "Pse", "Leg", "Aci",
      "4eLeg", "Kdo", "Dha", "Fru", "Tag", "Sor", "Psi"
    )
    dplyr::if_else(mono %in% anomer_on_pos2, "2", "1")
  }

  # Convert "(" and ")" to "[" and "]", respectively.
  # For residues, convert them to the condensed format.
  # This does not handle the last residue yet.
  process_token <- function(token) {
    dplyr::case_match(
      token,
      "(" ~ "[",
      ")" ~ "]",
      .default = local({
        match <- stringr::str_match(token, residue_pattern)
        mono <- match[, 2]
        anomer <- match[, 3]
        pos2 <- match[, 4]
        pos1 <- decide_pos1(mono)
        if (is.na(pos2)) {
          paste0(mono, "(", anomer, pos1, "-")
        } else {
          paste0(mono, "(", anomer, pos1, "-", pos2, ")")
        }
      })
    )
  }
  processed_tokens <- purrr::map_chr(tokens, process_token)

  # Now handle the last residue.
  last_processed_token <- local({
    match <- stringr::str_match(last_token, last_residue_pattern)
    mono <- match[, 2]
    anomer <- match[, 3]
    pos1 <- decide_pos1(mono)
    if (is.na(anomer)) {
      mono
    } else {
      paste0(mono, "(", anomer, pos1, "-")
    }
  })

  paste0(c(processed_tokens, last_processed_token), collapse = "")
}
