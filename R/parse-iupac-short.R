#' Parse IUPAC-short Structures
#'
#' Parse IUPAC-short-style structure characters into a [glyrepr::glycan_structure()].
#' For more information about IUPAC-short format, see \doi{10.1351/pac199668101919}.
#'
#' @details
#' The IUPAC-short notation is a compact form of IUPAC-condensed notation.
#' It is rarely used in database, but appears a lot in literature for its
#' conciseness. Compared with IUPAC-condensed notation, IUPAC-short notation
#' ignore the anomer positions, assuming they are known for common monosaccharides.
#' For example, "Neu5Aca3Gala-" assumes the anomer of Neu5Ac is C2 (a2-3 linked).
#' Also, the parentheses around linkages are omitted, and parentheses are used
#' to indicate branching, e.g. "Neu5Aca3Gala3(Fuca3)GlcNAcb-".
#'
#' In the first case, the anomer is "a2". In the second case, the anomer is "?2".
#'
#' @param x A character vector of IUPAC-short strings.
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' iupac <- "Neu5Aca3Gala3(Fuca6)GlcNAcb-"
#' parse_iupac_short(iupac)
#'
#' @seealso [parse_iupac_condensed()], [parse_iupac_extended()]
#'
#' @export
parse_iupac_short <- function(x) {
  struc_parser_wrapper(x, do_parse_iupac_short)
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
  mono_pattern <- paste(glyrepr::available_monosaccharides("all"), collapse = "|")
  sub_pattern <- paste(glyrepr::available_substituents(), collapse = "|")
  full_mono_pattern <- stringr::str_glue("(?:{mono_pattern})(?:(?:\\d+(?:/\\d+)*|\\?)(?:{sub_pattern}))*")
  residue_pattern <- stringr::str_glue("({full_mono_pattern})([ab\\?])-?(\\d+(?:/\\d+)*|\\?)")
  token_pattern <- paste(residue_pattern, "\\(", "\\)", sep = "|")
  tokens <- stringr::str_extract_all(x, token_pattern)[[1]]

  # The last residue is special because it doesn't have pos 2, e.g. "Mana-"
  last_residue_pattern <- stringr::str_glue("({full_mono_pattern})([ab\\?])-$")
  last_token <- stringr::str_extract(x, last_residue_pattern)

  # Validate if x has been thoroughly parsed into tokens.
  if (paste0(c(tokens, last_token), collapse = "") != x) {
    rlang::abort("Failed to parse the IUPAC-short string.")
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
        pos1 <- decide_anomer_pos(mono)
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
    pos1 <- decide_anomer_pos(mono)
    paste0(mono, "(", anomer, pos1, "-")
  })

  paste0(c(processed_tokens, last_processed_token), collapse = "")
}
