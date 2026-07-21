#' Parse IUPAC-compact Structures
#'
#' Parse IUPAC-compact strings into a [glyrepr::glycan_structure()].
#'
#' @details
#' IUPAC-compact notation is similar to IUPAC-condensed notation, but linkages
#' are written directly after the monosaccharide, such as `"Galb1-3GlcNAc"`,
#' and branches are written in parentheses. The parser normalizes compact
#' notation into IUPAC-condensed notation, then uses the IUPAC-condensed parser
#' to construct the glycan structure.
#'
#' Alditol glycans are parsed as regular reducing-end glycans with unknown
#' anomer configurations.
#'
#' @param x A character vector of IUPAC-compact strings. NA values are allowed
#'   and will be returned as NA structures.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param progress Whether to show a progress bar while parsing.
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' iupac <- "Mana1-3(Mana1-6)Manb1-4GlcNAcb"
#' parse_iupac_compact(iupac)
#'
#' @seealso [parse_iupac_condensed()]
#'
#' @export
parse_iupac_compact <- function(x, on_failure = "error", progress = FALSE) {
  normalized_struc_parser_wrapper(
    x,
    convert_iupac_compact_to_condensed,
    on_failure = on_failure,
    progress = progress
  )
}


#' Parse one IUPAC-compact string
#'
#' @param x A single IUPAC-compact string.
#'
#' @return A glycan graph.
#' @noRd
do_parse_iupac_compact <- function(x) {
  do_parse_iupac_condensed(convert_iupac_compact_to_condensed(x))
}


#' Convert IUPAC-compact to IUPAC-condensed notation
#'
#' @param x A character vector of IUPAC-compact strings.
#'
#' @return A character vector containing IUPAC-condensed notation.
#' @noRd
convert_iupac_compact_to_condensed <- function(x) {
  if (any(is_iupac_compact_alditol(x))) {
    warn_iupac_compact_alditol()
  }

  x |>
    normalize_iupac_compact_aliases() |>
    normalize_iupac_compact_modifiers() |>
    normalize_iupac_compact_branches() |>
    normalize_iupac_compact_linkages() |>
    normalize_iupac_compact_alditol() |>
    normalize_iupac_compact_terminal()
}


#' Normalize IUPAC-compact monosaccharide aliases
#'
#' @param x A character vector of IUPAC-compact strings.
#'
#' @return A character vector with aliases normalized.
#' @noRd
normalize_iupac_compact_aliases <- function(x) {
  x |>
    stringr::str_replace_all(stringr::fixed("Neu5,9Ac2"), "Neu5Ac9Ac") |>
    stringr::str_replace_all(stringr::fixed("NeuAc"), "Neu5Ac") |>
    stringr::str_replace_all(stringr::fixed("NeuGc"), "Neu5Gc")
}


#' Normalize IUPAC-compact parenthesized residue modifiers
#'
#' @param x A character vector of IUPAC-compact strings.
#'
#' @return A character vector with modifiers moved after monosaccharide names.
#' @noRd
normalize_iupac_compact_modifiers <- function(x) {
  modifier_pattern <- iupac_compact_modifier_before_mono_pattern()

  repeat {
    normalized <- stringr::str_replace_all(
      x,
      modifier_pattern,
      normalize_iupac_compact_modifier_match
    )
    if (identical(normalized, x)) {
      return(x)
    }
    x <- normalized
  }
}


#' Normalize IUPAC-compact branch delimiters
#'
#' @param x A character vector of IUPAC-compact strings.
#'
#' @return A character vector with IUPAC-condensed branch delimiters.
#' @noRd
normalize_iupac_compact_branches <- function(x) {
  x |>
    stringr::str_replace_all(stringr::fixed("("), "[") |>
    stringr::str_replace_all(stringr::fixed(")"), "]")
}


#' Normalize IUPAC-compact linkage notation
#'
#' @param x A character vector of IUPAC-compact strings.
#'
#' @return A character vector with IUPAC-condensed linkage notation.
#' @noRd
normalize_iupac_compact_linkages <- function(x) {
  x |>
    stringr::str_replace_all(stringr::fixed("|"), "/") |>
    stringr::str_replace_all(
      iupac_compact_linkage_pattern(),
      "(\\1\\2-\\3)"
    )
}


#' Normalize IUPAC-compact alditol suffixes
#'
#' @param x A character vector of partially normalized IUPAC-compact strings.
#'
#' @return A character vector with alditol suffixes removed and reducing-end
#'   anomer set to unknown.
#' @noRd
normalize_iupac_compact_alditol <- function(x) {
  alditol <- is_iupac_compact_alditol(x)
  if (!any(alditol)) {
    return(x)
  }

  x[alditol] <- purrr::map_chr(x[alditol], function(value) {
    value <- stringr::str_remove(value, "\\+aldi$")
    terminal <- iupac_compact_terminal_match(value)
    if (is.null(terminal)) {
      return(value)
    }
    replacement <- paste0(terminal$mono, terminal$modifiers)
    stringr::str_replace(value, iupac_compact_terminal_pattern(), replacement)
  })
  x
}


#' Normalize the reducing-end terminal of an IUPAC-compact string
#'
#' @param x A character vector of partially normalized IUPAC-compact strings.
#'
#' @return A character vector with reducing-end IUPAC-condensed linkages.
#' @noRd
normalize_iupac_compact_terminal <- function(x) {
  terminal_match <- stringr::str_match(x, iupac_compact_terminal_pattern())
  matched <- !is.na(terminal_match[, "mono"])
  if (!any(matched)) {
    return(x)
  }

  mono <- terminal_match[matched, "mono"]
  modifiers <- terminal_match[matched, "modifiers"]
  anomer <- terminal_match[matched, "anomer"]
  anomer[is.na(anomer)] <- "?"

  anomer_pos <- iupac_compact_default_anomer_pos(mono)
  terminal <- stringr::str_glue("{mono}{modifiers}({anomer}{anomer_pos}-")
  x[matched] <- stringr::str_replace(
    x[matched],
    iupac_compact_terminal_pattern(),
    terminal
  )
  x
}


#' Check whether a string is an IUPAC-compact alditol glycan
#'
#' @param x A single structure string.
#'
#' @return A logical vector.
#' @noRd
is_iupac_compact_alditol <- function(x) {
  stringr::str_ends(x, stringr::fixed("+aldi"))
}


#' Warn about alditol normalization in IUPAC-compact parsing.
#'
#' @return `NULL`, invisibly.
#' @noRd
warn_iupac_compact_alditol <- function() {
  cli::cli_warn(c(
    "Alditol IUPAC-compact glycans are parsed as regular reducing-end glycans with unknown anomer configurations.",
    "i" = "For example, Glc-ol is returned as Glc(?1-."
  ))
  invisible(NULL)
}


#' Check whether a string looks like IUPAC-compact notation
#'
#' @param x A single structure string.
#'
#' @return A logical scalar.
#' @noRd
is_iupac_compact_string <- function(x) {
  is_iupac_compact_alditol(x) ||
    stringr::str_detect(x, iupac_compact_linkage_pattern()) ||
    !is.null(iupac_compact_terminal_match(normalize_iupac_compact_aliases(x)))
}


#' Match terminal monosaccharide fields of an IUPAC-compact string
#'
#' @param x A single structure string.
#'
#' @return A named list of terminal fields, or `NULL` when unmatched.
#' @noRd
iupac_compact_terminal_match <- function(x) {
  match <- stringr::str_match(x, iupac_compact_terminal_pattern())
  if (is.na(match[1, "mono"])) {
    return(NULL)
  }

  list(
    mono = match[1, "mono"],
    modifiers = match[1, "modifiers"],
    anomer = match[1, "anomer"]
  )
}


#' Normalize one IUPAC-compact modifier match
#'
#' @param x A matched modifier-plus-monosaccharide string.
#'
#' @return A normalized monosaccharide-plus-modifier string.
#' @noRd
normalize_iupac_compact_modifier_match <- function(x) {
  purrr::map_chr(x, normalize_one_iupac_compact_modifier_match)
}


#' Normalize one scalar IUPAC-compact modifier match
#'
#' @param x A matched modifier-plus-monosaccharide string.
#'
#' @return A normalized monosaccharide-plus-modifier string.
#' @noRd
normalize_one_iupac_compact_modifier_match <- function(x) {
  modifier_match <- stringr::str_match(
    x,
    paste0("^", iupac_compact_modifier_before_mono_pattern(), "$")
  )
  modifier <- modifier_match[1, "modifier"]
  mono <- modifier_match[1, "mono"]

  if (stringr::str_detect(modifier, "^[A-Za-z]")) {
    modifier <- stringr::str_glue("?{modifier}")
  }

  stringr::str_glue("{mono}{modifier}")
}


#' Create the full IUPAC-compact linkage regex
#'
#' @return A regex pattern.
#' @noRd
iupac_compact_linkage_pattern <- function() {
  "([ab\\?])([0-9\\?])-([0-9\\?](?:/[0-9\\?])*)"
}


#' Create the terminal IUPAC-compact regex
#'
#' @return A regex pattern.
#' @noRd
iupac_compact_terminal_pattern <- function() {
  paste0(
    "(?<mono>",
    iupac_compact_monosaccharide_pattern(),
    ")",
    "(?<modifiers>(?:",
    iupac_compact_modifier_pattern(),
    ")*)",
    "(?<anomer>[ab\\?])?",
    "$"
  )
}


#' Create a normalized IUPAC-compact modifier regex
#'
#' @return A regex pattern.
#' @noRd
iupac_compact_modifier_pattern <- function() {
  paste0("[0-9\\?](?:", iupac_compact_substituent_pattern(), ")")
}


#' Create a modifier-before-monosaccharide regex for IUPAC-compact parsing
#'
#' @return A regex pattern.
#' @noRd
iupac_compact_modifier_before_mono_pattern <- function() {
  paste0(
    "\\(",
    "(?<modifier>[0-9\\?]*(?:",
    iupac_compact_substituent_pattern(),
    "))",
    "\\)",
    "(?<mono>",
    iupac_compact_monosaccharide_pattern(),
    ")"
  )
}


#' Create a substituent-name regex for IUPAC-compact parsing
#'
#' @return A regex pattern.
#' @noRd
iupac_compact_substituent_pattern <- local({
  pattern <- NULL

  function() {
    if (!is.null(pattern)) {
      return(pattern)
    }

    pattern <<- literal_regex_alternation(glyrepr::available_substituents())
    pattern
  }
})


#' Create a monosaccharide-name regex for IUPAC-compact parsing
#'
#' @return A regex pattern.
#' @noRd
iupac_compact_monosaccharide_pattern <- local({
  pattern <- NULL

  function() {
    if (!is.null(pattern)) {
      return(pattern)
    }

    pattern <<- literal_regex_alternation(glyrepr::available_monosaccharides())
    pattern
  }
})


#' Get the default reducing-end anomer position for a monosaccharide
#'
#' @param mono A monosaccharide name.
#'
#' @return A character scalar.
#' @noRd
iupac_compact_default_anomer_pos <- function(mono) {
  if (glyrepr::get_mono_type(mono) != "concrete") {
    # TODO: Remove this fallback when glyrepr::get_mono_type() and
    # glyrepr::get_anomer_pos() support generic glycans.
    return("1")
  }

  glyrepr::get_anomer_pos(mono)
}


#' Create a regex alternation from literal tokens
#'
#' @param x A character vector of literal regex alternatives.
#'
#' @return A regex alternation pattern.
#' @noRd
literal_regex_alternation <- function(x) {
  # glyrepr names include prefix overlaps, so match the longest tokens first.
  x <- x[order(nchar(x), decreasing = TRUE)]
  paste0(purrr::map_chr(x, stringr::str_escape), collapse = "|")
}
