#' Parse Linear Code Structures
#'
#' Parse Linear Code structures into a [glyrepr::glycan_structure()].
#' To know more about Linear Code, see [this article](https://www.jstage.jst.go.jp/article/tigg1989/14/77/14_77_127/_article).
#'
#' @param x A character vector of Linear Code strings.
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' linear_code <- "Ma3(Ma6)Mb4GNb4GNb"
#' parse_linear_code(linear_code)
#'
#' @export
parse_linear_code <- function(x) {
  struc_parser_wrapper(x, do_parse_linear_code)
}

do_parse_linear_code <- function(x) {
  do_parse_iupac_condensed(convert_linear_to_iupac(x))
}

convert_linear_to_iupac <- function(x) {
  # Mono mapping from IUPAC to Linear Code
  mono_map <- c(
    "Glc" = "G",
    "Gal" = "A",
    "GlcNAc" = "GN",
    "GalNAc" = "AN",
    "Man" = "M",
    "Neu5Ac" = "NN",
    "Neu" = "N",
    "Kdn" = "K",
    "Kdo" = "W",
    "GalA" = "L",
    "Ido" = "I",
    "Rha" = "H",
    "Fuc" = "F",
    "Xyl" = "X",
    "Rib" = "B",
    "Ara" = "R",
    "GlcA" = "U",
    "All" = "O",
    "Api" = "P",
    "Fru" = "E"
  )

  # Substituent mapping from IUPAC to Linear Code
  sub_map <- c(
    "NAc" = "N",
    "Me" = "ME",
    "Ac" = "T",
    "P" = "P",
    "S" = "S"
  )

  # ===== Convert linkages =====
  # "Ab3A[2P]b3(Ab4)Gb" will be converted to "A(b1-3)A_2P_(b1-3)[A(b1-4)]G(b1-"

  # First convert "[]" (substituents) to "_"
  x <- stringr::str_replace_all(x, stringr::fixed("["), "_")
  x <- stringr::str_replace_all(x, stringr::fixed("]"), "_")
  # Then convert "()" (branches) to "[]"
  x <- stringr::str_replace_all(x, stringr::fixed("("), "[")
  x <- stringr::str_replace_all(x, stringr::fixed(")"), "]")
  # Finally convert "a3" to "(a1-3)"
  # Here we assume the anomer position is always 1
  x <- stringr::str_replace_all(x, "([ab\\?])(\\d+(?:/\\d+)*|\\?)", "(\\11-\\2)")
  red_anomer <- stringr::str_sub(x, -1, -1)
  x <- paste0(stringr::str_sub(x, 1, -2), "(", red_anomer, "1-")

  # ===== Convert monosaccharides =====
  # "A(b1-3)A_2P_(b1-3)[A(b1-4)]G(b1-" will be converted to "Gal(b1-3)Gal_2P_(b1-3)[Gal(b1-4)]Glc(b1-"

  mono_patterns <- stringr::str_glue("(?<![:alnum:]|\\?){mono_map}(?=[_\\(])")
  mono_replaces <- names(mono_map)
  for (i in seq_along(mono_patterns)) {
    x <- stringr::str_replace_all(x, mono_patterns[i], mono_replaces[i])
  }

  # ===== Add anomer positions =====
  # When converting linkages, we assume the anomer positions are always 1.
  # Here we replace some of them with 2.
  anomer_pos <- decide_anomer_pos(names(mono_map))
  anomer_patterns <- stringr::str_glue("(?<![:alnum:])({names(mono_map)})(_.*?_)?\\(([ab\\?])1-")
  anomer_replaces <- stringr::str_glue("\\1\\2\\(\\3{anomer_pos}-")
  for (i in seq_along(anomer_patterns)) {
    x <- stringr::str_replace_all(x, anomer_patterns[i], anomer_replaces[i])
  }

  # ===== Convert substituents =====
  # "Gal(b1-3)Gal_2P_(b1-3)[Gal(b1-4)]Glc(b1-" will be converted to "Gal(b1-3)Gal2P(b1-3)[Gal(b1-4)]Glc(b1-"

  sub_patterns <- stringr::str_glue("_(\\d+|\\?)({sub_map})_")
  sub_replaces <- stringr::str_glue("\\1{names(sub_map)}")
  for (i in seq_along(sub_patterns)) {
    x <- stringr::str_replace_all(x, sub_patterns[i], sub_replaces[i])
  }

  x
}
