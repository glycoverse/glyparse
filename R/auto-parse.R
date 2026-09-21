#' Automatic Structure Parsing
#'
#' @description
#' Detect the structure string type and use the appropriate parser
#' to parse automatically.
#' Mixed types are supported.
#'
#' Supported types:
#' 1. GlycoCT
#' 2. IUPAC-condensed
#' 3. IUPAC-extended
#' 4. IUPAC-short
#' 5. GlyCAM IUPAC
#' 6. IUPAC-compact
#' 7. WURCS
#' 8. Linear Code
#' 9. pGlyco
#' 10. StrucGP
#' 11. KCF
#' 12. LINUCS
#' 13. GlycoWorkbench
#'
#' @param x A character vector of structure strings. NA values are allowed and will be returned as NA structures.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param progress Whether to show a progress bar while parsing.
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' # Single structure
#' x <- "Gal(b1-3)GlcNAc(b1-4)Glc(a1-"  # IUPAC-condensed
#' auto_parse(x)
#'
#' # Mixed types
#' x <- c(
#'   "Gal(b1-3)GlcNAc(b1-4)Glc(a1-",  # IUPAC-condensed
#'   "Neu5Aca3Gala3(Fuca6)GlcNAcb-"  # IUPAC-short
#' )
#' auto_parse(x)
#'
#' @export
auto_parse <- function(
  x,
  on_failure = "error",
  progress = FALSE
) {
  on_failure <- validate_struc_parser_wrapper_args(
    x,
    on_failure,
    progress,
    call = rlang::current_env()
  )
  input <- prepare_struc_parser_input(x)
  if (input$all_na) {
    return(make_na_glycan_structure(input$size, input$names))
  }

  formats <- vapply(input$unique_x, choose_parser, character(1))
  groups <- split(seq_along(input$unique_x), formats)
  parsed <- lapply(names(groups), function(format) {
    parser <- get(format, envir = environment(auto_parse))
    parser(
      input$unique_x[groups[[format]]],
      on_failure = "na",
      progress = progress
    )
  })
  unique_result <- do.call(c, parsed)[order(unlist(groups, use.names = FALSE))]
  abort_on_invalid_parse(
    input$unique_x[is.na(unique_result)],
    on_failure,
    call = rlang::current_env()
  )
  result <- unique_result[build_normalized_structure_indices(input)]
  if (!is.null(input$names)) {
    attr(result, "names") <- input$names
  }
  result
}

choose_parser <- function(x) {
  if (stringr::str_starts(x, "freeEnd|redEnd")) {
    return("parse_gwb")
  } else if (stringr::str_detect(x, "ENTRY")) {
    return("parse_kcf")
  } else if (stringr::str_detect(x, "RES")) {
    return("parse_glycoct")
  } else if (stringr::str_detect(x, "WURCS")) {
    return("parse_wurcs")
  } else if (stringr::str_starts(x, "\\([HNAGFSap]")) {
    return("parse_pglyco_struc")
  } else if (stringr::str_starts(x, "A") && stringr::str_ends(x, "a")) {
    return("parse_strucgp_struc")
  } else if (stringr::str_ends(x, "-OH")) {
    return("parse_glycam_iupac")
  } else if (stringr::str_ends(x, stringr::fixed("-ol"))) {
    return("parse_iupac_condensed")
  } else if (
    stringr::str_detect(x, "\\u2192") || # Unicode arrow →
      stringr::str_detect(x, "->") || # Plain text arrow ->
      stringr::str_detect(x, "alpha|beta") # Plain text anomers
  ) {
    return("parse_iupac_extended")
  } else if (stringr::str_detect(x, "\\w+\\([ab\\?][\\d\\?]-")) {
    return("parse_iupac_condensed")
  } else if (is_linucs_string(x)) {
    return("parse_linucs")
  } else if (is_iupac_compact_string(x)) {
    return("parse_iupac_compact")
  } else if (stringr::str_ends(x, "-")) {
    return("parse_iupac_short")
  } else {
    # Assume Linear Code
    return("parse_linear_code")
  }
}
