#' @useDynLib glyparse, .registration = TRUE
#' @importFrom Rcpp evalCpp
NULL

native_vocabulary <- local({
  vocabulary <- NULL
  function() {
    if (is.null(vocabulary)) {
      concrete <- glyrepr::available_monosaccharides("concrete")
      vocabulary <<- list(
        mono = glyrepr::available_monosaccharides(),
        sub = glyrepr::available_substituents(),
        source_anomer = as.character(glyrepr::get_anomer_pos(glyrepr::available_monosaccharides())),
        concrete = concrete,
        anomer = as.character(glyrepr::get_anomer_pos(concrete))
      )
    }
    vocabulary
  }
})

# Batch native parsing isolates invalid records without entering R per string.
native_convert <- function(x, format) {
  native_convert_cpp(x, format, native_vocabulary())
}

native_records <- function(x, format, progress = FALSE) {
  parse_batch <- function(values) {
    records <- native_records_cpp(values, format, native_vocabulary())
    for (i in which(attr(records, "warnings"))) {
      if (format == "wurcs") {
        cli::cli_warn(c(
          "Only the main reducing-end WURCS residue can retain alditol status.",
          "i" = "Non-root alditol residues are parsed as regular residues."
        ))
      } else if (format == "glycoct") {
        cli::cli_warn(c(
          "Only the main reducing-end GlycoCT residue can retain alditol status.",
          "i" = "Other alditol residues are parsed as regular residues."
        ))
      }
    }
    records
  }
  if (!progress) {
    return(parse_batch(x))
  }
  chunks <- split(x, ceiling(seq_along(x) / 128L))
  unlist(
    purrr::map(chunks, parse_batch, .progress = TRUE),
    recursive = FALSE,
    use.names = FALSE
  )
}

convert_short_to_condensed <- function(x) native_convert(x, "iupac_short")
convert_ext_to_con <- function(x) native_convert(x, "iupac_extended")
convert_iupac_compact_to_condensed <- function(x) {
  native_convert(x, "iupac_compact")
}
convert_glycam_iupac_to_condensed <- function(x) {
  native_convert(x, "glycam_iupac")
}
convert_gwb_to_condensed <- function(x) native_convert(x, "gwb")
