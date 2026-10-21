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
  native_convert_cpp(x, format, native_vocabulary(), native_threads())
}

native_records <- function(x, format, progress = FALSE) {
  threads <- native_threads()
  parse_batch <- function(values) {
    records <- native_records_cpp(
      values,
      format,
      native_vocabulary(),
      threads,
      constructible_only = TRUE
    )
    # One warning describes the same lossy conversion for the whole batch.
    if (any(attr(records, "warnings"))) {
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
  # Leave enough native work for every configured worker between updates.
  chunk_size <- max(1024L, 128L * threads)
  chunks <- split(x, ceiling(seq_along(x) / chunk_size))
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

# Limit the default batch to two CPU workers. Parsing only uses owned C++ data
# on workers; graph construction and all R operations remain on the caller.
native_threads <- function() {
  threads <- getOption("glyparse.threads", 2L)
  checkmate::assert_int(threads, lower = 1, upper = 64)
  as.integer(threads)
}

#' Native Parsing Options
#'
#' Native format decoding and normalization use up to two CPU workers for
#' batches of at least 256 distinct, normalized inputs. Smaller batches are
#' decoded serially. Set `options(glyparse.threads = 1L)` to disable parallel
#' decoding, or set an integer between 1 and 64 to choose the worker limit.
#' Each worker has its own residue cache. R object allocation, validation,
#' progress reporting, and graph construction run on the calling thread.
#' No complete input or parsed result is cached between calls.
#'
#' @name glyparse_options
NULL
