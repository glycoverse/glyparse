# Recreate native dictionaries from the frozen R implementation.
# Rscript export-tables.R BASELINE_CHECKOUT OUTPUT.json
args <- commandArgs(TRUE)
pkgload::load_all(args[[1]], quiet = TRUE)
pairs <- function(x) {
  lapply(seq_along(x), function(i) c(names(x)[[i]], unname(x[[i]])))
}
tables <- list(
  glycam = pairs(glycam_iupac_mono_map()),
  linucs = pairs(linucs_mono_stem_map()),
  extended = pairs(IUPAC_EXT_TO_CON),
  wurcs = pairs(WURCS_MONO_REGEX),
  wurcs_unknown = pairs(WURCS_UNKNOWN_RING_MONO_REGEX),
  wurcs_ambiguous = pairs(WURCS_AMBIGUOUS_MONO_REGEX),
  wurcs_alditol = pairs(WURCS_ALDITOL_MONO_REGEX),
  wurcs_sub = pairs(WURCS_SUB_REGEX),
  glycoct_entries = unname(glycoct_mapping_index()$entries),
  glycoct_alditol_entries = unname(glycoct_mapping_index(TRUE)$entries)
)
jsonlite::write_json(tables, args[[2]], auto_unbox = TRUE, pretty = TRUE)
