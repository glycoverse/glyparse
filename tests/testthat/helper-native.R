# Internal probes exercise the same native primitives as the public parsers.
# Frozen format mappings are input fixtures, not a runtime R parser fallback.
load_mono_mappings <- function() {
  readRDS(test_path("fixtures", "glycoct-mappings.rds"))
}
native_probe <- function(op, ..., call = rlang::caller_env()) {
  tryCatch(
    native_aux_cpp(op, list(...), native_vocabulary()),
    error = function(e) cli::cli_abort(conditionMessage(e), call = call)
  )
}
native_map <- function(name) native_probe("map", name = name)
glycam_iupac_mono_map <- function() native_map("glycam")
linucs_mono_stem_map <- function() native_map("linucs")
furanose_monosaccharide_map <- function() native_map("furanose")
unusual_configuration_monosaccharide_map <- function() native_map("unusual")
as_furanose_monosaccharide <- function(mono) native_probe("furanose", x = mono)
as_ringless_monosaccharide <- function(mono) native_probe("ringless", x = mono)
apply_monosaccharide_configuration <- function(mono, configuration) {
  native_probe("configuration", x = mono, configuration = configuration)
}
invert_configuration <- function(x) native_probe("invert", x = x)
iupac_compact_default_anomer_pos <- function(mono) {
  native_probe("anomer", x = mono)
}
parse_residue <- function(residue) native_probe("wurcs_residue", x = residue)
invert_wurcs_pattern_configuration <- function(pattern) {
  native_probe("wurcs_invert_pattern", x = pattern)
}
letter_to_int <- function(letter) native_probe("wurcs_letter", x = letter)
parse_one_linkage <- function(x, anomers = NULL, alditols = NULL) {
  native_probe("wurcs_linkage", x = x, anomers = anomers, alditols = alditols)
}
parse_wurcs_floating_linkage <- function(x) {
  native_probe("wurcs_floating", x = x)
}
map_single_mono_ringless <- function(content) {
  native_probe("ct_base", x = content)
}
glycoct_mono_signature <- function(content) {
  native_probe("ct_signature", x = content)
}
match_composite_structure <- function(signature) {
  native_probe("ct_exact", signature = signature)
}
parse_glycoct_und_block <- function(lines) native_probe("ct_und", lines = lines)
normalize_floating_substituent_parents <- function(
  parents,
  implicit_parents,
  substituent,
  occupied_slots,
  context
) {
  native_probe(
    "floating_sub",
    parents = parents,
    implicit = implicit_parents,
    substituent = substituent,
    occupied = occupied_slots,
    context = context,
    call = rlang::caller_env()
  )
}
filter_glycoct_und_parents <- function(parents, linkage, occupied_slots) {
  native_probe(
    "floating_parents",
    parents = parents,
    implicit = integer(),
    linkage = linkage,
    occupied = occupied_slots,
    context = "GlycoCT UND part"
  )
}
parse_pglyco_struc_arrays <- function(x) {
  native_records_cpp(x, "pglyco", native_vocabulary())[[1]]
}
parse_strucgp_struc_arrays <- function(x) {
  native_records_cpp(x, "strucgp", native_vocabulary())[[1]]
}
WURCS_MONO_REGEX <- native_map("wurcs")
WURCS_UNKNOWN_RING_MONO_REGEX <- native_map("wurcs_unknown")
WURCS_ALDITOL_MONO_REGEX <- native_map("wurcs_alditol")
WURCS_AMBIGUOUS_MONO_REGEX <- native_map("wurcs_ambiguous")

IUPAC_EXT_TO_CON <- native_map("extended")
