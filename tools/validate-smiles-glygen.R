# Run from the source checkout:
# Rscript tools/validate-smiles-glygen.R smiles.csv wurcs.csv
# Inputs: GlyGen 2.12.1 GLY_000291 and GLY_000292 reviewed CSV downloads.
# This deliberately prints validation counts without persisting result files.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
devtools::load_all(quiet = TRUE)
source("tests/testthat/helper-smiles.R")
s <- read.csv(args[1])
w <- read.csv(args[2])
stopifnot(!anyDuplicated(s$glytoucan_ac), !anyDuplicated(w$glytoucan_ac))
d <- merge(s, w, by = "glytoucan_ac")
a <- parse_smiles(d$sequence_smiles_isomeric, on_failure = "na")
b <- suppressWarnings(parse_wurcs(d$sequence_wurcs, on_failure = "na"))
both <- !is.na(a) & !is.na(b)
same <- both & as.character(a) == as.character(b)
same[is.na(same)] <- FALSE
reoriented <- which(both & !same)
for (i in reoriented) {
  same[i] <- identical(smiles_graph_identity(a[i]), smiles_graph_identity(b[i]))
}
cat("Matched accessions:", nrow(d), "\n")
cat("SMILES accepted:", sum(!is.na(a)), "\n")
cat("WURCS accepted:", sum(!is.na(b)), "\n")
cat("Both accepted:", sum(both), "\n")
cat("Identical residue graphs, ignoring conventional root:", sum(same), "\n")
print(
  data.frame(
    accession = d$glytoucan_ac[both & !same],
    smiles_result = as.character(a)[both & !same],
    wurcs_result = as.character(b)[both & !same]
  ),
  row.names = FALSE
)
z <- glyparse:::native_aux_cpp(
  "smiles_diagnostics",
  list(x = d$sequence_smiles_isomeric),
  glyparse:::native_vocabulary()
)
cat("Atom-graph decoding rejection reasons:\n")
print(sort(table(z$error), decreasing = TRUE))
cat(
  "Decoded but not in the current residue/structure vocabulary:",
  sum(!is.na(z$wurcs) & is.na(a)),
  "\n"
)
