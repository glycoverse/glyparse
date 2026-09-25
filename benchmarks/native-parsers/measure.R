# Rscript measure.R INSTALLED_LIBRARY ROUND OUTPUT.rds
# A fresh process per build and round, with identical dependency libraries.
args <- commandArgs(TRUE)
.libPaths(c(normalizePath(args[[1]]), .libPaths()))
library(glyparse)
workloads <- readRDS("benchmarks/native-parsers/workloads.rds")
rows <- list()
keys <- list()
for (name in names(workloads)) {
  parser <- getExportedValue(
    "glyparse",
    if (name == "auto_iupac") "auto_parse" else name
  )
  x <- workloads[[name]]
  keys[[name]] <- as.character(suppressWarnings(parser(x, on_failure = "na")))
  gc()
  operations <- 3L
  elapsed <- system.time(
    for (i in seq_len(operations)) {
      suppressWarnings(parser(x, on_failure = "na"))
    }
  )[["elapsed"]]
  rows[[name]] <- data.frame(
    parser = name,
    n = length(x),
    valid = sum(!is.na(keys[[name]])),
    round = as.integer(args[[2]]),
    operations = operations,
    seconds = elapsed / operations
  )
}
saveRDS(
  list(timings = do.call(rbind, rows), keys = keys, session = sessionInfo()),
  args[[3]]
)
