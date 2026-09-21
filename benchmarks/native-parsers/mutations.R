# Deterministic malformed-input differential probe; run against each revision.
# Rscript mutations.R CHECKOUT OUTPUT.rds
args <- commandArgs(TRUE)
pkgload::load_all(args[[1]], quiet = TRUE)
path <- 'benchmarks/native-parsers/mutations.rds'
if (!file.exists(path)) {
  set.seed(20260921)
  workloads <- readRDS('benchmarks/native-parsers/workloads.rds')
  workloads <- workloads[
    !names(workloads) %in%
      c('auto_parse', 'auto_iupac', 'parse_iupac_condensed')
  ]
  mutate <- function(x) {
    i <- sample.int(nchar(x), 1)
    switch(
      sample.int(3, 1),
      paste0(substr(x, 1, i - 1), substr(x, i + 1, nchar(x))),
      paste0(
        substr(x, 1, i),
        sample(c('?', ' ', 'a', '1', '\n'), 1),
        substr(x, i + 1, nchar(x))
      ),
      paste0(substr(x, 1, i - 1), '?', substr(x, i + 1, nchar(x)))
    )
  }
  inputs <- lapply(workloads, function(x) {
    chosen <- head(x, 8)
    unique(c(
      chosen,
      unlist(lapply(chosen, function(s) replicate(10, mutate(s))))
    ))
  })
  saveRDS(inputs, path)
}
inputs <- readRDS(path)
results <- lapply(names(inputs), function(n) {
  as.character(suppressWarnings(getExportedValue('glyparse', n)(
    inputs[[n]],
    on_failure = 'na'
  )))
})
names(results) <- names(inputs)
saveRDS(results, args[[2]])
