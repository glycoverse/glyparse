args <- commandArgs(TRUE)
stopifnot(length(args) == 3L)
p <- args[1]
out <- normalizePath(args[2])
.libPaths(c(normalizePath(args[3]), .libPaths()))
library(glyparse)
options(
  glyparse.threads = as.integer(Sys.getenv(
    "GLYPARSE_THREADS",
    as.character(getOption("glyparse.threads", 2L))
  ))
)
files <- list.files(file.path(out, 'data'), full.names = TRUE, pattern = 'csv$')
inputs <- list()
for (file in files) {
  fmt <- sub(
    '^(glycan_sequences_|derived_)',
    '',
    sub('.csv$', '', basename(file))
  )
  inputs[[paste0('parse_', fmt)]] <- read.csv(file, stringsAsFactors = FALSE)[[
    2
  ]]
}
set.seed(20261021)
inputs$auto_parse <- unname(unlist(
  lapply(inputs, function(x) sample(x, min(length(x), 500))),
  use.names = FALSE
))
x <- inputs[[p]]
f <- getExportedValue('glyparse', p)
gc()
t <- system.time(y <- suppressWarnings(f(x, on_failure = 'na')))
row <- data.frame(
  parser = p,
  rows = length(x),
  elapsed = t[['elapsed']],
  successful = sum(!is.na(y))
)
print(row)
write.table(
  row,
  file.path(out, 'fresh.csv'),
  sep = ',',
  row.names = FALSE,
  col.names = !file.exists(file.path(out, 'fresh.csv')),
  append = file.exists(file.path(out, 'fresh.csv'))
)
b <- readRDS(file.path(out, paste0(p, '-full-baseline.rds')))
stopifnot(identical(as.character(y), as.character(b)))
mask <- !is.na(y)
rm(y, b)
rows <- list()
for (i in 1:3) {
  gc()
  t <- system.time(y <- suppressWarnings(f(x[mask], on_failure = 'na')))
  stopifnot(!anyNA(y))
  rows[[i]] <- data.frame(
    parser = p,
    iteration = i,
    rows = sum(mask),
    elapsed = t[['elapsed']]
  )
  rm(y)
}
write.table(
  do.call(rbind, rows),
  file.path(out, 'successful.csv'),
  sep = ',',
  row.names = FALSE,
  col.names = !file.exists(file.path(out, 'successful.csv')),
  append = file.exists(file.path(out, 'successful.csv'))
)
