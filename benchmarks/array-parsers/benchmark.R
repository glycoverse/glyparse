# Run from the current checkout with a baseline checkout made by git archive.
# Rscript benchmarks/array-parsers/benchmark.R CHECKOUT OUTPUT.rds
args <- commandArgs(TRUE)
pkgload::load_all(args[[1]], quiet = TRUE)
inputs <- readRDS("benchmarks/array-parsers/inputs.rds")
families <- c(
  "parse_pglyco_struc",
  "parse_strucgp_struc",
  "parse_kcf",
  "parse_linucs",
  "parse_glycoct",
  "parse_wurcs"
)
workloads <- lapply(inputs[families], function(x) unique(x[!is.na(x)]))
# Add controlled synthetic trees to the two small generic-format fixtures.
workloads$parse_pglyco_struc <- unique(c(
  workloads$parse_pglyco_struc,
  vapply(
    1:40,
    function(n) paste0(strrep("(H", n), "(N)", strrep(")", n)),
    character(1)
  )
))
workloads$parse_strucgp_struc <- unique(c(
  workloads$parse_strucgp_struc,
  vapply(
    1:24,
    function(n) {
      paste0(
        paste0(LETTERS[1:n], "1", collapse = ""),
        paste0(rev(letters[1:n]), collapse = "")
      )
    },
    character(1)
  )
))
# Frozen values are reused by both revisions; failures are not timed.
if (!file.exists("benchmarks/array-parsers/workloads.rds")) {
  workloads <- lapply(names(workloads), function(name) {
    x <- workloads[[name]]
    x[
      !is.na(suppressWarnings(getExportedValue("glyparse", name)(
        x,
        on_failure = "na"
      )))
    ]
  }) |>
    stats::setNames(families)
  workloads$auto_parse <- unique(unlist(workloads, use.names = FALSE))
  iupac <- readRDS("../glyrepr/benchmarks/compact-cpp/results-v2/corpus.rds")
  workloads$auto_iupac <- head(unique(iupac[!is.na(iupac)]), 500L)
  saveRDS(workloads, "benchmarks/array-parsers/workloads.rds")
} else {
  workloads <- readRDS("benchmarks/array-parsers/workloads.rds")
}
rows <- list()
keys <- list()
for (name in names(workloads)) {
  parser <- getExportedValue(
    "glyparse",
    if (name == "auto_iupac") "auto_parse" else name
  )
  x <- workloads[[name]]
  keys[[name]] <- as.character(suppressWarnings(parser(x, on_failure = "na")))
  for (i in 1:5) {
    gc()
    elapsed <- system.time(suppressWarnings(parser(x, on_failure = "na")))[[
      "elapsed"
    ]]
    rows[[length(rows) + 1L]] <- data.frame(
      parser = name,
      n = length(x),
      iteration = i,
      seconds = elapsed
    )
    cat(name, length(x), i, elapsed, "\n")
  }
}
saveRDS(
  list(timings = do.call(rbind, rows), keys = keys, session = sessionInfo()),
  args[[2]]
)
