# Rscript compare.R BASELINE_LIBRARY NATIVE_LIBRARY
args <- commandArgs(TRUE)
for (round in seq_len(5L)) {
  for (build in if (round %% 2L) {
    c("before", "after")
  } else {
    c("after", "before")
  }) {
    output <- sprintf("benchmarks/native-parsers/%s-%s.rds", build, round)
    status <- system2(
      file.path(R.home("bin"), "Rscript"),
      c(
        "benchmarks/native-parsers/measure.R",
        shQuote(args[[if (build == "before") 1L else 2L]]),
        round,
        output
      )
    )
    stopifnot(status == 0L)
    cat(build, round, "complete\n")
  }
}
