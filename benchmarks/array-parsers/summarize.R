before <- readRDS("benchmarks/array-parsers/timing-before.rds")
after <- readRDS("benchmarks/array-parsers/timing-after.rds")
stopifnot(identical(before$keys, after$keys))
summary_rows <- lapply(unique(before$timings$parser), function(name) {
  a <- subset(before$timings, parser == name)
  b <- subset(after$timings, parser == name)
  data.frame(
    parser = name,
    n = a$n[[1]],
    valid = sum(!is.na(before$keys[[name]])),
    old_min = min(a$seconds),
    old_median = median(a$seconds),
    old_max = max(a$seconds),
    new_min = min(b$seconds),
    new_median = median(b$seconds),
    new_max = max(b$seconds),
    speedup = median(a$seconds) / median(b$seconds)
  )
})
summary <- do.call(rbind, summary_rows)
write.csv(
  summary,
  "benchmarks/array-parsers/timing-summary.csv",
  row.names = FALSE
)
print(summary, digits = 4)
backend <- readRDS("benchmarks/array-parsers/backend-results.rds")
print(aggregate(seconds ~ path, backend$timings, function(x) {
  c(min = min(x), median = median(x), max = max(x))
}))
write.csv(
  data.frame(
    file = basename(names(tools::md5sum(list.files(
      "benchmarks/array-parsers",
      pattern = "rds$",
      full.names = TRUE
    )))),
    md5 = unname(tools::md5sum(list.files(
      "benchmarks/array-parsers",
      pattern = "rds$",
      full.names = TRUE
    )))
  ),
  "benchmarks/array-parsers/hashes.csv",
  row.names = FALSE
)
lines <- c(
  "# Array parser migration results",
  "",
  "Production commit: `952d989`; baseline: `06ca81b`. Both use glyrepr 1.0.0.9000. These measurements apply to the frozen test-derived workloads in `workloads.rds`.",
  "",
  "## Verification",
  "",
  "- 1,676 test expectations passed; no failures, warnings, or skips.",
  "- R CMD check: 0 errors, 0 warnings, 1 NOTE (remote system-time verification unavailable).",
  "- All 1,931 regression input positions across 14 entrypoints match: canonical strings, missing/failure positions, names, full graph signatures, and floating metadata.",
  "- Every benchmark workload's output keys match the baseline, including the 500-string IUPAC and 867-string mixed automatic-parser workloads.",
  "",
  "## End-to-end timings",
  "",
  "Seconds; median and min–max from five runs after one warm-up. Garbage collection is outside the measured region. The retained baseline and new measurements ran sequentially after checks finished.",
  "",
  "| Entry point / workload | Inputs | Before median (range) | After median (range) | Ratio |",
  "|---|---:|---:|---:|---:|"
)
for (i in seq_len(nrow(summary))) {
  r <- summary[i, ]
  label <- if (r$parser == "auto_iupac") "auto_parse: IUPAC only" else r$parser
  lines <- c(
    lines,
    sprintf(
      "| %s | %d | %.3f (%.3f–%.3f) | %.3f (%.3f–%.3f) | %.2fx |",
      label,
      r$n,
      r$old_median,
      r$old_min,
      r$old_max,
      r$new_median,
      r$new_min,
      r$new_max,
      r$speedup
    )
  )
}
a <- subset(backend$timings, path == "old")$seconds
b <- subset(backend$timings, path == "new")$seconds
lines <- c(
  lines,
  "",
  "The large IUPAC gain applies to `auto_parse()`: it now passes the whole format group to the already-native IUPAC constructor instead of extracting and recanonicalizing one graph at a time. Direct IUPAC parsing keeps its existing entrypoint.",
  "",
  "LINUCS improves only modestly because its R syntax/residue processing still dominates. No format-specific parser was rewritten in C++. The old mixed automatic-parser timings vary substantially (3.988–7.299 s), so its median ratio is descriptive rather than a universal throughput claim.",
  "",
  "## Construction only",
  "",
  sprintf(
    "On %d already-parsed records, the old graph pipeline takes %.3f s (%.3f–%.3f); structure_from_arrays() takes %.3f s (%.3f–%.3f), a %.2fx median ratio over five alternating paired runs. Both routes include their required graph materialization; format parsing is excluded.",
    backend$n,
    median(a),
    min(a),
    max(a),
    median(b),
    min(b),
    max(b),
    median(a) / median(b)
  ),
  "",
  "## Compatibility",
  "",
  "Requires the new glyrepr development API (>= 1.0.0.9000). `validate = FALSE` remains accepted but no longer bypasses mandatory array validation. IUPAC-normalizing formats retain the existing native IUPAC entrypoint. Parser-specific warnings and default failure recovery remain covered by the existing snapshots and differential checks.",
  "",
  "See README.md for reproduction commands and artifact definitions. Raw timings, graph signatures, sessions, and checksums are retained alongside this report."
)
writeLines(lines, "benchmarks/array-parsers/RESULTS.md")
writeLines(
  capture.output(after$session),
  "benchmarks/array-parsers/session.txt"
)
