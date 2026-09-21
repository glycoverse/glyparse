x <- read.csv("benchmarks/native-parsers/timing-summary.csv")
lines <- c(
  "# Native parsing results",
  "",
  "Warm end-to-end measurements against the R-parser revision `1cc34cd`, using identical glyrepr and dependency installations. Five alternating rounds, three calls per measured workload; values below are seconds per complete vector call. The ranges reflect the five retained rounds, not confidence intervals.",
  "",
  "| Parser | Inputs | R median (range) | Native median (range) | Speedup |",
  "|---|---:|---:|---:|---:|"
)
for (i in seq_len(nrow(x))) {
  r <- x[i, ]
  lines <- c(
    lines,
    sprintf(
      "| `%s` | %s | %.4f (%.4f–%.4f) | %.4f (%.4f–%.4f) | %.2f× |",
      r$parser,
      r$n,
      r$before_median,
      r$before_min,
      r$before_max,
      r$after_median,
      r$after_min,
      r$after_max,
      r$speedup
    )
  )
}
lines <- c(
  lines,
  "",
  "All timed canonical outputs agree across builds and rounds. Separately, 1,931 named positions and complete graph signatures agree across 14 public entrypoints, as do 959 additional perturbed inputs. Condensed IUPAC keeps glyrepr's native parser; Linear Code retains the faster baseline R implementation. These unchanged paths serve as controls. See README.md for corpus selection, build isolation, limitations and reproduction commands."
)
writeLines(lines, "benchmarks/native-parsers/RESULTS.md")
