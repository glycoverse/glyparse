read_build <- function(build) {
  lapply(seq_len(5L), function(i) {
    readRDS(sprintf("benchmarks/native-parsers/%s-%s.rds", build, i))
  })
}
before <- read_build("before")
after <- read_build("after")
for (i in seq_len(5L)) {
  stopifnot(identical(before[[i]]$keys, after[[i]]$keys))
}
a <- do.call(rbind, lapply(before, `[[`, "timings"))
b <- do.call(rbind, lapply(after, `[[`, "timings"))
rows <- lapply(unique(a$parser), function(name) {
  old <- a[a$parser == name, ]
  new <- b[b$parser == name, ]
  data.frame(
    parser = name,
    n = old$n[[1]],
    valid = old$valid[[1]],
    before_min = min(old$seconds),
    before_median = median(old$seconds),
    before_max = max(old$seconds),
    after_min = min(new$seconds),
    after_median = median(new$seconds),
    after_max = max(new$seconds),
    speedup = median(old$seconds) / median(new$seconds)
  )
})
rows <- do.call(rbind, rows)
write.csv(
  rows,
  "benchmarks/native-parsers/timing-summary.csv",
  row.names = FALSE
)
print(rows, digits = 4)
