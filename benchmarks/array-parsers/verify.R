before <- readRDS("benchmarks/array-parsers/reference.rds")$results
after <- readRDS("benchmarks/array-parsers/current.rds")$results
rows <- lapply(names(before), function(name) {
  a <- before[[name]]
  b <- after[[name]]
  data.frame(
    parser = name,
    positions = length(a$keys),
    valid = sum(!is.na(a$keys)),
    keys = identical(a$keys, b$keys),
    graphs = isTRUE(all.equal(a$graphs, b$graphs))
  )
})
rows <- do.call(rbind, rows)
stopifnot(all(rows$keys), all(rows$graphs))
write.csv(rows, "benchmarks/array-parsers/parity.csv", row.names = FALSE)
print(rows)
