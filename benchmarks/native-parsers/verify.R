before <- readRDS("benchmarks/native-parsers/reference.rds")$results
after <- readRDS("benchmarks/native-parsers/current.rds")$results
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
write.csv(rows, "benchmarks/native-parsers/parity.csv", row.names = FALSE)
print(rows)
mutations_before <- readRDS("benchmarks/native-parsers/mutations-before.rds")
mutations_after <- readRDS("benchmarks/native-parsers/mutations-after.rds")
stopifnot(identical(mutations_before, mutations_after))
cat(sum(lengths(mutations_before)), "perturbed-input results also agree.\n")
