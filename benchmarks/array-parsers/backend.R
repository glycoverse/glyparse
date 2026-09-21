# Isolate construction after format parsing, including graph allocation in each
# route. The graph route reproduces the baseline's public low-level pipeline.
pkgload::load_all(quiet = TRUE)
workloads <- readRDS("benchmarks/array-parsers/workloads.rds")
parsers <- c(
  parse_pglyco_struc = "parse_pglyco_struc_arrays",
  parse_strucgp_struc = "parse_strucgp_struc_arrays",
  parse_kcf = "parse_kcf_arrays",
  parse_linucs = "parse_linucs_arrays",
  parse_glycoct = "parse_glycoct_arrays",
  parse_wurcs = "parse_wurcs_arrays"
)
records <- unlist(
  lapply(names(parsers), function(name) {
    lapply(
      head(workloads[[name]], 50),
      get(parsers[[name]], asNamespace("glyparse"))
    )
  }),
  recursive = FALSE
)
make_graph <- function(x) {
  g <- igraph::make_empty_graph(length(x$mono), directed = TRUE)
  if (length(x$edges)) {
    g <- igraph::add_edges(g, x$edges)
  }
  g <- igraph::set_vertex_attr(
    g,
    "name",
    value = as.character(seq_along(x$mono))
  )
  g <- igraph::set_vertex_attr(g, "mono", value = x$mono)
  g <- igraph::set_vertex_attr(g, "sub", value = x$sub)
  g <- igraph::set_edge_attr(g, "linkage", value = x$linkage)
  for (field in c(
    "anomer",
    "alditol",
    "floating_parts",
    "floating_substituents"
  )) {
    if (!is.null(x[[field]])) g <- igraph::set_graph_attr(g, field, x[[field]])
  }
  g
}
old <- function() {
  graphs <- lapply(records, function(x) {
    g <- glyrepr::validate_glycan_graph(make_graph(x))
    glyrepr::canonicalize_glycan_graph(g)
  })
  glyrepr::validate_glycan_graph_vector(graphs)
  keys <- vapply(graphs, glyrepr::graph_to_iupac, character(1))
  keep <- !duplicated(keys)
  glyrepr::new_glycan_structure(keys, stats::setNames(graphs[keep], keys[keep]))
}
new <- function() glyrepr::structure_from_arrays(records)
stopifnot(identical(as.character(old()), as.character(new())))
rows <- list()
for (i in 1:5) {
  for (path in if (i %% 2L) c("old", "new") else c("new", "old")) {
    gc()
    elapsed <- system.time(get(path)())[["elapsed"]]
    rows[[length(rows) + 1L]] <- data.frame(
      path = path,
      iteration = i,
      seconds = elapsed
    )
  }
}
saveRDS(
  list(
    n = length(records),
    timings = do.call(rbind, rows),
    session = sessionInfo()
  ),
  "benchmarks/array-parsers/backend-results.rds"
)
print(do.call(rbind, rows))
