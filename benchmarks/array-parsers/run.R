# Rscript benchmarks/array-parsers/run.R CHECKOUT OUTPUT.rds
args <- commandArgs(TRUE)
root <- normalizePath(args[[1]])
inputs <- readRDS("benchmarks/array-parsers/inputs.rds")
pkgload::load_all(root, quiet = TRUE)
signature <- function(g) {
  if (is.null(g)) {
    return(NULL)
  }
  attrs <- igraph::graph_attr(g)
  vertices <- igraph::vertex_attr(g)
  edges <- igraph::edge_attr(g)
  list(
    attributes = attrs[sort(names(attrs))],
    vertices = vertices[sort(names(vertices))],
    edges = igraph::as_edgelist(g, names = FALSE),
    edge_attributes = edges[sort(names(edges))]
  )
}
results <- list()
for (name in names(inputs)) {
  parser <- getExportedValue("glyparse", name)
  x <- inputs[[name]]
  # Exercise position restoration, names, duplicates, and missing elements.
  x <- c(x, x[seq_len(min(3L, length(x)))], NA_character_)
  names(x) <- paste0("input", seq_along(x))
  value <- suppressWarnings(parser(x, on_failure = "na"))
  results[[name]] <- list(
    input = x,
    keys = as.character(value),
    graphs = lapply(attr(value, "graphs"), signature)
  )
  cat(name, length(x), sum(!is.na(value)), "\n")
}
saveRDS(list(results = results, session = sessionInfo()), args[[2]])
