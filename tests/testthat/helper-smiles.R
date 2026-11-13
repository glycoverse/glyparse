# Root-independent identity for fully localized tree glycans. Retain each
# monosaccharide, substituent, anomer, and both endpoints of every linkage.
# This is needed only for non-reducing glycans, whose root is conventional.
smiles_graph_identity <- function(x) {
  g <- glyrepr::get_structure_graphs(x)
  v <- igraph::vertex_attr(g)
  e <- igraph::as_data_frame(g)
  anomer <- rep(NA_character_, igraph::vcount(g))
  parent <- as.integer(e$from)
  child <- as.integer(e$to)
  anomer[child] <- sub("-.*", "", e$linkage)
  anomer[setdiff(seq_along(anomer), child)] <- igraph::graph_attr(g, "anomer")
  labels <- paste(v$mono, v$sub, anomer, sep = ":")
  donor <- sub("^[ab?]", "", sub("-.*", "", e$linkage))
  acceptor <- sub(".*-", "", e$linkage)
  visit <- function(node, previous = 0L) {
    idx <- which(
      (parent == node & child != previous) |
        (child == node & parent != previous)
    )
    tails <- vapply(
      idx,
      function(i) {
        if (parent[i] == node) {
          paste(acceptor[i], donor[i], visit(child[i], node), sep = ":")
        } else {
          paste(donor[i], acceptor[i], visit(parent[i], node), sep = ":")
        }
      },
      ""
    )
    paste0(labels[node], "{", paste(sort(tails), collapse = ";"), "}")
  }
  min(vapply(seq_along(labels), visit, ""))
}
