# Root-independent identity for fully localized tree glycans. Retain each
# monosaccharide, substituent, anomer, and both endpoints of every linkage.
# Account for conventional roots and symmetric alditol carbon numbering.
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
  root <- setdiff(seq_along(anomer), child)
  alditol <- isTRUE(igraph::graph_attr(g, "alditol"))
  labels[root] <- paste0(labels[root], if (alditol) ":alditol" else "")
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
  identity <- min(vapply(seq_along(labels), visit, ""))
  # Mannitol's h1122h skeleton is identical on reversing the carbon chain
  # and reversing all tetrahedral orientations. Fully unknown hxxxxh is
  # likewise symmetric. Reverse every root attachment together, never each
  # position independently; this retains relative branch placement.
  if (alditol && v$mono[root] %in% c("Man", "Hex") && v$sub[root] == "") {
    at_root <- parent == root
    acceptor[at_root] <- as.character(7L - as.integer(acceptor[at_root]))
    identity <- min(identity, vapply(seq_along(labels), visit, ""))
  }
  identity
}
