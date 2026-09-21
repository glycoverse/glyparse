array_roots <- function(x) {
  children <- matrix(x$edges, ncol = 2L, byrow = TRUE)[, 2L]
  setdiff(seq_along(x$mono), children)
}

array_components <- function(x) {
  # Union-find also handles malformed cyclic inputs; glyrepr validates topology.
  parent <- seq_along(x$mono)
  root <- function(i) {
    while (parent[[i]] != i) {
      i <- parent[[i]]
    }
    i
  }
  if (length(x$edges)) {
    for (i in seq.int(1L, length(x$edges), by = 2L)) {
      a <- root(x$edges[[i]])
      b <- root(x$edges[[i + 1L]])
      parent[[b]] <- a
    }
  }
  vapply(seq_along(parent), root, integer(1))
}
