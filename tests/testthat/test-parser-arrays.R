test_that("array topology retains isolated nodes and disconnected components", {
  record <- list(mono = rep("Hex", 6), edges = c(4L, 2L, 2L, 3L, 6L, 5L))
  expect_identical(array_roots(record), c(1L, 4L, 6L))
  groups <- split(seq_along(record$mono), array_components(record))
  expect_equal(unname(groups), list(1L, 2:4, 5:6))
})

test_that("array component discovery terminates for cyclic input", {
  record <- list(mono = rep("Hex", 3), edges = c(1L, 2L, 2L, 3L, 3L, 1L))
  expect_identical(array_roots(record), integer())
  expect_length(unique(array_components(record)), 1L)
})

test_that("array topology handles single residues without edges", {
  record <- list(mono = "Hex", edges = integer())
  expect_identical(array_roots(record), 1L)
  expect_identical(array_components(record), 1L)
})
