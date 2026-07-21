test_that("struc_parser_wrapper handles duplicate values efficiently", {
  # Test using actual parser function - pGlyco parser is simple
  # Create a vector with many duplicates
  input <- rep(c("(N)", "(H)", "(F)"), times = c(50, 30, 20))

  # Test that the result is correct
  result <- parse_pglyco_struc(input)
  expect_length(result, 100)

  # Test that all unique structures are represented correctly
  unique_input <- unique(input)
  unique_result <- parse_pglyco_struc(unique_input)
  expect_length(unique_result, 3)
})

test_that("struc_parser_wrapper parses each unique non-NA input once", {
  parser_calls <- character()
  parser <- function(x) {
    parser_calls <<- c(parser_calls, x)
    graph <- igraph::make_empty_graph(n = 2, directed = TRUE)
    igraph::V(graph)$name <- c("1", "2")
    igraph::V(graph)$mono <- c("Hex", "Hex")
    igraph::V(graph)$sub <- c("", "")
    graph <- igraph::add_edges(graph, c("2", "1"))
    igraph::E(graph)$linkage <- "a1-4"
    graph$anomer <- "a1"
    graph$alditol <- FALSE
    graph
  }

  input <- c("A", "B", NA, "A", "C", "B")

  result <- struc_parser_wrapper(input, parser, on_failure = "error")

  expect_length(result, length(input))
  expect_equal(parser_calls, c("A", "B", "C"))
})

test_that("struc_parser_wrapper checks each unique non-NA graph once", {
  validation_calls <- 0L
  canonicalization_calls <- 0L
  original_validate <- glyrepr::validate_glycan_graph
  original_canonicalize <- glyrepr::canonicalize_glycan_graph
  testthat::local_mocked_bindings(
    validate_glycan_graph = function(graph) {
      validation_calls <<- validation_calls + 1L
      original_validate(graph)
    },
    canonicalize_glycan_graph = function(graph) {
      canonicalization_calls <<- canonicalization_calls + 1L
      original_canonicalize(graph)
    },
    .package = "glyrepr"
  )

  input <- c("(N)", "(H)", NA, "(N)", "(H)", "(N)")

  result <- parse_pglyco_struc(input)

  expect_identical(
    as.character(result),
    c("HexNAc(??-", "Hex(??-", NA, "HexNAc(??-", "Hex(??-", "HexNAc(??-")
  )
  expect_equal(validation_calls, 2L)
  expect_equal(canonicalization_calls, 2L)
  expect_length(attr(result, "graphs"), 2L)
})

test_that("struc_parser_wrapper avoids high-level structure constructors", {
  testthat::local_mocked_bindings(
    as_glycan_structure = function(...) {
      stop("Unexpected high-level construction")
    },
    glycan_structure = function(...) {
      stop("Unexpected high-level construction")
    },
    .package = "glyrepr"
  )

  result <- parse_pglyco_struc(c("(N)", "(H)"))

  expect_identical(as.character(result), c("HexNAc(??-", "Hex(??-"))
})

test_that("struc_parser_wrapper deduplicates equivalent parsed graphs", {
  parser <- function(x) {
    graph <- igraph::make_empty_graph(n = 1, directed = TRUE)
    igraph::V(graph)$name <- "1"
    igraph::V(graph)$mono <- "Hex"
    igraph::V(graph)$sub <- ""
    igraph::E(graph)$linkage <- character()
    graph$anomer <- "??"
    graph
  }

  result <- struc_parser_wrapper(c("first", "second"), parser)

  expect_identical(as.character(result), rep("Hex(??-", 2))
  expect_length(attr(result, "graphs"), 1L)
})

test_that("struc_parser_wrapper preserves order with duplicates", {
  input <- c("(N)", "(H)", "(N)", "(F)", "(H)", "(N)")
  result <- parse_pglyco_struc(input)

  # Extract the actual igraph objects from glycan_structure
  graphs <- glyrepr::get_structure_graphs(result)

  # Check that results match the expected order
  mono_names <- sapply(graphs, function(g) igraph::V(g)$mono[1])
  expected <- c("HexNAc", "Hex", "HexNAc", "dHex", "Hex", "HexNAc")

  expect_equal(mono_names, expected)
})

test_that("struc_parser_wrapper works with all identical values", {
  input <- rep("(N)", 50)
  result <- parse_pglyco_struc(input)

  expect_length(result, 50)

  # Extract graphs and check that all are identical
  graphs <- glyrepr::get_structure_graphs(result)
  mono_names <- sapply(graphs, function(g) igraph::V(g)$mono[1])
  expect_true(all(mono_names == "HexNAc"))
})
