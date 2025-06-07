# This is a simple parser, that treats x as a linear NE glycan graph.
# "NNH" will be parsed as a linear graph with 3 nodes.
simple_parser <- function(x) {
  graph <- igraph::make_empty_graph(n = stringr::str_length(x), directed = TRUE)
  graph <- igraph::add_edges(graph, c(1:(igraph::vcount(graph) - 1), 2:igraph::vcount(graph)))
  igraph::V(graph)$name <- as.character(1:igraph::vcount(graph))
  igraph::V(graph)$mono <- stringr::str_split(x, "")[[1]]
  igraph::V(graph)$sub <- ""
  igraph::E(graph)$linkage <- "??-?"
  graph$anomer <- "??"
  graph$alditol <- FALSE
  glyrepr::as_glycan_graph(graph)
}


bad_parser <- function(x) {
  stop("error")
}


test_that("x is not a character", {
  expect_error(struc_parser_wrapper(1, simple_parser))
})


test_that("parser is not a function", {
  expect_error(struc_parser_wrapper("x", "not_a_function"))
})


test_that("parsing single character", {
  expect_s3_class(
    struc_parser_wrapper("HHN", simple_parser),
    "glycan_graph"
  )
})


test_that("parsing single character fails", {
  expect_error(
    struc_parser_wrapper("HHN", bad_parser),
    "Could not be parsed."
  )
})


test_that("parsing multiple characters", {
  result <- struc_parser_wrapper(c("HHN", "NNH"), simple_parser)
  expect_s3_class(result[[1]], "glycan_graph")
  expect_s3_class(result[[2]], "glycan_graph")
})


test_that("parsing multiple characters with failures", {
  expect_error(
    struc_parser_wrapper(c("x", "y", "z"), bad_parser),
    'These could not be parsed: "x", "y", and "z"'
  )
})
