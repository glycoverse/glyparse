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