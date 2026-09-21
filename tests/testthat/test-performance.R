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
    list(
      mono = c("Hex", "Hex"),
      sub = c("", ""),
      edges = c(2L, 1L),
      linkage = "a1-4",
      anomer = "a1",
      alditol = FALSE
    )
  }

  input <- c("A", "B", NA, "A", "C", "B")

  result <- struc_parser_wrapper(input, parser, on_failure = "error")

  expect_length(result, length(input))
  expect_equal(parser_calls, c("A", "B", "C"))
})

test_that("array parsers batch unique records without graph canonicalization", {
  batches <- list()
  original <- glyrepr::structure_from_arrays
  local_mocked_bindings(
    structure_from_arrays = function(x, ...) {
      batches[[length(batches) + 1L]] <<- x
      original(x, ...)
    },
    canonicalize_glycan_graphs = function(...) stop("Unexpected graph path"),
    canonicalize_glycan_graph = function(...) stop("Unexpected graph path"),
    .package = "glyrepr"
  )

  input <- c(first = "(N)", second = "(H)", missing = NA, duplicate = "(N)")
  result <- parse_pglyco_struc(input)

  expect_length(batches, 1L)
  expect_length(batches[[1]], 2L)
  expect_identical(names(result), names(input))
  expect_identical(
    unname(as.character(result)),
    c("HexNAc(??-", "Hex(??-", NA, "HexNAc(??-")
  )
  expect_length(attr(result, "graphs"), 2L)
})

test_that("validate compatibility flag retains the array backend", {
  local_mocked_bindings(
    canonicalize_glycan_graphs = function(...) stop("Unexpected graph path"),
    .package = "glyrepr"
  )
  input <- c("(N)", "(H)", "(N)", NA_character_)
  expect_equal(
    as.character(parse_pglyco_struc(input, validate = FALSE)),
    as.character(parse_pglyco_struc(input))
  )
})

test_that("graph parsers expose validate", {
  parsers <- c(
    "parse_glycoct",
    "parse_kcf",
    "parse_linucs",
    "parse_pglyco_struc",
    "parse_strucgp_struc",
    "parse_wurcs"
  )

  for (parser in parsers) {
    expect_identical(formals(get(parser))$validate, TRUE, info = parser)
  }
})

test_that("no parser exposes drop_generic", {
  parsers <- c(
    "auto_parse",
    "parse_glycam_iupac",
    "parse_glycoct",
    "parse_gwb",
    "parse_iupac_compact",
    "parse_iupac_condensed",
    "parse_iupac_extended",
    "parse_iupac_short",
    "parse_kcf",
    "parse_linear_code",
    "parse_linucs",
    "parse_pglyco_struc",
    "parse_strucgp_struc",
    "parse_wurcs"
  )

  for (parser in parsers) {
    expect_null(formals(get(parser))$drop_generic, info = parser)
  }
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
    list(
      mono = "Hex",
      sub = "",
      edges = integer(),
      linkage = character(),
      anomer = "??"
    )
  }

  result <- struc_parser_wrapper(c("first", "second"), parser)

  expect_identical(as.character(result), rep("Hex(??-", 2))
  expect_length(attr(result, "graphs"), 1L)
})

test_that("normalized parsers pass one complete unique vector to glyrepr", {
  constructor_inputs <- list()
  original_constructor <- glyrepr::as_glycan_structure
  testthat::local_mocked_bindings(
    as_glycan_structure = function(x, ...) {
      constructor_inputs[[length(constructor_inputs) + 1L]] <<- x
      original_constructor(x, ...)
    },
    .package = "glyrepr"
  )

  input <- c(
    first = "Gal(b1-3)Glc(a1-",
    second = "Glc(a1-",
    duplicate = "Gal(b1-3)Glc(a1-",
    missing = NA_character_
  )
  result <- parse_iupac_condensed(input)

  expect_length(constructor_inputs, 1L)
  expect_identical(
    constructor_inputs[[1]],
    c("Gal(b1-3)Glc(a1-", "Glc(a1-")
  )
  expect_identical(names(result), names(input))
  expect_identical(
    unname(as.character(result)),
    c("Gal(b1-3)Glc(a1-", "Glc(a1-", "Gal(b1-3)Glc(a1-", NA_character_)
  )
})

test_that("normalization wrapper vectorizes unique inputs", {
  normalizer_inputs <- list()
  normalizer <- function(x) {
    normalizer_inputs[[length(normalizer_inputs) + 1L]] <<- x
    dplyr::recode_values(
      x,
      "A" ~ "Gal(b1-3)Glc(a1-",
      "B" ~ "Glc(a1-"
    )
  }

  result <- normalized_struc_parser_wrapper(c("A", "B", "A"), normalizer)

  expect_length(normalizer_inputs, 1L)
  expect_identical(normalizer_inputs[[1]], c("A", "B"))
  expect_identical(
    as.character(result),
    c("Gal(b1-3)Glc(a1-", "Glc(a1-", "Gal(b1-3)Glc(a1-")
  )
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
