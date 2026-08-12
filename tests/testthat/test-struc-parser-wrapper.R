test_that("struc_parser_wrapper returns glyrepr-compatible vectors", {
  skip_on_old_win()

  parsed <- parse_strucgp_struc(
    "A2B2C1D1E2F1G3gfF5fedD1E2F1feE2F1fedcba"
  )
  reconstructed <- glyrepr::as_glycan_structure(
    glyrepr::get_structure_graphs(parsed)
  )

  expect_identical(typeof(parsed), typeof(reconstructed))

  parsed_tbl <- dplyr::tibble(glycan_structure = parsed)
  unique_tbl <- dplyr::distinct(parsed_tbl, .data$glycan_structure)

  expect_error(
    dplyr::left_join(parsed_tbl, unique_tbl, by = "glycan_structure"),
    NA
  )
})

test_that("struc_parser_wrapper treats invalid parsed graphs as failures", {
  parser <- function(x) {
    graph <- igraph::make_empty_graph(n = 1, directed = TRUE)
    igraph::V(graph)$name <- "1"
    igraph::V(graph)$mono <- "Hex"
    igraph::V(graph)$sub <- ""
    igraph::E(graph)$linkage <- character()
    graph$anomer <- "??"

    if (x == "invalid") {
      graph <- igraph::as_undirected(graph)
    }

    graph
  }

  result <- struc_parser_wrapper(
    c("valid", "invalid"),
    parser,
    on_failure = "na"
  )

  expect_identical(as.character(result), c("Hex(??-", NA_character_))
})

test_that("struc_parser_wrapper drops generic glycans by input position", {
  input <- c(
    generic = "Hex(??-",
    concrete = "Glc(?1-",
    generic_duplicate = "Hex(??-",
    missing = NA_character_
  )

  expect_snapshot(
    error = TRUE,
    struc_parser_wrapper(input, do_parse_iupac_condensed)
  )
  expect_snapshot(
    result <- struc_parser_wrapper(
      input,
      do_parse_iupac_condensed,
      drop_generic = TRUE
    )
  )

  expect_identical(
    unname(as.character(result)),
    c(NA_character_, "Glc(?1-", NA_character_, NA_character_)
  )
  expect_identical(names(result), names(input))
})
