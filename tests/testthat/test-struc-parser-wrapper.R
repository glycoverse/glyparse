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
