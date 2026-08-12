test_that("IUPAC-condensed: some O-glycan", {
  skip_on_old_win()
  to_parse <- "Neu5Ac(a2-3)Gal(b1-4)[Fuc(a1-3)]GlcNAc(b1-6)[Neu5Ac(a2-3)Gal(b1-3)]GalNAc(b1-"
  glycan <- parse_iupac_condensed(to_parse)
  expect_snapshot(print(glycan, verbose = TRUE))
})

test_that("IUPAC-condensed can drop generic glycans", {
  input <- c("Hex(??-", "Glc(?1-", "HexNAc(??-")

  expect_snapshot(
    error = TRUE,
    parse_iupac_condensed(input)
  )
  expect_snapshot(
    result <- parse_iupac_condensed(
      input,
      drop_generic = TRUE
    )
  )

  expect_identical(
    as.character(result),
    c(NA_character_, "Glc(?1-", NA_character_)
  )
})

test_that("drop_generic drops generic-only parser output", {
  expect_snapshot(
    result <- parse_pglyco_struc(
      c("(N)", "(H)"),
      drop_generic = TRUE
    )
  )

  expect_identical(as.character(result), rep(NA_character_, 2))
})
