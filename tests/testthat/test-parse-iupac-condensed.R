test_that("IUPAC-condensed: some O-glycan", {
  skip_on_old_win()
  to_parse <- "Neu5Ac(a2-3)Gal(b1-4)[Fuc(a1-3)]GlcNAc(b1-6)[Neu5Ac(a2-3)Gal(b1-3)]GalNAc"
  glycan <- parse_iupac_condensed(to_parse)
  expect_snapshot(print(glycan, verbose = TRUE))
})


test_that("IUPAC-condensed: H9N2", {
  skip_on_old_win()
  to_parse <- "Man(a1-2)Man(a1-2)Man(a1-3)[Man(a1-2)Man(a1-3)[Man(a1-2)Man(a1-6)]Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc"
  glycan <- parse_iupac_condensed(to_parse)
  expect_snapshot(print(glycan, verbose = TRUE))
})


test_that("IUPAC-condensed: unknown linkages", {
  skip_on_old_win()
  to_parse <- "Man(a1-?)Man(?1-3)Man(??-?)Man"
  glycan <- parse_iupac_condensed(to_parse)
  expect_snapshot(print(glycan, verbose = TRUE))
})


test_that("IUPAC-condensed: unknown linkages", {
  skip_on_old_win()
  to_parse <- "Neu5Ac(a2-3/6)Gal"
  glycan <- parse_iupac_condensed(to_parse)
  expect_snapshot(print(glycan, verbose = TRUE))
})


test_that("IUPAC-condensed: single monosaccharide", {
  skip_on_old_win()
  to_parse <- "Man"
  glycan <- parse_iupac_condensed(to_parse)
  expect_snapshot(print(glycan, verbose = TRUE))
})


test_that("IUPAC-condensed: substituent", {
  skip_on_old_win()
  to_parse <- "Man3S(a1-2)Gal6Ac"
  glycan <- parse_iupac_condensed(to_parse)
  expect_snapshot(print(glycan, verbose = TRUE))
})


test_that("IUPAC-condensed: Neu5Ac", {
  skip_on_old_win()
  to_parse <- "Neu5Ac"
  glycan <- parse_iupac_condensed(to_parse)
  expect_snapshot(print(glycan, verbose = TRUE))
})


test_that("IUPAC-condensed: Neu5Ac9Ac", {
  skip_on_old_win()
  to_parse <- "Neu5Ac9Ac"
  glycan <- parse_iupac_condensed(to_parse)
  expect_snapshot(print(glycan, verbose = TRUE))
})


test_that("IUPAC-condensed: Neu5Ac9NAc", {
  skip_on_old_win()
  to_parse <- "Neu5Ac9NAc"
  glycan <- parse_iupac_condensed(to_parse)
  expect_snapshot(print(glycan, verbose = TRUE))
})


test_that("IUPAC-condensed: Neu5Ac(a2-", {
  skip_on_old_win()
  to_parse <- "Neu5Ac(a2-"
  glycan <- parse_iupac_condensed(to_parse)
  expect_snapshot(print(glycan, verbose = TRUE))
})


test_that("IUPAC-condensed: Neu5Ac(a?-", {
  skip_on_old_win()
  to_parse <- "Neu5Ac(a?-"
  glycan <- parse_iupac_condensed(to_parse)
  expect_snapshot(print(glycan, verbose = TRUE))
})


test_that("IUPAC-condensed: Neu5Ac?Ac", {
  skip_on_old_win()
  to_parse <- "Neu5Ac?Ac"
  glycan <- parse_iupac_condensed(to_parse)
  expect_snapshot(print(glycan, verbose = TRUE))
})


test_that("IUPAC-condensed: GlcNAc(b1-4)GlcNAc-ol", {
  skip_on_old_win()
  to_parse <- "GlcNAc(b1-4)GlcNAc-ol"
  glycan <- parse_iupac_condensed(to_parse)
  expect_true(glycan$alditol)
})
