test_that("IUPAC-condensed: some O-glycan", {
  skip_on_old_win()
  to_parse <- "Neu5Ac(a2-3)Gal(b1-4)[Fuc(a1-3)]GlcNAc(b1-6)[Neu5Ac(a2-3)Gal(b1-3)]GalNAc"
  glycan <- do_parse_iupac_condensed(to_parse)
  expect_snapshot(print(glycan, verbose = TRUE))
})


test_that("IUPAC-condensed: H9N2", {
  skip_on_old_win()
  to_parse <- "Man(a1-2)Man(a1-2)Man(a1-3)[Man(a1-2)Man(a1-3)[Man(a1-2)Man(a1-6)]Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc"
  glycan <- do_parse_iupac_condensed(to_parse)
  expect_snapshot(print(glycan, verbose = TRUE))
})


test_that("IUPAC-condensed: unknown linkages", {
  skip_on_old_win()
  to_parse <- "Man(a1-?)Man(?1-3)Man(??-?)Man"
  glycan <- do_parse_iupac_condensed(to_parse)
  expect_snapshot(print(glycan, verbose = TRUE))
})
