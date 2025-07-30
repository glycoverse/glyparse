test_that("IUPAC-condensed: some O-glycan", {
  skip_on_old_win()
  to_parse <- "Neu5Ac(a2-3)Gal(b1-4)[Fuc(a1-3)]GlcNAc(b1-6)[Neu5Ac(a2-3)Gal(b1-3)]GalNAc(b1-"
  glycan <- parse_iupac_condensed(to_parse)
  expect_snapshot(print(glycan, verbose = TRUE))
})
