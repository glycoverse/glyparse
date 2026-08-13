test_that("furanose helpers cover every concrete monosaccharide", {
  map <- furanose_monosaccharide_map()
  concrete <- glyrepr::available_monosaccharides("concrete")

  expect_setequal(c(names(map), unname(map)), concrete)
  expect_identical(as_furanose_monosaccharide(names(map)), unname(map))
  expect_identical(as_ringless_monosaccharide(unname(map)), names(map))
})

test_that("IUPAC-condensed parses every furanose monosaccharide", {
  furanose <- unname(furanose_monosaccharide_map())
  anomer_pos <- glyrepr::get_anomer_pos(furanose)
  input <- paste0(furanose, "(?", anomer_pos, "-")

  parsed <- parse_iupac_condensed(input)

  expect_identical(as.character(parsed), input)
})

test_that("furanose aliases replace only pyranose ring markers", {
  glycam_map <- glycam_iupac_mono_map()
  linucs_map <- linucs_mono_stem_map()

  expect_identical(unname(glycam_map[["DApif"]]), "DApif")
  expect_identical(unname(glycam_map[["LApif"]]), "Apif")
  expect_identical(unname(linucs_map[["D-Apif"]]), "DApif")
  expect_identical(unname(linucs_map[["L-Apif"]]), "Apif")
  expect_false("DAfif" %in% names(glycam_map))
  expect_false("D-Afif" %in% names(linucs_map))
})
