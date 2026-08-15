test_that("IUPAC-condensed: some O-glycan", {
  skip_on_old_win()
  to_parse <- "Neu5Ac(a2-3)Gal(b1-4)[Fuc(a1-3)]GlcNAc(b1-6)[Neu5Ac(a2-3)Gal(b1-3)]GalNAc(b1-"
  glycan <- parse_iupac_condensed(to_parse)
  expect_snapshot(print(glycan, verbose = TRUE))
})

test_that("IUPAC-condensed preserves unusual configurations", {
  parsed <- parse_iupac_condensed(
    "D-Fuc3S(a1-2)[L-Gul(b1-3)]D-Fucf(?1-"
  )

  expect_identical(
    as.character(parsed),
    "D-Fuc3S(a1-2)[L-Gul(b1-3)]D-Fucf(?1-"
  )
})

test_that("IUPAC-condensed preserves alditols", {
  parsed <- parse_iupac_condensed("Gal(b1-4)GlcNAc-ol(a1-")

  expect_identical(as.character(parsed), "Gal(b1-4)GlcNAc-ol(a1-")
  expect_identical(unname(glyrepr::get_alditol(parsed)), TRUE)
})

test_that("IUPAC-condensed preserves mixed monosaccharide types", {
  input <- c(
    mixed = "Gal(?1-?)HexNAc(?1-",
    generic = "Hex(??-",
    concrete = "Glc(?1-",
    missing = NA_character_
  )
  result <- parse_iupac_condensed(input)

  expect_identical(as.character(result), input)
  expect_identical(
    glyrepr::get_mono_type(result),
    c(
      mixed = "mixed",
      generic = "generic",
      concrete = "concrete",
      missing = NA_character_
    )
  )
})
