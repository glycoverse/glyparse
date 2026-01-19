test_that("auto_parse correctly identifies and parses IUPAC-condensed format", {
  input <- "Gal(b1-3)GlcNAc(b1-4)Glc(a1-"
  result <- auto_parse(input)
  expected <- parse_iupac_condensed(input)
  expect_equal(as.character(result), as.character(expected))
})

test_that("auto_parse correctly identifies and parses IUPAC-short format", {
  input <- "Neu5Aca3Gala3(Fuca6)GlcNAcb-"
  result <- auto_parse(input)
  expected <- parse_iupac_short(input)
  expect_equal(as.character(result), as.character(expected))
})

test_that("auto_parse correctly identifies and parses IUPAC-extended format", {
  input <- "β-D-Galp-(1→3)[β-D-Galp-(1→4)-β-D-GlcpNAc-(1→6)]-α-D-GalpNAc-(1→"
  result <- auto_parse(input)
  expected <- parse_iupac_extended(input)
  expect_equal(as.character(result), as.character(expected))
})

test_that("auto_parse correctly identifies and parses pGlyco format", {
  input <- "(N(F)(N(H(H(N))(H(N(H))))))"
  result <- auto_parse(input)
  expected <- parse_pglyco_struc(input)
  expect_equal(as.character(result), as.character(expected))
})

test_that("auto_parse correctly identifies and parses StrucGP format", {
  input <- "A2B2C1D1E2F1fedD1E2edcbB5ba"
  result <- auto_parse(input)
  expected <- parse_strucgp_struc(input)
  expect_equal(as.character(result), as.character(expected))
})

test_that("auto_parse correctly identifies and parses Linear Code format", {
  input <- "Ma3(Ma6)Mb4GNb4GNb"
  result <- auto_parse(input)
  expected <- parse_linear_code(input)
  expect_equal(as.character(result), as.character(expected))
})

test_that("auto_parse works with mixed format vector", {
  mixed_input <- c(
    "Gal(b1-3)GlcNAc(b1-4)Glc(a1-",   # IUPAC-condensed
    "Neu5Aca3Gala3(Fuca6)GlcNAcb-"    # IUPAC-short
  )
  result <- auto_parse(mixed_input)
  expected <- c(
    parse_iupac_condensed(mixed_input[1]),
    parse_iupac_short(mixed_input[2])
  )
  expect_equal(as.character(result), as.character(expected))
})
