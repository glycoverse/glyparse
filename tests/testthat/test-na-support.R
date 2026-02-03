test_that("parse_strucgp_struc handles single NA", {
  skip_if_not_installed("glyrepr", minimum_version = "0.10.0")
  result <- parse_strucgp_struc(NA_character_)
  expect_equal(length(result), 1)
  expect_true(is.na(result[1]))
})

test_that("parse_strucgp_struc handles mixed input with NA", {
  skip_if_not_installed("glyrepr", minimum_version = "0.10.0")
  input <- c("A2B2C1D1E2F1fedD1E2edcbB5ba", NA, "A2B2C1D1E2F1fedD1E2edcbB5ba")
  result <- parse_strucgp_struc(input)
  expect_equal(length(result), 3)
  expect_false(is.na(result[1]))
  expect_true(is.na(result[2]))
  expect_false(is.na(result[3]))
})

test_that("parse_strucgp_struc handles all NA input", {
  skip_if_not_installed("glyrepr", minimum_version = "0.10.0")
  result <- parse_strucgp_struc(c(NA, NA))
  expect_equal(length(result), 2)
  expect_true(all(is.na(result)))
})

test_that("parse_strucgp_struc preserves names with NA", {
  skip_if_not_installed("glyrepr", minimum_version = "0.10.0")
  input <- c(struct1 = "A2B2C1D1E2F1fedD1E2edcbB5ba", missing = NA)
  result <- parse_strucgp_struc(input)
  expect_equal(names(result), c("struct1", "missing"))
})

test_that("parse_pglyco_struc handles NA", {
  skip_if_not_installed("glyrepr", minimum_version = "0.10.0")
  input <- c("(N(F)(N(H)))", NA)
  result <- parse_pglyco_struc(input)
  expect_equal(length(result), 2)
  expect_false(is.na(result[1]))
  expect_true(is.na(result[2]))
})

test_that("parse_iupac_condensed handles NA", {
  skip_if_not_installed("glyrepr", minimum_version = "0.10.0")
  input <- c("Gal(b1-3)GlcNAc(b1-4)Glc(a1-", NA)
  result <- parse_iupac_condensed(input)
  expect_equal(length(result), 2)
  expect_false(is.na(result[1]))
  expect_true(is.na(result[2]))
})

test_that("parse_iupac_extended handles NA", {
  skip_if_not_installed("glyrepr", minimum_version = "0.10.0")
  input <- c("α-D-Galp-(1→", NA)
  result <- parse_iupac_extended(input)
  expect_equal(length(result), 2)
  expect_false(is.na(result[1]))
  expect_true(is.na(result[2]))
})

test_that("parse_iupac_short handles NA", {
  skip_if_not_installed("glyrepr", minimum_version = "0.10.0")
  input <- c("GlcNAcb-", NA)
  result <- parse_iupac_short(input)
  expect_equal(length(result), 2)
  expect_false(is.na(result[1]))
  expect_true(is.na(result[2]))
})

test_that("parse_wurcs handles NA", {
  skip_if_not_installed("glyrepr", minimum_version = "0.10.0")
  input <- c("WURCS=2.0/3,3,2/[a2122h-1b_1-5_2*NCC/3=O][a1122h-1b_1-5][a1122h-1a_1-5]/1-2-3/a3-b1_b4-c1", NA)
  result <- parse_wurcs(input)
  expect_equal(length(result), 2)
  expect_false(is.na(result[1]))
  expect_true(is.na(result[2]))
})

test_that("parse_linear_code handles NA", {
  skip_if_not_installed("glyrepr", minimum_version = "0.10.0")
  input <- c("Ma3(Ma6)Mb4GNb4GNb", NA)
  result <- parse_linear_code(input)
  expect_equal(length(result), 2)
  expect_false(is.na(result[1]))
  expect_true(is.na(result[2]))
})

test_that("parse_glycoct handles NA", {
  skip_if_not_installed("glyrepr", minimum_version = "0.10.0")
  input <- c("RES\n1b:b-dglc-HEX-1:5\nLIN\n1:1d(2+1)2n", NA)
  result <- parse_glycoct(input)
  expect_equal(length(result), 2)
  expect_false(is.na(result[1]))
  expect_true(is.na(result[2]))
})

test_that("auto_parse handles NA", {
  skip_if_not_installed("glyrepr", minimum_version = "0.10.0")
  input <- c("Gal(b1-3)GlcNAc(b1-4)Glc(a1-", NA)
  result <- auto_parse(input)
  expect_equal(length(result), 2)
  expect_false(is.na(result[1]))
  expect_true(is.na(result[2]))
})

test_that("invalid structures still error, not treated as NA", {
  skip_if_not_installed("glyrepr", minimum_version = "0.10.0")
  expect_error(
    parse_strucgp_struc(c("valid_structure", "invalid")),
    "must have no NA in vertex attribute"
  )
})

test_that("mixed valid and invalid structures error", {
  skip_if_not_installed("glyrepr", minimum_version = "0.10.0")
  expect_error(
    parse_strucgp_struc(c("A2B2C1D1E2F1fedD1E2edcbB5ba", "not_valid")),
    "must have no NA in vertex attribute"
  )
})
