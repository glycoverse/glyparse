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
  input <- c(
    "WURCS=2.0/3,3,2/[a2122h-1b_1-5_2*NCC/3=O][a1122h-1b_1-5][a1122h-1a_1-5]/1-2-3/a3-b1_b4-c1",
    NA
  )
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

parser_failure_cases <- list(
  parse_pglyco_struc = list(
    valid = "(N(F)(N(H)))",
    invalid = "(N(X))"
  ),
  parse_strucgp_struc = list(
    valid = "A2B2C1D1E2F1fedD1E2edcbB5ba",
    invalid = "not_valid"
  ),
  parse_iupac_condensed = list(
    valid = "Gal(b1-3)GlcNAc(b1-4)Glc(a1-",
    invalid = "Gal(b1-3)Bogus(b1-4)Glc(a1-"
  ),
  parse_iupac_extended = list(
    valid = "beta-D-Galp-(1->3)-alpha-D-GalpNAc-(1->",
    invalid = "beta-D-Bogusp-(1->3)-alpha-D-GalpNAc-(1->"
  ),
  parse_iupac_short = list(
    valid = "Neu5Aca3Gala3(Fuca6)GlcNAcb-",
    invalid = "Bogusa3GlcNAcb-"
  ),
  parse_wurcs = list(
    valid = paste0(
      "WURCS=2.0/3,3,2/",
      "[a2122h-1b_1-5_2*NCC/3=O]",
      "[a1122h-1b_1-5]",
      "[a1122h-1a_1-5]/1-2-3/a3-b1_b4-c1"
    ),
    invalid = "WURCS=2.0/1,1,0/[axxxxxh-1a_1-5]/1/"
  ),
  parse_linear_code = list(
    valid = "Ma3(Ma6)Mb4GNb4GNb",
    invalid = "Zz"
  ),
  parse_glycoct = list(
    valid = "RES\n1b:b-dglc-HEX-1:5\nLIN\n1:1d(2+1)2n",
    invalid = "LIN\n1:1d(2+1)2n"
  ),
  auto_parse = list(
    valid = "Gal(b1-3)GlcNAc(b1-4)Glc(a1-",
    invalid = "Gal(b1-3)Bogus(b1-4)Glc(a1-"
  )
)

purrr::iwalk(parser_failure_cases, function(case, parser_name) {
  test_that(glue::glue("{parser_name} errors on invalid input by default"), {
    skip_if_not_installed("glyrepr", minimum_version = "0.10.0")

    parser <- get(parser_name)

    err <- rlang::catch_cnd(parser(c(case$valid, case$invalid)), classes = "error")

    expect_s3_class(err, "error")
    expect_match(conditionMessage(err), "Can't parse", fixed = TRUE)
  })

  test_that(glue::glue("{parser_name} returns NA for invalid input with on_failure = 'na'"), {
    skip_if_not_installed("glyrepr", minimum_version = "0.10.0")

    parser <- get(parser_name)
    input <- c(valid = case$valid, invalid = case$invalid)

    result <- parser(input, on_failure = "na")

    expect_equal(length(result), 2)
    expect_equal(names(result), names(input))
    expect_false(is.na(result[["valid"]]))
    expect_true(is.na(result[["invalid"]]))
  })
})

test_that("parser functions validate on_failure", {
  skip_if_not_installed("glyrepr", minimum_version = "0.10.0")

  expect_error(
    parse_iupac_condensed("Gal(b1-3)GlcNAc(b1-4)Glc(a1-", on_failure = "ignore"),
    "`on_failure`"
  )
})
