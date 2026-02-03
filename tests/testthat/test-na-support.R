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
