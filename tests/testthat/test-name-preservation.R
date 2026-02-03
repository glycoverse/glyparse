# Test that parse_iupac_condensed preserves names
test_that("parse_iupac_condensed preserves names from input", {
  skip_on_old_win()
  x <- c(a = "Gal(b1-4)GlcNAc(b1-", b = "Gal(b1-3)GalNAc(a1-")
  result <- parse_iupac_condensed(x)
  expect_equal(names(result), c("a", "b"))
})

# Test that auto_parse preserves names
test_that("auto_parse preserves names from input", {
  skip_on_old_win()
  x <- c(a = "Gal(b1-4)GlcNAc(b1-", b = "Gal(b1-3)GalNAc(a1-")
  result <- auto_parse(x)
  expect_equal(names(result), c("a", "b"))
})

# Test that unnamed input returns unnamed output
test_that("parse_iupac_condensed returns unnamed output for unnamed input", {
  skip_on_old_win()
  x <- c("Gal(b1-4)GlcNAc(b1-", "Gal(b1-3)GalNAc(a1-")
  result <- parse_iupac_condensed(x)
  expect_null(names(result))
})

# Test that names are preserved with duplicate values
test_that("parse_iupac_condensed preserves names with duplicate values", {
  skip_on_old_win()
  x <- c(a = "Gal(b1-4)GlcNAc(b1-", b = "Gal(b1-4)GlcNAc(b1-")
  result <- parse_iupac_condensed(x)
  expect_equal(names(result), c("a", "b"))
})

# Test that partial names work (some NA names)
test_that("parse_iupac_condensed preserves partial names", {
  skip_on_old_win()
  x <- c("Gal(b1-4)GlcNAc(b1-", b = "Gal(b1-3)GalNAc(a1-")
  names(x) <- c(NA, "b")
  result <- parse_iupac_condensed(x)
  expect_equal(names(result), c(NA, "b"))
})
