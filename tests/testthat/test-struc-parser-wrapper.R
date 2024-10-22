simple_parser <- function(x) {
  paste0(x, "_parsed")
}


bad_parser <- function(x) {
  stop("error")
}


test_that("x is not a character", {
  expect_error(
    struc_parser_wrapper(1, simple_parser),
    "`x` must be a character vector."
  )
})


test_that("parser is not a function", {
  expect_error(
    struc_parser_wrapper("x", "not_a_function"),
    "`parser` must be a function."
  )
})


test_that("parsing single character", {
  expect_equal(
    struc_parser_wrapper("x", simple_parser),
    "x_parsed"
  )
})


test_that("parsing single character fails", {
  expect_error(
    struc_parser_wrapper("x", bad_parser),
    "Could not be parsed."
  )
})


test_that("parsing multiple characters", {
  expect_equal(
    struc_parser_wrapper(c("x", "y", "z"), simple_parser),
    list("x_parsed", "y_parsed", "z_parsed")
  )
})


test_that("parsing multiple characters with failures", {
  expect_error(
    struc_parser_wrapper(c("x", "y", "z"), bad_parser),
    'These could not be parsed: "x", "y", and "z"'
  )
})
