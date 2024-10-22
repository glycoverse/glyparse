test_that("(N(F)(N(H(H(N))(H(N(H))))))", {
  skip_on_old_win()
  glycan <- parse_pglyco_struc("(N(F)(N(H(H(N))(H(N(H))))))")
  expect_snapshot(print(glycan, verbose = TRUE))
})


test_that("(N(F)(N(H(H(N(H)))(H(N(H(A)))))))", {
  skip_on_old_win()
  glycan <- parse_pglyco_struc("(N(F)(N(H(H(N(H)))(H(N(H(A)))))))")
  expect_snapshot(print(glycan, verbose = TRUE))
})


test_that("parse fails", {
  x <- "wrong format"
  expect_error(parse_pglyco_struc(x), "Could not be parsed.")
})


test_that("fails when x is not character", {
  expect_error(parse_pglyco_struc(1), "`x` must be a character vector.")
})


test_that("parse multiple glycans", {
  x <- c("(N(F)(N(H(H(N))(H(N(H))))))", "(N(F)(N(H(H(N(H)))(H(N(H(A)))))))")
  glycan <- parse_pglyco_struc(x)
  expect_length(glycan, 2)
  expect_snapshot(print(glycan[[1]], verbose = TRUE))
  expect_snapshot(print(glycan[[2]], verbose = TRUE))
})


test_that("parse multiple glycans with failures", {
  x <- c("(N(F)(N(H(H(N))(H(N(H))))))", "bad 1", "bad 2")
  expect_error(parse_pglyco_struc(x), 'These could not be parsed: "bad 1" and "bad 2"')
})
