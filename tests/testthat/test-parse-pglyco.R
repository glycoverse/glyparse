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


test_that("single monosaccharide", {
  skip_on_old_win()
  glycan <- parse_pglyco_struc("(N)")
  expect_snapshot(print(glycan, verbose = TRUE))
})


test_that("pH and aH monosaccharides", {
  skip_on_old_win()
  glycan <- parse_pglyco_struc("(pH(aH))")
  expect_snapshot(print(glycan, verbose = TRUE))
})
