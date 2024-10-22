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
