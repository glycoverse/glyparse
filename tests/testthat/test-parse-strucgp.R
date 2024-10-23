test_that("A2B2C1D1E2F1G3gfF5fedD1E2F1feE2F1fedcba", {
  skip_on_old_win()
  glycan <- parse_strucgp_struc("A2B2C1D1E2F1G3gfF5fedD1E2F1feE2F1fedcba")
  expect_snapshot(print(glycan, verbose = TRUE))
})


test_that("A2B2C1D1E1F1fedD1E1F1feE1F1fedcba", {
  skip_on_old_win()
  glycan <- parse_strucgp_struc("A2B2C1D1E1F1fedD1E1F1feE1F1fedcba")
  expect_snapshot(print(glycan, verbose = TRUE))
})
