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


test_that("StrucGP array assembly preserves branched topology", {
  graph <- parse_strucgp_struc_arrays(
    "A2B2C1D1E2F1fedD1E2edcbB5ba"
  )

  expect_identical(
    matrix(as.character(graph$edges), ncol = 2, byrow = TRUE),
    matrix(
      c(
        "1",
        "2",
        "2",
        "3",
        "3",
        "4",
        "4",
        "5",
        "5",
        "6",
        "3",
        "7",
        "7",
        "8",
        "1",
        "9"
      ),
      ncol = 2,
      byrow = TRUE
    )
  )
  expect_identical(
    graph$mono,
    c(
      "HexNAc",
      "HexNAc",
      "Hex",
      "Hex",
      "HexNAc",
      "Hex",
      "Hex",
      "HexNAc",
      "dHex"
    )
  )
  expect_identical(
    graph$linkage,
    rep("??-?", 8)
  )
})

test_that("StrucGP rejects unmatched markers and skipped content", {
  invalid <- c(
    "A1B2bX", "A1B2b", "A1B2a", "A1B2baB3b", "A1B2bXB3ba"
  )
  expect_error(parse_strucgp_struc(invalid), "Can't parse")
  expect_true(all(is.na(parse_strucgp_struc(invalid, on_failure = "na"))))
  expect_true(all(is.na(auto_parse(invalid, on_failure = "na"))))

  input <- c(valid = "A1B2bB3ba", invalid = "A1B2bXB3ba", missing = NA)
  result <- parse_strucgp_struc(input, on_failure = "na")
  expect_identical(names(result), names(input))
  expect_identical(unname(is.na(result)), c(FALSE, TRUE, TRUE))
  expect_equal(unname(result[1]), parse_strucgp_struc("A1B2bB3ba"))
})
