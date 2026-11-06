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


test_that("pGlyco array assembly preserves branched topology", {
  graph <- parse_pglyco_struc_arrays(
    "(N(F)(N(H(H(N))(H(N(H))))))"
  )

  expect_identical(
    matrix(as.double(graph$edges), ncol = 2, byrow = TRUE),
    matrix(
      c(
        1,
        2,
        1,
        3,
        3,
        4,
        4,
        5,
        5,
        6,
        4,
        7,
        7,
        8,
        8,
        9
      ),
      ncol = 2,
      byrow = TRUE
    )
  )
  expect_identical(
    graph$mono,
    c(
      "HexNAc",
      "dHex",
      "HexNAc",
      "Hex",
      "Hex",
      "HexNAc",
      "Hex",
      "HexNAc",
      "Hex"
    )
  )
  expect_identical(
    graph$linkage,
    rep("??-?", 8)
  )
})

test_that("pGlyco rejects unclosed branches", {
  invalid <- c("(H(N)", "(H", "(N(H)", "(N(H", "(N(F)(N(H))")
  expect_error(parse_pglyco_struc(invalid), "Can't parse")
  expect_true(all(is.na(parse_pglyco_struc(invalid, on_failure = "na"))))
  expect_true(all(is.na(auto_parse(invalid, on_failure = "na"))))

  input <- c(valid = "(N(H))", invalid = "(N(H)", missing = NA)
  result <- parse_pglyco_struc(input, on_failure = "na")
  expect_identical(names(result), names(input))
  expect_identical(unname(is.na(result)), c(FALSE, TRUE, TRUE))
  expect_identical(
    unname(as.character(result[1])),
    as.character(parse_pglyco_struc("(N(H))"))
  )
})
