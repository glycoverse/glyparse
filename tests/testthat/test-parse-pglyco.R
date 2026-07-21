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


test_that("pGlyco graph assembly preserves branched topology", {
  graph <- do_parse_pglyco_struc(
    "(N(F)(N(H(H(N))(H(N(H))))))"
  )

  expect_identical(
    igraph::as_edgelist(graph, names = FALSE),
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
    igraph::vertex_attr(graph, "mono"),
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
    igraph::edge_attr(graph, "linkage"),
    rep("??-?", 8)
  )
})
