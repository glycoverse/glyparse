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


test_that("StrucGP graph assembly preserves branched topology", {
  graph <- do_parse_strucgp_struc(
    "A2B2C1D1E2F1fedD1E2edcbB5ba"
  )

  expect_identical(
    igraph::as_edgelist(graph, names = TRUE),
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
    igraph::vertex_attr(graph, "mono"),
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
    igraph::edge_attr(graph, "linkage"),
    rep("??-?", 8)
  )
})
