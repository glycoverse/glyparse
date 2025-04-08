exp_with_struc_col <- function() {
  exp <- glyexp::toy_experiment()
  exp$var_info$glycan_structure <- c(
    "(N(N(H(H(H))(H(H)))))",
    "(N(N(H(H(H))(H(H)))))",
    "(N(N(H(H(H)))))",
    "(N(N(H(H(H)))))"
  )
  exp$meta_data$structure_type <- "pglyco"
  exp
}


test_that("add_structures works", {
  exp <- exp_with_struc_col()
  exp <- add_structures(exp)
  expect_snapshot(exp$glycan_structures)
})


test_that("override works", {
  exp <- exp_with_struc_col()
  exp$glycan_structures <- list(
    H5N2 = glyrepr::n_glycan_core(),
    H4N2 = glyrepr::n_glycan_core()
  )

  exp <- add_structures(exp, override = TRUE)
  expect_snapshot(exp$glycan_structures)
})


test_that("override set to FALSE raises error", {
  exp <- exp_with_struc_col()
  exp$glycan_structures <- list(
    H5N2 = glyrepr::n_glycan_core(),
    H4N2 = glyrepr::n_glycan_core()
  )

  expect_error(
    add_structures(exp, override = FALSE),
    "The experiment already has glycan structures."
  )
})


test_that("silence set to TRUE does not raise error", {
  exp <- exp_with_struc_col()
  exp$glycan_structures <- list(
    H5N2 = glyrepr::n_glycan_core(),
    H4N2 = glyrepr::n_glycan_core()
  )

  expect_silent(
    exp <- add_structures(exp, override = FALSE, silence = TRUE)
  )
  expect_snapshot(exp$glycan_structures)
})


test_that("no structure column raises error", {
  exp <- glyexp::toy_experiment()
  expect_error(
    add_structures(exp),
    "Structure column not found in the experiment."
  )
})


test_that("no structure column and aggregated to 'gf' raises error", {
  exp <- glyexp::toy_experiment()
  exp$meta_data$aggr_level <- "gf"
  expect_snapshot(add_structures(exp), error = TRUE)
})
