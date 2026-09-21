test_that("native topology rejects disconnected structures", {
  input <- "WURCS=2.0/1,6,3/[a2122h-1a_1-5]/1-1-1-1-1-1/d4-b1_b4-c1_f4-e1"
  expect_identical(is.na(parse_wurcs(input, on_failure = "na")), TRUE)
})

test_that("native component discovery terminates for cyclic input", {
  input <- "WURCS=2.0/1,3,3/[a2122h-1a_1-5]/1-1-1/a4-b1_b4-c1_c4-a1"
  expect_identical(is.na(parse_wurcs(input, on_failure = "na")), TRUE)
})

test_that("native topology handles single residues without edges", {
  input <- "WURCS=2.0/1,1,0/[a2122h-1a_1-5]/1/"
  expect_identical(as.character(parse_wurcs(input)), "Glc(a1-")
})
