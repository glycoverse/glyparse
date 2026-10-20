test_that("GlycoCT descriptor reuse preserves substituents and ring bounds", {
  inputs <- c(
    "RES 1b:b-dglc-HEX-1:5",
    "RES 1b:b-dglc-HEX-1:5 2s:n-acetyl LIN 1:1d(2+1)2n",
    "RES 1b:b-dglc-HEX-1:4 2s:n-acetyl LIN 1:1d(2+1)2n",
    "RES 1b:b-dglc-HEX-1:5 2s:sulfate LIN 1:1d(6+1)2n"
  )
  expect_identical(
    as.character(parse_glycoct(rep(inputs, 2))),
    rep(c("Glc(b1-", "GlcNAc(b1-", "GlcfNAc(b1-", "Glc6S(b1-"), 2)
  )
  expect_identical(
    as.character(parse_glycoct(rev(inputs))),
    c("Glc6S(b1-", "GlcfNAc(b1-", "GlcNAc(b1-", "Glc(b1-")
  )
})

test_that("failed shared WURCS residues retain their positions and reasons", {
  inputs <- c(
    "WURCS=2.0/1,1,0/[not-a-residue]/1/",
    "WURCS=2.0/1,2,1/[not-a-residue]/1-1/a4-b1",
    "WURCS=2.0/1,1,0/[a2122h-1a_1-5]/1/"
  )
  records <- native_records_cpp(inputs, "wurcs", native_vocabulary())
  expect_identical(records[[1]], NULL)
  expect_identical(records[[2]], NULL)
  expect_identical(attr(records, "reasons")[[1]], attr(records, "reasons")[[2]])
  expect_identical(
    as.character(parse_wurcs(inputs, on_failure = "na")),
    c(NA_character_, NA_character_, "Glc(a1-")
  )
})

test_that("linear scanners retain whitespace and prefix parsing behavior", {
  ct <- "RES\t1b:b-dglc-HEX-1:5\v2b:b-dgal-HEX-1:5\fLIN 1:1o(4+1)2dtrailing"
  expect_identical(as.character(parse_glycoct(ct)), "Gal(b1-4)Glc(b1-")
  wurcs <- "WURCS=2.0/1,1,0/[a2122h-1a_1-5]/1"
  expect_identical(
    as.character(parse_wurcs(paste0(wurcs, c("", "/", "trailing", "-")))),
    rep("Glc(a1-", 4)
  )
})

test_that("batch alditol normalization reports one warning", {
  input <- "RES 1b:a-dgal-HEX-1:5 2b:o-dglc-HEX-0:0|1:aldi LIN 1:1o(3+1)2d"
  messages <- character()
  result <- withCallingHandlers(
    parse_glycoct(c(input, sub("3+1", "4+1", input, fixed = TRUE))),
    warning = function(cnd) {
      messages <<- c(messages, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(messages, 1L)
  expect_match(messages, "Only the main reducing-end GlycoCT residue")
  expect_identical(
    as.character(result),
    c("Glc(?1-3)Gal(a1-", "Glc(?1-4)Gal(a1-")
  )
})

test_that("the array wrapper supports older glyrepr constructors", {
  constructor <- glyrepr::structure_from_arrays
  testthat::local_mocked_bindings(
    structure_from_arrays = function(x, on_failure) {
      constructor(x, on_failure = on_failure)
    },
    .package = "glyrepr"
  )
  expect_identical(
    as.character(parse_glycoct(
      c("RES 1b:b-dglc-HEX-1:5", "invalid"),
      on_failure = "na"
    )),
    c("Glc(b1-", NA_character_)
  )
})
