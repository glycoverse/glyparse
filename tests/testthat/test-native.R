test_that("native converters isolate malformed strings within a batch", {
  x <- c(
    first = "Galb-",
    invalid = "not a glycan",
    missing = NA,
    last = "Glca-"
  )
  expect_identical(
    native_convert(x, "iupac_short"),
    c(first = "Gal(b1-", invalid = NA, missing = NA, last = "Glc(a1-")
  )
})

test_that("native records isolate overflowing indices and malformed nesting", {
  x <- c(
    "WURCS=2.0/1,1,0/[a2122h-1a_1-5]/1/",
    "WURCS=2.0/1,1,0/[a2122h-1a_1-5]/9999999999999999999999999/",
    "WURCS=2.0/1,1,0/[a2122h-1a_1-5]/0/",
    "WURCS=2.0/1,2,1/[a2122h-1a_1-5]/1-1/a4-Z1",
    NA_character_
  )
  records <- native_records_cpp(x, "wurcs", native_vocabulary())
  expect_identical(
    vapply(records, is.null, logical(1)),
    c(FALSE, TRUE, TRUE, TRUE, TRUE)
  )
  expect_identical(
    is.na(parse_wurcs(x, on_failure = "na")),
    c(FALSE, TRUE, TRUE, TRUE, TRUE)
  )
  expect_identical(
    is.na(parse_linucs(
      c("[][a-D-Glcp]{", "[][a-D-Glcp]{}"),
      on_failure = "na"
    )),
    c(TRUE, FALSE)
  )
})

test_that("progress chunks preserve invalid positions and names", {
  x <- vapply(
    seq_len(140L),
    function(n) paste0(strrep("(H", n), strrep(")", n)),
    character(1)
  )
  x <- c(x, "(H", "bad input", NA_character_, x[1:3])
  names(x) <- paste0("input", seq_along(x))
  expected <- parse_pglyco_struc(x, on_failure = "na")
  actual <- parse_pglyco_struc(x, on_failure = "na", progress = TRUE)
  expect_identical(as.character(actual), as.character(expected))
  expect_identical(names(actual), names(x))
})

test_that("generic WURCS alditols retain their reducing anomer carbon", {
  x <- "WURCS=2.0/1,1,0/[hUdxxxxxh_5*NCC/3=O]/1/"
  expect_identical(as.character(parse_wurcs(x)), "NeuAc-ol(?2-")
})

test_that("KCF ignores whitespace-only lines inside sections", {
  x <- "ENTRY G00001 Glycan\nNODE 2\n1 Glc\n2 Gal\nEDGE 1\n  \n1 2:b1 1:4\n  \n///"
  expect_identical(as.character(parse_kcf(x)), "Gal(b1-4)Glc(?1-")
})

test_that("unidentified GlycoCT alditols remain parsing failures", {
  expect_identical(
    is.na(parse_glycoct("RES\n1b:o-HEX-0:0|1:aldi", on_failure = "na")),
    TRUE
  )
})

test_that("native ports preserve established tokenization and recovery", {
  expect_identical(
    as.character(parse_iupac_extended(
      c(
        "beta-D-Galp-(1->3)-al?ha-D-GalpNAc-(1->",
        "bet1a-D-Galp-(1->3)-alpha-D-GalpNAc-(1->"
      ),
      on_failure = "na"
    )),
    c(NA_character_, "GalNAc(a1-")
  )
  expect_identical(
    as.character(parse_pglyco_struc(c("(H(N)", "(H"), on_failure = "na")),
    c("HexNAc(??-?)Hex(??-", NA_character_)
  )
  expect_identical(
    as.character(parse_strucgp_struc("A1B1a")),
    "Hex(??-?)Hex(??-"
  )
  expect_identical(
    as.character(parse_linucs("[][b-D-Glcp]{[(4+1)][a-D-Glcp\n]{}}")),
    "Glc(a1-4)Glc(b1-"
  )
  expect_identical(
    as.character(parse_glycoct("RES\n1b:x-dido-HEX-1:51|6:a")),
    "D-IdoA(?1-"
  )
})
