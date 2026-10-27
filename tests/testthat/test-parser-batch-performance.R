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

test_that("residue caches do not retain graph positions or children", {
  linucs <- c(
    "[][b-D-Glcp]{[(4+1)][a-D-Glcp]{}}",
    "[][a-D-Glcp]{[(6+1)][b-D-Glcp]{}}",
    "[][b-D-Glcp6S]{}"
  )
  kcf <- c(
    "NODE 2\n1 Glc 0 0\n2 Gal 0 0\nEDGE 1\n1 2:b1 1:4",
    "NODE 3\n1 Gal 0 0\n2 Glc 0 0\n3 Glc 0 0\nEDGE 2\n1 3:a1 2:4\n2 2:b1 1:6"
  )
  gwb <- c(
    "freeEnd--1b1D-Glc,p--4b1D-Gal,p",
    "freeEnd--1b1D-Gal,p--4b1D-Glc,p",
    "freeEnd--1b1D-Glc,p(--6S)--4b1D-Gal,p"
  )
  for (format in c("linucs", "kcf", "gwb")) {
    input <- get(format)
    parser <- get(paste0("parse_", format))
    expected <- vapply(input, function(x) as.character(parser(x)), character(1))
    expect_identical(as.character(parser(input)), unname(expected))
    expect_identical(as.character(parser(rev(input))), rev(unname(expected)))
  }
})

test_that("plain Linear Code retains the general normalization contract", {
  input <- c(
    "Ma3(Ma6)Mb4GNb4GNb",
    "NNa3Ab4GNb",
    "G^b4Ab",
    "Ab3/6Gb",
    "Ab?G?",
    "A[2P]b3Gb",
    "Ab3(A[2T]b4)Gb",
    "invalid",
    "",
    NA_character_
  )
  expect_identical(
    convert_linear_to_iupac(input),
    convert_linear_to_iupac_general(input)
  )
})

test_that("CPU workers preserve parser results and per-record diagnostics", {
  fixtures <- list(
    wurcs = c("WURCS=2.0/1,1,0/[a2122h-1b_1-5]/1/", "invalid", NA_character_),
    glycoct = c(
      "RES\n1b:b-dglc-HEX-1:5",
      "RES\n1b:o-dglc-HEX-0:0",
      NA_character_
    ),
    kcf = c("NODE 2\n1 Glc 0 0\n2 Gal 0 0\nEDGE 1\n1 2:b1 1:4", "invalid"),
    linucs = c("[][b-D-Glcp]{[(4+1)][a-D-Glcp]{}}", "invalid")
  )
  for (format in names(fixtures)) {
    x <- rep(fixtures[[format]], length.out = 2051)
    serial <- native_records_cpp(x, format, native_vocabulary(), 1L)
    parallel <- native_records_cpp(x, format, native_vocabulary(), 2L)
    expect_identical(parallel, serial, info = format)
  }
  for (format in c("iupac_short", "iupac_extended", "glycam_iupac", "detect")) {
    x <- rep(
      c(
        "Galb4Glcb-",
        "alpha-D-Galp-(1->4)-beta-D-Glcp-(1->",
        "DGalpb1-4DGlcpb1-OH",
        NA_character_,
        "invalid"
      ),
      length.out = 2051
    )
    expect_identical(
      native_convert_cpp(x, format, native_vocabulary(), 2L),
      native_convert_cpp(x, format, native_vocabulary(), 1L),
      info = format
    )
  }
})

test_that("shared native record fields survive GC and copy on modification", {
  x <- c("RES\n1b:b-dglc-HEX-1:5", "RES\n1b:b-dgal-HEX-1:5")
  reference <- native_records_cpp(x, "glycoct", native_vocabulary())
  previous <- gctorture2(50L)
  on.exit(gctorture2(previous), add = TRUE)
  actual <- native_records_cpp(x, "glycoct", native_vocabulary())
  gctorture2(previous)
  expect_identical(actual, reference)
  actual[[1]]$sub[[1]] <- "6S"
  actual[[1]]$anomer[[1]] <- "a1"
  actual[[1]]$alditol[[1]] <- TRUE
  expect_identical(actual[[2]], reference[[2]])
})

test_that("impossible out-trees are filtered without skipping valid arrays", {
  x <- c(
    "RES\n1b:b-dglc-HEX-1:5\n2b:b-dgal-HEX-1:5",
    "RES\n1b:b-dglc-HEX-1:5\n2b:b-dgal-HEX-1:5\nLIN\n1:1o(4+1)2d"
  )
  raw <- native_records_cpp(x, "glycoct", native_vocabulary(), 1L)
  filtered <- native_records_cpp(x, "glycoct", native_vocabulary(), 1L, TRUE)
  expect_null(filtered[[1]])
  expect_identical(filtered[[2]], raw[[2]])
  expect_identical(attr(filtered, "warnings"), attr(raw, "warnings"))
  expected <- suppressWarnings(glyrepr::structure_from_arrays(
    raw,
    on_failure = "na"
  ))
  expect_identical(
    as.character(parse_glycoct(x, on_failure = "na")),
    as.character(expected)
  )
})

test_that("normalized parsing supports character constructors without warn", {
  constructor <- glyrepr::as_glycan_structure
  testthat::local_mocked_bindings(
    as_glycan_structure = function(x, on_failure, progress = FALSE) {
      constructor(x, on_failure = on_failure, progress = progress)
    },
    .package = "glyrepr"
  )
  expect_identical(
    as.character(parse_iupac_short(
      c("Galb4Glcb-", "invalid"),
      on_failure = "na"
    )),
    c("Gal(b1-4)Glc(b1-", NA_character_)
  )
})
