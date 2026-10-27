parser_progress_examples <- list(
  auto_parse = "Gal(b1-3)GlcNAc(b1-4)Glc(a1-",
  parse_glycam_iupac = "DGlcpNAcb1-OH",
  parse_glycoct = "RES\n1b:x-HEX-x:x",
  parse_iupac_compact = "Galb1-3GlcNAc",
  parse_iupac_condensed = "Gal(b1-3)GlcNAc(b1-4)Glc(a1-",
  parse_iupac_extended = "beta-D-Galp-(1->3)-alpha-D-GalpNAc-(1->",
  parse_iupac_short = "Neu5Aca3Gala3(Fuca6)GlcNAcb-",
  parse_linucs = "[][Hexp]{}",
  parse_linear_code = "Ma3(Ma6)Mb4GNb4GNb",
  parse_pglyco_struc = "(N)",
  parse_strucgp_struc = "A2B2C1D1E1F1fedD1E1F1feE1F1fedcba",
  parse_wurcs = "WURCS=2.0/1,0,0/[a2122h-1x_1-5]/1/"
)

purrr::iwalk(parser_progress_examples, function(input, parser_name) {
  test_that(paste(parser_name, "accepts progress"), {
    parser <- get(parser_name, envir = asNamespace("glyparse"))

    expect_true("progress" %in% names(formals(parser)))
    expect_no_error(suppressMessages(parser(input, progress = TRUE)))
  })
})

test_that("IUPAC parsers forward progress to structure construction", {
  constructor <- glyrepr::as_glycan_structure
  progress_values <- logical()
  local_mocked_bindings(
    as_glycan_structure = function(
      x,
      on_failure,
      progress = FALSE,
      warn = TRUE
    ) {
      progress_values <<- c(progress_values, progress)
      constructor(x, on_failure = on_failure, progress = progress, warn = warn)
    },
    .package = "glyrepr"
  )
  parser_names <- c(
    "auto_parse",
    "parse_glycam_iupac",
    "parse_iupac_compact",
    "parse_iupac_condensed",
    "parse_iupac_extended",
    "parse_iupac_short"
  )

  for (parser_name in parser_names) {
    parser <- get(parser_name, envir = asNamespace("glyparse"))
    input <- c(
      first = parser_progress_examples[[parser_name]],
      missing = NA_character_,
      duplicate = parser_progress_examples[[parser_name]]
    )
    progress_values <- logical()
    expected <- parser(input)
    disabled <- parser(input, progress = FALSE)
    enabled <- suppressMessages(parser(input, progress = TRUE))

    expect_identical(progress_values, c(FALSE, FALSE, TRUE), info = parser_name)
    expect_identical(
      as.character(disabled),
      as.character(expected),
      info = parser_name
    )
    expect_identical(
      as.character(enabled),
      as.character(expected),
      info = parser_name
    )
    expect_identical(names(enabled), names(input), info = parser_name)
    expect_identical(unname(is.na(enabled)), c(FALSE, TRUE, FALSE))
  }
})

test_that("progress updates retain parallel decoding and record order", {
  previous <- options(glyparse.threads = 4L)
  on.exit(options(previous), add = TRUE)
  input <- rep(
    c(
      "RES 1b:b-dglc-HEX-1:5",
      "invalid",
      "RES 1b:b-dgal-HEX-1:5",
      NA_character_
    ),
    length.out = 2305L
  )
  expected <- native_records(input, "glycoct")
  decode <- native_records_cpp
  sizes <- workers <- integer()
  local_mocked_bindings(
    native_records_cpp = function(
      x,
      format,
      vocabulary,
      threads,
      constructible_only
    ) {
      sizes <<- c(sizes, length(x))
      workers <<- c(workers, threads)
      decode(x, format, vocabulary, threads, constructible_only)
    }
  )
  actual <- suppressMessages(native_records(input, "glycoct", progress = TRUE))
  expect_identical(actual, lapply(expected, identity))
  expect_gt(length(sizes), 1L)
  expect_gte(min(sizes), 256L)
  expect_identical(workers, rep(4L, length(sizes)))
})
