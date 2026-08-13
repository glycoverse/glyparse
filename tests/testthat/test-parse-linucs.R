linucs_parse_cases <- list(
  generic_hex = list(
    input = "[][Hexp]{}",
    expected = "Hex(?1-"
  ),
  generic_substituents = list(
    input = "[][HexpNS]{}",
    expected = "HexN2S(?1-"
  ),
  concrete_monosaccharide = list(
    input = "[][D-Glcp6P]{}",
    expected = "Glc6P(?1-"
  ),
  anomer_and_linkage = list(
    input = "[][b-D-Glcp]{[(4+1)][a-D-Glcp]{}}",
    expected = "Glc(a1-4)Glc(b1-"
  ),
  alditol_root = list(
    input = "[][D-Glc-ol]{[(?+1)][b-D-Galp]{}}",
    expected = "Gal(b1-?)Glc(?1-"
  ),
  nested_modifiers = list(
    input = "[][HexpNAc6S]{[(4+1)][HexpA2S]{[(4+1)][HexpNS3S]{[(4+1)][HexpA2S]{}}}}",
    expected = "HexA2S(?1-4)HexN2S3S(?1-4)HexA2S(?1-4)HexNAc6S(?1-"
  ),
  implicit_substituent = list(
    input = "[][MurpNAc]{[(3+1)][1(R)-carboxyethoxy]{}}",
    expected = "MurNAc(?1-"
  ),
  onic_nonulosonate = list(
    input = "[][b-Legp-onic]{}",
    expected = "Leg(b2-"
  ),
  branching = list(
    input = "[][b-D-Glcp]{[(3+1)][b-D-Glcp]{[(3+1)][b-D-Glcp]{}[(6+1)][b-D-Glcp]{}}}",
    expected = "Glc(b1-3)[Glc(b1-6)]Glc(b1-3)Glc(b1-"
  )
)

purrr::iwalk(linucs_parse_cases, function(case, case_name) {
  test_that(paste("LINUCS parses", case_name), {
    skip_on_old_win()

    parsed <- parse_linucs(case$input)

    expect_s3_class(parsed, "glyrepr_structure")
    expect_identical(as.character(parsed), case$expected)
  })
})

test_that("LINUCS preserves NA and name semantics", {
  skip_on_old_win()

  to_parse <- c(valid = "[][Hexp]{}", missing = NA_character_)

  parsed <- parse_linucs(to_parse)

  expect_named(parsed, names(to_parse))
  expect_identical(
    as.character(parsed),
    c(valid = "Hex(?1-", missing = NA_character_)
  )
})

test_that("LINUCS preserves unusual configurations", {
  parsed <- parse_linucs(c(
    "[][a-L-Fucp]{}",
    "[][a-D-Fucp3S]{}",
    "[][b-L-Gulp]{}",
    "[][?-D-Fucf]{}"
  ))

  expect_identical(
    as.character(parsed),
    c("Fuc(a1-", "D-Fuc3S(a1-", "L-Gul(b1-", "D-Fucf(?1-")
  )
})

test_that("LINUCS parses N-sulfate suffixes on unusual configurations", {
  parsed <- parse_linucs(c(
    "[][L-GulpNS]{}",
    "[][L-GulfNS]{}",
    "[][D-IdopNS]{}",
    "[][D-IdofNS]{}"
  ))

  expect_identical(
    as.character(parsed),
    c(
      "L-GulN2S(?1-",
      "L-GulfN2S(?1-",
      "D-IdoN2S(?1-",
      "D-IdofN2S(?1-"
    )
  )
})

test_that("LINUCS supports on_failure handling", {
  skip_on_old_win()

  parsed <- parse_linucs(
    c(valid = "[][Hexp]{}", invalid = "not a LINUCS structure"),
    on_failure = "na"
  )

  expect_identical(
    as.character(parsed),
    c(valid = "Hex(?1-", invalid = NA_character_)
  )
})

test_that("LINUCS parses every mapped furanose monosaccharide", {
  mono_map <- linucs_mono_stem_map()
  furanose <- intersect(
    unname(furanose_monosaccharide_map()),
    unname(mono_map)
  )
  source_names <- names(mono_map)[match(furanose, mono_map)]
  input <- paste0("[][b-", source_names, "]{}")

  parsed <- parse_linucs(input)
  graphs <- glyrepr::get_structure_graphs(parsed, return_list = TRUE)

  expect_identical(purrr::map_chr(graphs, ~ igraph::V(.x)$mono), furanose)
})

test_that("LINUCS rejects malformed Api furanose aliases", {
  expect_error(parse_linucs("[][b-D-Afif]{}"), "Can't parse")
})
