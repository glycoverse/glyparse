test_that("G00051UX mixed residue case is consistent across formats", {
  inputs <- list(
    glycoct = paste(
      "RES",
      "1b:x-HEX-x:x",
      "2s:n-acetyl",
      "3b:x-dgal-HEX-1:5",
      "LIN",
      "1:1d(2+1)2n",
      "2:1o(-1+1)3d"
    ),
    wurcs = paste0(
      "WURCS=2.0/2,2,1/",
      "[uxxxxh_2*NCC/3=O][a2112h-1x_1-5]/1-2/a?-b1"
    ),
    iupac_condensed = "Gal(?1-?)HexNAc",
    iupac_extended = "?-D-Galp-(1->?)-?-HexNAc",
    iupac_compact = "Gal?1-?HexNAc",
    gwb = "freeEnd--?HexNAc--??1D-Gal,p$MONO,Und,0,0,freeEnd"
  )
  parsers <- list(
    glycoct = parse_glycoct,
    wurcs = parse_wurcs,
    iupac_condensed = parse_iupac_condensed,
    iupac_extended = parse_iupac_extended,
    iupac_compact = parse_iupac_compact,
    gwb = parse_gwb
  )

  parsed <- purrr::map2(inputs, parsers, ~ .y(.x))
  result <- do.call(c, parsed)
  expected <- stats::setNames(
    rep("Gal(?1-?)HexNAc(?1-", length(inputs)),
    names(inputs)
  )

  expect_identical(as.character(result), expected)
  expect_identical(
    glyrepr::get_mono_type(result),
    stats::setNames(rep("mixed", length(inputs)), names(inputs))
  )

  graphs <- unname(glyrepr::get_structure_graphs(result, return_list = TRUE))
  expect_equal(graphs, rep(graphs[1], length(graphs)))
})

test_that("auto_parse preserves G00051UX across formats", {
  input <- c(
    glycoct = paste(
      "RES",
      "1b:x-HEX-x:x",
      "2s:n-acetyl",
      "3b:x-dgal-HEX-1:5",
      "LIN",
      "1:1d(2+1)2n",
      "2:1o(-1+1)3d"
    ),
    wurcs = paste0(
      "WURCS=2.0/2,2,1/",
      "[uxxxxh_2*NCC/3=O][a2112h-1x_1-5]/1-2/a?-b1"
    ),
    iupac_condensed = "Gal(?1-?)HexNAc",
    iupac_extended = "?-D-Galp-(1->?)-?-HexNAc",
    iupac_compact = "Gal?1-?HexNAc",
    gwb = "freeEnd--?HexNAc--??1D-Gal,p$MONO,Und,0,0,freeEnd"
  )

  parsed <- auto_parse(input)

  expect_identical(
    as.character(parsed),
    stats::setNames(rep("Gal(?1-?)HexNAc(?1-", length(input)), names(input))
  )
  expect_identical(
    glyrepr::get_mono_type(parsed),
    stats::setNames(rep("mixed", length(input)), names(input))
  )
})
