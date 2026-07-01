test_that("GlyCAM IUPAC converts simple and branched glycans", {
  skip_on_old_win()

  to_parse <- c(
    glucose_trimer = "DGlcpb1-3DGlcpb1-3DGlcpb1-OH",
    branched_n_glycan = "DManpa1-3[DManpa1-6]DManpb1-4DGlcpNAcb1-OH",
    fucosylated_lac = "LFucpa1-2DGalpb1-4[LFucpa1-3]DGlcpNAcb1-OH"
  )

  parsed <- parse_glycam_iupac(to_parse)

  expect_s3_class(parsed, "glyrepr_structure")
  expect_named(parsed, names(to_parse))
  expect_identical(
    as.character(parsed),
    c(
      glucose_trimer = "Glc(b1-3)Glc(b1-3)Glc(b1-",
      branched_n_glycan = "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-",
      fucosylated_lac = "Fuc(a1-2)Gal(b1-4)[Fuc(a1-3)]GlcNAc(b1-"
    )
  )
})

test_that("GlyCAM IUPAC converts residue modifiers", {
  skip_on_old_win()

  to_parse <- c(
    sulfate = "DNeup5Aca2-3DGalp[6S]b1-4[LFucpa1-3]DGlcpNAc[6S]b1-OH",
    acetate = "DNeup5Ac[9A]a2-6DGalpb1-4DGlcpNAcb1-OH",
    methyl = "DGalp[2Me]b1-3[LFucpa1-4]DGlcpNAcb1-OH"
  )

  parsed <- parse_glycam_iupac(to_parse)

  expect_identical(
    as.character(parsed),
    c(
      sulfate = "Neu5Ac(a2-3)Gal6S(b1-4)[Fuc(a1-3)]GlcNAc6S(b1-",
      acetate = "Neu5Ac9Ac(a2-6)Gal(b1-4)GlcNAc(b1-",
      methyl = "Gal2Me(b1-3)[Fuc(a1-4)]GlcNAc(b1-"
    )
  )
})

test_that("GlyCAM IUPAC preserves NA and on_failure semantics", {
  skip_on_old_win()

  to_parse <- c(valid = "DGlcpb1-OH", missing = NA_character_)

  parsed <- parse_glycam_iupac(to_parse)

  expect_named(parsed, names(to_parse))
  expect_identical(as.character(parsed), c(valid = "Glc(b1-", missing = NA))
  expect_identical(
    as.character(parse_glycam_iupac("not-glycam", "na")),
    NA_character_
  )
  expect_error(parse_glycam_iupac("not-glycam"), "Can't parse")
})

test_that("documented GlyCAM IUPAC corpus is parsed or reported", {
  skip_on_old_win()

  glycam <- utils::read.csv(
    test_path("../../docs/glycan_sequences_glycam_iupac.csv"),
    stringsAsFactors = FALSE
  )
  iupac <- utils::read.csv(
    test_path("../../docs/glycan_sequences_iupac_condensed.csv"),
    stringsAsFactors = FALSE
  )
  failure_path <- test_path("../../docs/glycam_iupac_parse_failures.csv")
  expect_true(file.exists(failure_path))
  failures <- utils::read.csv(failure_path, stringsAsFactors = FALSE)

  report_cols <- c(
    "row",
    "glytoucan_ac",
    "sequence_glycam_iupac",
    "sequence_iupac_condensed",
    "reason",
    "message"
  )
  expect_true(all(report_cols %in% names(failures)))
  expect_false(any(is.na(failures$reason) | failures$reason == ""))

  iupac_lookup <- stats::setNames(
    iupac$sequence_iupac_condensed,
    iupac$glytoucan_ac
  )
  joined <- data.frame(
    glycam,
    sequence_iupac_condensed = iupac_lookup[glycam$glytoucan_ac],
    row.names = NULL
  )

  reported <- failures$glytoucan_ac
  to_check <- !joined$glytoucan_ac %in% reported
  rows_to_check <- which(to_check)
  chunks <- split(rows_to_check, ceiling(seq_along(rows_to_check) / 500))

  parsed_chunks <- purrr::map(chunks, function(rows) {
    parsed_glycam <- suppressMessages(parse_glycam_iupac(
      joined$sequence_glycam_iupac[rows],
      on_failure = "na"
    ))
    parsed_glycam_chr <- as.character(parsed_glycam)
    expect_false(anyNA(parsed_glycam_chr))

    rows_with_ref <- rows[!is.na(joined$sequence_iupac_condensed[rows])]
    parsed_ref <- suppressMessages(parse_iupac_condensed(
      joined$sequence_iupac_condensed[rows_with_ref],
      on_failure = "na"
    ))
    expect_identical(
      parsed_glycam_chr[match(rows_with_ref, rows)],
      as.character(parsed_ref)
    )

    data.frame(
      glytoucan_ac = joined$glytoucan_ac[rows],
      parsed = parsed_glycam_chr,
      stringsAsFactors = FALSE
    )
  })

  parsed_ids <- do.call(rbind, parsed_chunks)
  parsed_ids <- parsed_ids$glytoucan_ac[!is.na(parsed_ids$parsed)]

  expect_setequal(
    glycam$glytoucan_ac,
    c(parsed_ids, reported)
  )
})
