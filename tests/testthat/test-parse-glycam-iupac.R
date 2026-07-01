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

test_that("GlyCAM IUPAC converts representative corpus patterns", {
  skip_on_old_win()

  to_parse <- c(
    sialylated_lacnac = "DNeup5Aca2-3DGalpb1-4DGlcpNAcb1-OH",
    core_fucose = "DManpa1-6DManpb1-4DGlcpNAcb1-4[LFucpa1-6]DGlcpNAcb1-OH",
    uronic_acid = "DGlcpAb1-3DGalpNAcb1-OH",
    kdn = "DKDNpa2-3DGalpb1-OH",
    ido_sulfate = "LIdopA[2S]a1-3DGalpNAcb1-OH",
    xyl_furanose = "DXylfa1-3DGlcpb1-OH",
    xyl_pyranose = "DXylpa1-6DGlcpa1-OH"
  )

  parsed <- parse_glycam_iupac(to_parse)

  expect_identical(
    as.character(parsed),
    c(
      sialylated_lacnac = "Neu5Ac(a2-3)Gal(b1-4)GlcNAc(b1-",
      core_fucose = "Man(a1-6)Man(b1-4)GlcNAc(b1-4)[Fuc(a1-6)]GlcNAc(b1-",
      uronic_acid = "GlcA(b1-3)GalNAc(b1-",
      kdn = "Kdn(a2-3)Gal(b1-",
      ido_sulfate = "IdoA2S(a1-3)GalNAc(b1-",
      xyl_furanose = "Xyl(a1-3)Glc(b1-",
      xyl_pyranose = "Xyl(a1-6)Glc(a1-"
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
