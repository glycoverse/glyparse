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
    acetate = "DNeup5Ac[9A]a2-6DGalpb1-4DGlcpNAcb1-OH"
  )

  parsed <- parse_glycam_iupac(to_parse)

  expect_identical(
    as.character(parsed),
    c(
      sulfate = "Neu5Ac(a2-3)Gal6S(b1-4)[Fuc(a1-3)]GlcNAc6S(b1-",
      acetate = "Neu5Ac9Ac(a2-6)Gal(b1-4)GlcNAc(b1-"
    )
  )
})

test_that("GlyCAM IUPAC converts representative corpus patterns", {
  skip_on_old_win()

  to_parse <- c(
    sialylated_lacnac = "DNeup5Aca2-3DGalpb1-4DGlcpNAcb1-OH",
    core_fucose = "DManpa1-6DManpb1-4DGlcpNAcb1-4[LFucpa1-6]DGlcpNAcb1-OH",
    uronic_acid = "DGlcpAb1-3DGalpNAcb1-OH",
    xyl_pyranose = "DXylpa1-6DGlcpa1-OH",
    polysialic_branch = "DNeup5Aca2-8DNeup5Aca2-3[DGalpNAcb1-4]DGalpb1-4DGlcpb1-OH",
    xyl_chain = "DGlcpb1-3DXylpb1-3DXylpb1-OH",
    glcnac_7_neu5ac = "DGalpa1-2DGlcpb1-3DGlcpNAcb1-7DNeup5Acb2-OH",
    gal_neu5ac_8 = "DGalpb1-8DNeup5Aca2-OH",
    gal_neu5ac_9 = "DGalpb1-9DNeup5Aca2-OH"
  )

  parsed <- parse_glycam_iupac(to_parse)

  expect_identical(
    as.character(parsed),
    c(
      sialylated_lacnac = "Neu5Ac(a2-3)Gal(b1-4)GlcNAc(b1-",
      core_fucose = "Man(a1-6)Man(b1-4)GlcNAc(b1-4)[Fuc(a1-6)]GlcNAc(b1-",
      uronic_acid = "GlcA(b1-3)GalNAc(b1-",
      xyl_pyranose = "Xyl(a1-6)Glc(a1-",
      polysialic_branch = "Neu5Ac(a2-8)Neu5Ac(a2-3)[GalNAc(b1-4)]Gal(b1-4)Glc(b1-",
      xyl_chain = "Glc(b1-3)Xyl(b1-3)Xyl(b1-",
      glcnac_7_neu5ac = "Gal(a1-2)Glc(b1-3)GlcNAc(b1-7)Neu5Ac(b2-",
      gal_neu5ac_8 = "Gal(b1-8)Neu5Ac(a2-",
      gal_neu5ac_9 = "Gal(b1-9)Neu5Ac(a2-"
    )
  )
})

test_that("GlyCAM IUPAC preserves NA and on_failure semantics", {
  skip_on_old_win()

  to_parse <- c(valid = "DGlcpb1-OH", missing = NA_character_)

  parsed <- parse_glycam_iupac(to_parse)

  expect_named(parsed, names(to_parse))
  expect_identical(as.character(parsed), c(valid = "Glc(b1-", missing = NA))
})
