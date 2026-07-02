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

test_that("GlyCAM IUPAC parses checklist monosaccharide names", {
  skip_on_old_win()

  to_parse <- c(
    Hex = "Hexpb1-OH",
    Glc = "DGlcpb1-OH",
    Man = "DManpb1-OH",
    Gal = "DGalpb1-OH",
    Gul = "DGulpb1-OH",
    Alt = "LAltpb1-OH",
    All = "DAllpb1-OH",
    Tal = "DTalpb1-OH",
    Ido = "LIdopb1-OH",
    GlcNAc = "DGlcpNAcb1-OH",
    GalNAc = "DGalpNAcb1-OH",
    ManNAc = "DManpNAcb1-OH",
    GulNAc = "DGulpNAcb1-OH",
    AltNAc = "LAltpNAcb1-OH",
    AllNAc = "DAllpNAcb1-OH",
    TalNAc = "DTalpNAcb1-OH",
    IdoNAc = "LIdopNAcb1-OH",
    GlcN = "DGlcpNb1-OH",
    ManN = "DManpNb1-OH",
    GalN = "DGalpNb1-OH",
    GulN = "DGulpNb1-OH",
    AltN = "LAltpNb1-OH",
    AllN = "DAllpNb1-OH",
    TalN = "DTalpNb1-OH",
    IdoN = "LIdopNb1-OH",
    GlcA = "DGlcpAb1-OH",
    ManA = "DManpAb1-OH",
    GalA = "DGalpAb1-OH",
    GulA = "DGulpAb1-OH",
    AltA = "LAltpAb1-OH",
    AllA = "DAllpAb1-OH",
    TalA = "DTalpAb1-OH",
    IdoA = "LIdopAb1-OH",
    Fuc = "LFucpb1-OH",
    Qui = "DQuipb1-OH",
    Rha = "LRhapb1-OH",
    QuiNAc = "DQuipNAcb1-OH",
    RhaNAc = "LRhapNAcb1-OH",
    FucNAc = "LFucpNAcb1-OH",
    Oli = "DOlipb1-OH",
    Tyv = "DTyvpb1-OH",
    Abe = "DAbepb1-OH",
    Par = "DParpb1-OH",
    Dig = "DDigpb1-OH",
    Col = "LColpb1-OH",
    Ara = "LArapb1-OH",
    Lyx = "LLyxpb1-OH",
    Xyl = "DXylpb1-OH",
    Rib = "DRibpb1-OH",
    Neu5Ac = "DNeup5Acb2-OH",
    Neu5Gc = "DNeup5Gcb2-OH",
    Neu = "Neupb2-OH",
    Kdn = "DKDNpb2-OH",
    Pse = "Psepb2-OH",
    Leg = "Legpb2-OH",
    Aci = "Acipb2-OH",
    Bac = "DBacpb1-OH",
    LDmanHep = "LDmanHeppb1-OH",
    Kdo = "DKdopb2-OH",
    Dha = "DDhapb2-OH",
    DDmanHep = "DDmanHeppb1-OH",
    MurNAc = "DMurpNAcb1-OH",
    MurNGc = "DMurpNGcb1-OH",
    Mur = "DMurpb1-OH",
    Api = "DApifb1-OH",
    Fru = "DFrupb2-OH",
    Tag = "DTagpb2-OH",
    Sor = "LSorpb2-OH",
    Psi = "DPsipb2-OH"
  )

  parsed <- parse_glycam_iupac(to_parse)

  expect_identical(
    as.character(parsed),
    c(
      Hex = "Hex(b1-",
      Glc = "Glc(b1-",
      Man = "Man(b1-",
      Gal = "Gal(b1-",
      Gul = "Gul(b1-",
      Alt = "Alt(b1-",
      All = "All(b1-",
      Tal = "Tal(b1-",
      Ido = "Ido(b1-",
      GlcNAc = "GlcNAc(b1-",
      GalNAc = "GalNAc(b1-",
      ManNAc = "ManNAc(b1-",
      GulNAc = "GulNAc(b1-",
      AltNAc = "AltNAc(b1-",
      AllNAc = "AllNAc(b1-",
      TalNAc = "TalNAc(b1-",
      IdoNAc = "IdoNAc(b1-",
      GlcN = "GlcN(b1-",
      ManN = "ManN(b1-",
      GalN = "GalN(b1-",
      GulN = "GulN(b1-",
      AltN = "AltN(b1-",
      AllN = "AllN(b1-",
      TalN = "TalN(b1-",
      IdoN = "IdoN(b1-",
      GlcA = "GlcA(b1-",
      ManA = "ManA(b1-",
      GalA = "GalA(b1-",
      GulA = "GulA(b1-",
      AltA = "AltA(b1-",
      AllA = "AllA(b1-",
      TalA = "TalA(b1-",
      IdoA = "IdoA(b1-",
      Fuc = "Fuc(b1-",
      Qui = "Qui(b1-",
      Rha = "Rha(b1-",
      QuiNAc = "QuiNAc(b1-",
      RhaNAc = "RhaNAc(b1-",
      FucNAc = "FucNAc(b1-",
      Oli = "Oli(b1-",
      Tyv = "Tyv(b1-",
      Abe = "Abe(b1-",
      Par = "Par(b1-",
      Dig = "Dig(b1-",
      Col = "Col(b1-",
      Ara = "Ara(b1-",
      Lyx = "Lyx(b1-",
      Xyl = "Xyl(b1-",
      Rib = "Rib(b1-",
      Neu5Ac = "Neu5Ac(b2-",
      Neu5Gc = "Neu5Gc(b2-",
      Neu = "Neu(b2-",
      Kdn = "Kdn(b2-",
      Pse = "Pse(b2-",
      Leg = "Leg(b2-",
      Aci = "Aci(b2-",
      Bac = "Bac(b1-",
      LDmanHep = "LDmanHep(b1-",
      Kdo = "Kdo(b2-",
      Dha = "Dha(b2-",
      DDmanHep = "DDmanHep(b1-",
      MurNAc = "MurNAc(b1-",
      MurNGc = "MurNGc(b1-",
      Mur = "Mur(b1-",
      Api = "Api(b1-",
      Fru = "Fru(b2-",
      Tag = "Tag(b2-",
      Sor = "Sor(b2-",
      Psi = "Psi(b2-"
    )
  )
})

test_that("GlyCAM IUPAC converts residue modifiers", {
  skip_on_old_win()

  to_parse <- c(
    sulfate = "DNeup5Aca2-3DGalp[6S]b1-4[LFucpa1-3]DGlcpNAc[6S]b1-OH",
    acetate = "DNeup5Ac[9A]a2-6DGalpb1-4DGlcpNAcb1-OH",
    methyl = "DGlcp[3Me]b1-OH",
    phosphate = "DGlcp[6P]b1-OH",
    amino = "DGlcp[2N]b1-OH",
    unknown_position = "DGalp[?S]b1-OH"
  )

  parsed <- parse_glycam_iupac(to_parse)

  expect_identical(
    as.character(parsed),
    c(
      sulfate = "Neu5Ac(a2-3)Gal6S(b1-4)[Fuc(a1-3)]GlcNAc6S(b1-",
      acetate = "Neu5Ac9Ac(a2-6)Gal(b1-4)GlcNAc(b1-",
      methyl = "Glc3Me(b1-",
      phosphate = "Glc6P(b1-",
      amino = "Glc2N(b1-",
      unknown_position = "Gal?S(b1-"
    )
  )
})

test_that("GlyCAM IUPAC converts anomers and unknown linkage positions", {
  skip_on_old_win()

  to_parse <- c(
    alpha = "DGalpa1-4DGlcpb1-OH",
    beta = "DGalpb1-4DGlcpb1-OH",
    unknown_anomer = "DGalp?1-4DGlcpb1-OH",
    unknown_child_position = "DGalpa?-4DGlcpb1-OH",
    unknown_parent_position = "DGalpa1-?DGlcpb1-OH",
    unknown_both_linkage_positions = "DGalp?1-?DGlcpb1-OH"
  )

  parsed <- parse_glycam_iupac(to_parse)

  expect_identical(
    as.character(parsed),
    c(
      alpha = "Gal(a1-4)Glc(b1-",
      beta = "Gal(b1-4)Glc(b1-",
      unknown_anomer = "Gal(?1-4)Glc(b1-",
      unknown_child_position = "Gal(a?-4)Glc(b1-",
      unknown_parent_position = "Gal(a1-?)Glc(b1-",
      unknown_both_linkage_positions = "Gal(?1-?)Glc(b1-"
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
