test_that("parse_residue works for each monosaccharide without substituents", {
  expect_equal(
    parse_residue("a2122h-1a_1-5"),
    c(mono = "Glc", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a1122h-1a_1-5"),
    c(mono = "Man", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2112h-1a_1-5"),
    c(mono = "Gal", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2212h-1a_1-5"),
    c(mono = "Gul", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2111h-1a_1-5"),
    c(mono = "Alt", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2222h-1a_1-5"),
    c(mono = "All", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a1112h-1a_1-5"),
    c(mono = "Tal", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2121h-1a_1-5"),
    c(mono = "Ido", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2122h-1a_1-5_2*NCC/3=O"),
    c(mono = "GlcNAc", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2112h-1a_1-5_2*NCC/3=O"),
    c(mono = "GalNAc", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a1122h-1a_1-5_2*NCC/3=O"),
    c(mono = "ManNAc", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2212h-1a_1-5_2*NCC/3=O"),
    c(mono = "GulNAc", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2111h-1a_1-5_2*NCC/3=O"),
    c(mono = "AltNAc", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2222h-1a_1-5_2*NCC/3=O"),
    c(mono = "AllNAc", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a1112h-1a_1-5_2*NCC/3=O"),
    c(mono = "TalNAc", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2121h-1a_1-5_2*NCC/3=O"),
    c(mono = "IdoNAc", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2122h-1a_1-5_2*N"),
    c(mono = "GlcN", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a1122h-1a_1-5_2*N"),
    c(mono = "ManN", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2112h-1a_1-5_2*N"),
    c(mono = "GalN", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2212h-1a_1-5_2*N"),
    c(mono = "GulN", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2111h-1a_1-5_2*N"),
    c(mono = "AltN", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2222h-1a_1-5_2*N"),
    c(mono = "AllN", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a1112h-1a_1-5_2*N"),
    c(mono = "TalN", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2121h-1a_1-5_2*N"),
    c(mono = "IdoN", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2122A-1a_1-5"),
    c(mono = "GlcA", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a1122A-1a_1-5"),
    c(mono = "ManA", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2112A-1a_1-5"),
    c(mono = "GalA", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2212A-1a_1-5"),
    c(mono = "GulA", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2111A-1a_1-5"),
    c(mono = "AltA", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2222A-1a_1-5"),
    c(mono = "AllA", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a1112A-1a_1-5"),
    c(mono = "TalA", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2121A-1a_1-5"),
    c(mono = "IdoA", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a1221m-1a_1-5"),
    c(mono = "Fuc", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2122m-1a_1-5"),
    c(mono = "Qui", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2211m-1a_1-5"),
    c(mono = "Rha", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2212m-1a_1-5"),
    c(mono = "6dGul", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2111m-1a_1-5"),
    c(mono = "6dAlt", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a1112m-1a_1-5"),
    c(mono = "6dTal", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2122m-1a_1-5_2*NCC/3=O"),
    c(mono = "QuiNAc", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2211m-1a_1-5_2*NCC/3=O"),
    c(mono = "RhaNAc", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2111m-1a_1-5_2*NCC/3=O"),
    c(mono = "6dAltNAc", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a1112m-1a_1-5_2*NCC/3=O"),
    c(mono = "6dTalNAc", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a1221m-1a_1-5_2*NCC/3=O"),
    c(mono = "FucNAc", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("ad122m-1a_1-5"),
    c(mono = "Oli", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a1d22m-1a_1-5"),
    c(mono = "Tyv", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2d12m-1a_1-5"),
    c(mono = "Abe", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2d22m-1a_1-5"),
    c(mono = "Par", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("ad222m-1a_1-5"),
    c(mono = "Dig", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a1d21m-1a_1-5"),
    c(mono = "Col", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a211h-1a_1-5"),
    c(mono = "Ara", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a221h-1a_1-5"),
    c(mono = "Lyx", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a212h-1a_1-5"),
    c(mono = "Xyl", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a222h-1a_1-5"),
    c(mono = "Rib", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("Aad21122h-2a_2-6_5*NCC/3=O"),
    c(mono = "Neu5Ac", anomer = "a2", sub = "")
  )
  expect_equal(
    parse_residue("Aad21122h-2a_2-6_5*NCCO/3=O"),
    c(mono = "Neu5Gc", anomer = "a2", sub = "")
  )
  expect_equal(
    parse_residue("Aad21122h-2a_2-6_5*N"),
    c(mono = "Neu", anomer = "a2", sub = "")
  )
  expect_equal(
    parse_residue("Aad21122h-2a_2-6"),
    c(mono = "Kdn", anomer = "a2", sub = "")
  )
  expect_equal(
    parse_residue("had22111m-2a_2-6_5*N_7*N"),
    c(mono = "Pse", anomer = "a2", sub = "")
  )
  expect_equal(
    parse_residue("Aad21122m-2a_2-6_5*N_7*N"),
    c(mono = "Leg", anomer = "a2", sub = "")
  )
  expect_equal(
    parse_residue("Aad21111m-2a_2-6_5*N_7*N"),
    c(mono = "Aci", anomer = "a2", sub = "")
  )
  expect_equal(
    parse_residue("Aad11122m-2a_2-6_5*N_7*N"),
    c(mono = "4eLeg", anomer = "a2", sub = "")
  )
  expect_equal(
    parse_residue("a2122m-1a_1-5_2*N_4*N"),
    c(mono = "Bac", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a11221h-1a_1-5"),
    c(mono = "LDmanHep", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("Aad1122h-2a_2-6"),
    c(mono = "Kdo", anomer = "a2", sub = "")
  )
  expect_equal(
    parse_residue("Aad112A-2a_2-6"),
    c(mono = "Dha", anomer = "a2", sub = "")
  )
  expect_equal(
    parse_residue("a11222h-1a_1-5"),
    c(mono = "DDmanHep", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2122h-1a_1-5_2*NCC/3=O_3*OC^RCO/4=O/3C"),
    c(mono = "MurNAc", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2122h-1a_1-5_2*NCCO/3=O_3*OC^RCO/4=O/3C"),
    c(mono = "MurNGc", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2122h-1a_1-5_3*OC^RCO/4=O/3C"),
    c(mono = "Mur", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("ha122h-2a_2-6"),
    c(mono = "Fru", anomer = "a2", sub = "")
  )
  expect_equal(
    parse_residue("ha112h-2a_2-6"),
    c(mono = "Tag", anomer = "a2", sub = "")
  )
  expect_equal(
    parse_residue("ha121h-2a_2-6"),
    c(mono = "Sor", anomer = "a2", sub = "")
  )
  expect_equal(
    parse_residue("ha222h-2a_2-6"),
    c(mono = "Psi", anomer = "a2", sub = "")
  )
})


test_that("parse_residue raises an error with unkown monosaccharide", {
  expect_error(
    parse_residue("axxxxxh-1a_1-5"),
    'Unable to parse residue: "axxxxxh-1a_1-5"'
  )
})


test_that("parse_residue recognizes substituents on Glc", {
  expect_equal(
    parse_residue("a2122h-1a_1-5_3*OC"),
    c(mono = "Glc", anomer = "a1", sub = "3Me")
  )
  expect_equal(
    parse_residue("a2122h-1a_1-5_3*OCC/3=O"),
    c(mono = "Glc", anomer = "a1", sub = "3Ac")
  )
  expect_equal(
    parse_residue("a2122h-1a_1-5_3*NCC/3=O"),
    c(mono = "Glc", anomer = "a1", sub = "3NAc")
  )
  expect_equal(
    parse_residue("a2122h-1a_1-5_3*OPO/3O/3=O"),
    c(mono = "Glc", anomer = "a1", sub = "3P")
  )
  expect_equal(
    parse_residue("a2122h-1a_1-5_3*OSO/3=O/3=O"),
    c(mono = "Glc", anomer = "a1", sub = "3S")
  )
  expect_equal(
    parse_residue("a2122h-1a_1-5_3*OCCC/4=O/3=O"),
    c(mono = "Glc", anomer = "a1", sub = "3Pyr")
  )
  expect_equal(
    parse_residue("a2122h-1a_1-5_3*OPOCCNC/7C/7C/3O/3=O"),
    c(mono = "Glc", anomer = "a1", sub = "3PC")
  )
  expect_equal(
    parse_residue("a2122h-1a_1-5_3*OPOPOCCN/5O/5=O/3O/3=O"),
    c(mono = "Glc", anomer = "a1", sub = "3PPEtn")
  )
  expect_equal(
    parse_residue("a2122h-1a_1-5_3*OPOCCN/3O/3=O"),
    c(mono = "Glc", anomer = "a1", sub = "3PEtn")
  )
  expect_equal(
    parse_residue("a2122h-1a_1-5_3*N"),
    c(mono = "Glc", anomer = "a1", sub = "3N")
  )
})


test_that("parse_residue recognizes multiple substituents", {
  expect_equal(
    parse_residue("a2122h-1a_1-5_3*OC_6*OSO/3=O/3=O"),
    c(mono = "Glc", anomer = "a1", sub = "3Me,6S")
  )
  expect_equal(
    parse_residue("Aad21122h-2a_2-6_4*OCC/3=O_5*NCC/3=O_9*OCC/3=O"),
    c(mono = "Neu5Ac", anomer = "a2", sub = "4Ac,9Ac")
  )
})


test_that("parse_residue recognizes substituents on complex monosaccharides", {
  expect_equal(
    parse_residue("a2122h-1a_1-5_2*NCC/3=O_3*N"),
    c(mono = "GlcNAc", anomer = "a1", sub = "3N")
  )
  expect_equal(
    parse_residue("a2122h-1a_1-5_2*NCC/3=O_3*NCC/3=O"),
    c(mono = "GlcNAc", anomer = "a1", sub = "3NAc")
  )
  expect_equal(
    parse_residue("a1221m-1a_1-5_4*OPO/3O/3=O"),
    c(mono = "Fuc", anomer = "a1", sub = "4P")
  )
})


test_that("parse_residue recognized substituents with known positions", {
  expect_equal(
    parse_residue("a2122h-1a_1-5_2*NCC/3=O_?*OC"),
    c(mono = "GlcNAc", anomer = "a1", sub = "?Me")
  )
})


test_that("parse_residue raises an error with unkown substituent", {
  expect_error(
    parse_residue("a2122h-1a_1-5_3*OXXXX"),
    'Unable to parse substituent: "_3\\*OXXXX"'
  )
})


test_that("parse_residue correctly deals with anomers", {
  expect_equal(
    parse_residue("a2122h-1a_1-5"),
    c(mono = "Glc", anomer = "a1", sub = "")
  )
  expect_equal(
    parse_residue("a2122h-1b_1-5"),
    c(mono = "Glc", anomer = "b1", sub = "")
  )
  expect_equal(
    parse_residue("a2122h-1x_1-5"),
    c(mono = "Glc", anomer = "?1", sub = "")
  )
})


test_that("parse_residue knows unkown anomers", {
  expect_equal(
    parse_residue("a2122h-1x_1-5"),
    c(mono = "Glc", anomer = "?1", sub = "")
  )
})


test_that("parse_residue knows unkown substituent linkages", {
  expect_equal(
    parse_residue("a2122h-1a_1-5_?*OC"),
    c(mono = "Glc", anomer = "a1", sub = "?Me")
  )
})


test_that("parse_residue handles ambiguous u residues", {
  expect_ambiguous_residue <- function(residue, mono, sub = "") {
    expect_equal(
      parse_residue(residue),
      c(mono = mono, anomer = "??", sub = sub)
    )
  }

  purrr::iwalk(
    c(
      Glc = "u2122h",
      Man = "u1122h",
      Gal = "u2112h",
      Gul = "u2212h",
      Alt = "u2111h",
      All = "u2222h",
      Tal = "u1112h",
      Ido = "u2121h",
      GlcA = "u2122A",
      ManA = "u1122A",
      GalA = "u2112A",
      GulA = "u2212A",
      AltA = "u2111A",
      AllA = "u2222A",
      TalA = "u1112A",
      IdoA = "u2121A",
      Fuc = "u1221m",
      Qui = "u2122m",
      Rha = "u2211m",
      `6dGul` = "u2212m",
      `6dAlt` = "u2111m",
      `6dTal` = "u1112m",
      Oli = "ud122m",
      Tyv = "u1d22m",
      Abe = "u2d12m",
      Par = "u2d22m",
      Dig = "ud222m",
      Col = "u1d21m",
      Ara = "u211h",
      Lyx = "u221h",
      Xyl = "u212h",
      Rib = "u222h"
    ),
    ~ expect_ambiguous_residue(.x, .y)
  )

  purrr::iwalk(
    c(
      GlcNAc = "u2122h_2*NCC/3=O",
      ManNAc = "u1122h_2*NCC/3=O",
      GalNAc = "u2112h_2*NCC/3=O",
      GulNAc = "u2212h_2*NCC/3=O",
      AltNAc = "u2111h_2*NCC/3=O",
      AllNAc = "u2222h_2*NCC/3=O",
      TalNAc = "u1112h_2*NCC/3=O",
      IdoNAc = "u2121h_2*NCC/3=O",
      FucNAc = "u1221m_2*NCC/3=O",
      QuiNAc = "u2122m_2*NCC/3=O",
      RhaNAc = "u2211m_2*NCC/3=O",
      `6dAltNAc` = "u2111m_2*NCC/3=O",
      `6dTalNAc` = "u1112m_2*NCC/3=O"
    ),
    ~ expect_ambiguous_residue(.x, .y)
  )

  purrr::iwalk(
    c(
      GlcN = "u2122h_2*N",
      ManN = "u1122h_2*N",
      GalN = "u2112h_2*N",
      GulN = "u2212h_2*N",
      AltN = "u2111h_2*N",
      AllN = "u2222h_2*N",
      TalN = "u1112h_2*N",
      IdoN = "u2121h_2*N"
    ),
    ~ expect_ambiguous_residue(.x, .y)
  )

  purrr::pwalk(
    list(
      c(
        "u2122A_2*NCC/3=O",
        "u1122A_2*NCC/3=O",
        "u2112A_2*NCC/3=O",
        "u2212A_2*NCC/3=O",
        "u2111A_2*NCC/3=O",
        "u2222A_2*NCC/3=O",
        "u1112A_2*NCC/3=O",
        "u2121A_2*NCC/3=O",
        "u2212m_2*NCC/3=O",
        "ud122m_2*NCC/3=O",
        "u1d22m_2*NCC/3=O",
        "u2d12m_2*NCC/3=O",
        "u2d22m_2*NCC/3=O",
        "ud222m_2*NCC/3=O",
        "u1d21m_2*NCC/3=O",
        "u211h_2*NCC/3=O",
        "u221h_2*NCC/3=O",
        "u212h_2*NCC/3=O",
        "u222h_2*NCC/3=O"
      ),
      c(
        "GlcA",
        "ManA",
        "GalA",
        "GulA",
        "AltA",
        "AllA",
        "TalA",
        "IdoA",
        "6dGul",
        "Oli",
        "Tyv",
        "Abe",
        "Par",
        "Dig",
        "Col",
        "Ara",
        "Lyx",
        "Xyl",
        "Rib"
      ),
      "2NAc"
    ),
    expect_ambiguous_residue
  )

  purrr::pwalk(
    list(
      c(
        "u2122A_2*N",
        "u1122A_2*N",
        "u2112A_2*N",
        "u2212A_2*N",
        "u2111A_2*N",
        "u2222A_2*N",
        "u1112A_2*N",
        "u2121A_2*N",
        "u1221m_2*N",
        "u2122m_2*N",
        "u2211m_2*N",
        "u2212m_2*N",
        "u2111m_2*N",
        "u1112m_2*N",
        "ud122m_2*N",
        "u1d22m_2*N",
        "u2d12m_2*N",
        "u2d22m_2*N",
        "ud222m_2*N",
        "u1d21m_2*N",
        "u211h_2*N",
        "u221h_2*N",
        "u212h_2*N",
        "u222h_2*N"
      ),
      c(
        "GlcA",
        "ManA",
        "GalA",
        "GulA",
        "AltA",
        "AllA",
        "TalA",
        "IdoA",
        "Fuc",
        "Qui",
        "Rha",
        "6dGul",
        "6dAlt",
        "6dTal",
        "Oli",
        "Tyv",
        "Abe",
        "Par",
        "Dig",
        "Col",
        "Ara",
        "Lyx",
        "Xyl",
        "Rib"
      ),
      "2N"
    ),
    expect_ambiguous_residue
  )

  expect_ambiguous_residue(
    "u2122h_2*NCC/3=O_?*OSO/3=O/3=O",
    "GlcNAc",
    "?S"
  )
})


test_that("parse_residue maps generic WURCS descriptors to generic monosaccharides", {
  expect_equal(
    parse_residue("axxxxh-1x_1-5"),
    c(mono = "Hex", anomer = "?1", sub = "")
  )
  expect_equal(
    parse_residue("axxxxh-1b_1-5_2*NCC/3=O"),
    c(mono = "HexNAc", anomer = "b1", sub = "")
  )
  expect_equal(
    parse_residue("uxxxxm"),
    c(mono = "dHex", anomer = "??", sub = "")
  )
  expect_equal(
    parse_residue("u2112m"),
    c(mono = "dHex", anomer = "??", sub = "")
  )
  expect_equal(
    parse_residue("a4334m-1x_1-?"),
    c(mono = "dHex", anomer = "?1", sub = "")
  )
  expect_equal(
    parse_residue("hxxxxh_2*NCC/3=O"),
    c(mono = "HexNAc", anomer = "?1", sub = "")
  )
  expect_equal(
    parse_residue("AUd21122h_5*NCC/3=O"),
    c(mono = "Neu5Ac", anomer = "??", sub = "")
  )
})


test_that("parse_residue handles unknown ring closure", {
  expect_unknown_ring_residue <- function(residue, mono, sub = "") {
    expect_equal(
      parse_residue(residue),
      c(mono = mono, anomer = "?1", sub = sub)
    )
  }

  purrr::iwalk(
    c(
      Glc = "a2122h-1x_1-?",
      Man = "a1122h-1x_1-?",
      Gal = "a2112h-1x_1-?",
      Gul = "a2212h-1x_1-?",
      Alt = "a2111h-1x_1-?",
      All = "a2222h-1x_1-?",
      Tal = "a1112h-1x_1-?",
      Ido = "a2121h-1x_1-?",
      GlcA = "a2122A-1x_1-?",
      ManA = "a1122A-1x_1-?",
      GalA = "a2112A-1x_1-?",
      GulA = "a2212A-1x_1-?",
      AltA = "a2111A-1x_1-?",
      AllA = "a2222A-1x_1-?",
      TalA = "a1112A-1x_1-?",
      IdoA = "a2121A-1x_1-?",
      Fuc = "a1221m-1x_1-?",
      Qui = "a2122m-1x_1-?",
      Rha = "a2211m-1x_1-?",
      `6dGul` = "a2212m-1x_1-?",
      `6dAlt` = "a2111m-1x_1-?",
      `6dTal` = "a1112m-1x_1-?",
      Oli = "ad122m-1x_1-?",
      Tyv = "a1d22m-1x_1-?",
      Abe = "a2d12m-1x_1-?",
      Par = "a2d22m-1x_1-?",
      Dig = "ad222m-1x_1-?",
      Col = "a1d21m-1x_1-?",
      Ara = "a211h-1x_1-?",
      Lyx = "a221h-1x_1-?",
      Xyl = "a212h-1x_1-?",
      Rib = "a222h-1x_1-?"
    ),
    ~ expect_unknown_ring_residue(.x, .y)
  )

  purrr::iwalk(
    c(
      GlcNAc = "a2122h-1x_1-?_2*NCC/3=O",
      ManNAc = "a1122h-1x_1-?_2*NCC/3=O",
      GalNAc = "a2112h-1x_1-?_2*NCC/3=O",
      GulNAc = "a2212h-1x_1-?_2*NCC/3=O",
      AltNAc = "a2111h-1x_1-?_2*NCC/3=O",
      AllNAc = "a2222h-1x_1-?_2*NCC/3=O",
      TalNAc = "a1112h-1x_1-?_2*NCC/3=O",
      IdoNAc = "a2121h-1x_1-?_2*NCC/3=O",
      FucNAc = "a1221m-1x_1-?_2*NCC/3=O",
      QuiNAc = "a2122m-1x_1-?_2*NCC/3=O",
      RhaNAc = "a2211m-1x_1-?_2*NCC/3=O",
      `6dAltNAc` = "a2111m-1x_1-?_2*NCC/3=O",
      `6dTalNAc` = "a1112m-1x_1-?_2*NCC/3=O"
    ),
    ~ expect_unknown_ring_residue(.x, .y)
  )

  purrr::iwalk(
    c(
      GlcN = "a2122h-1x_1-?_2*N",
      ManN = "a1122h-1x_1-?_2*N",
      GalN = "a2112h-1x_1-?_2*N",
      GulN = "a2212h-1x_1-?_2*N",
      AltN = "a2111h-1x_1-?_2*N",
      AllN = "a2222h-1x_1-?_2*N",
      TalN = "a1112h-1x_1-?_2*N",
      IdoN = "a2121h-1x_1-?_2*N"
    ),
    ~ expect_unknown_ring_residue(.x, .y)
  )

  purrr::pwalk(
    list(
      c(
        "a2122A-1x_1-?_2*NCC/3=O",
        "a1122A-1x_1-?_2*NCC/3=O",
        "a2112A-1x_1-?_2*NCC/3=O",
        "a2212A-1x_1-?_2*NCC/3=O",
        "a2111A-1x_1-?_2*NCC/3=O",
        "a2222A-1x_1-?_2*NCC/3=O",
        "a1112A-1x_1-?_2*NCC/3=O",
        "a2121A-1x_1-?_2*NCC/3=O",
        "a2212m-1x_1-?_2*NCC/3=O",
        "ad122m-1x_1-?_2*NCC/3=O",
        "a1d22m-1x_1-?_2*NCC/3=O",
        "a2d12m-1x_1-?_2*NCC/3=O",
        "a2d22m-1x_1-?_2*NCC/3=O",
        "ad222m-1x_1-?_2*NCC/3=O",
        "a1d21m-1x_1-?_2*NCC/3=O",
        "a211h-1x_1-?_2*NCC/3=O",
        "a221h-1x_1-?_2*NCC/3=O",
        "a212h-1x_1-?_2*NCC/3=O",
        "a222h-1x_1-?_2*NCC/3=O"
      ),
      c(
        "GlcA",
        "ManA",
        "GalA",
        "GulA",
        "AltA",
        "AllA",
        "TalA",
        "IdoA",
        "6dGul",
        "Oli",
        "Tyv",
        "Abe",
        "Par",
        "Dig",
        "Col",
        "Ara",
        "Lyx",
        "Xyl",
        "Rib"
      ),
      "2NAc"
    ),
    expect_unknown_ring_residue
  )

  purrr::pwalk(
    list(
      c(
        "a2122A-1x_1-?_2*N",
        "a1122A-1x_1-?_2*N",
        "a2112A-1x_1-?_2*N",
        "a2212A-1x_1-?_2*N",
        "a2111A-1x_1-?_2*N",
        "a2222A-1x_1-?_2*N",
        "a1112A-1x_1-?_2*N",
        "a2121A-1x_1-?_2*N",
        "a1221m-1x_1-?_2*N",
        "a2122m-1x_1-?_2*N",
        "a2211m-1x_1-?_2*N",
        "a2212m-1x_1-?_2*N",
        "a2111m-1x_1-?_2*N",
        "a1112m-1x_1-?_2*N",
        "ad122m-1x_1-?_2*N",
        "a1d22m-1x_1-?_2*N",
        "a2d12m-1x_1-?_2*N",
        "a2d22m-1x_1-?_2*N",
        "ad222m-1x_1-?_2*N",
        "a1d21m-1x_1-?_2*N",
        "a211h-1x_1-?_2*N",
        "a221h-1x_1-?_2*N",
        "a212h-1x_1-?_2*N",
        "a222h-1x_1-?_2*N"
      ),
      c(
        "GlcA",
        "ManA",
        "GalA",
        "GulA",
        "AltA",
        "AllA",
        "TalA",
        "IdoA",
        "Fuc",
        "Qui",
        "Rha",
        "6dGul",
        "6dAlt",
        "6dTal",
        "Oli",
        "Tyv",
        "Abe",
        "Par",
        "Dig",
        "Col",
        "Ara",
        "Lyx",
        "Xyl",
        "Rib"
      ),
      "2N"
    ),
    expect_unknown_ring_residue
  )
})


test_that("letter_to_int handles uppercase WURCS residue IDs", {
  expect_equal(letter_to_int("z"), 26)
  expect_equal(letter_to_int("A"), 27)
  expect_equal(letter_to_int("K"), 37)
  expect_equal(
    parse_one_linkage("a6-K1"),
    list(from = 1, to = 37, linkage = "1-6")
  )
})


test_that("parse_residue handles N-sulfate on amino sugars", {
  expect_n_sulfate_residue <- function(residue, mono, anomer, sub = "2S") {
    expect_equal(
      parse_residue(residue),
      c(mono = mono, anomer = anomer, sub = sub)
    )
  }

  hexn_codes <- c(
    GlcN = "2122h",
    ManN = "1122h",
    GalN = "2112h",
    GulN = "2212h",
    AltN = "2111h",
    AllN = "2222h",
    TalN = "1112h",
    IdoN = "2121h"
  )

  purrr::iwalk(
    hexn_codes,
    ~ expect_n_sulfate_residue(
      stringr::str_glue("u{.x}_2*NSO/3=O/3=O"),
      .y,
      "??"
    )
  )
  purrr::iwalk(
    hexn_codes,
    ~ expect_n_sulfate_residue(
      stringr::str_glue("a{.x}-1x_1-5_2*NSO/3=O/3=O"),
      .y,
      "?1"
    )
  )
  purrr::iwalk(
    hexn_codes,
    ~ expect_n_sulfate_residue(
      stringr::str_glue("a{.x}-1x_1-?_2*NSO/3=O/3=O"),
      .y,
      "?1"
    )
  )
  purrr::iwalk(
    hexn_codes,
    ~ expect_n_sulfate_residue(
      stringr::str_glue("u{.x}_2*NSO/3=O/3=O_6*OSO/3=O/3=O"),
      .y,
      "??",
      "2S,6S"
    )
  )

  expect_equal(
    parse_residue("a2122h-1x_1-5_2*NSO/3=O/3=O_6*OSO/3=O/3=O"),
    c(mono = "GlcN", anomer = "?1", sub = "2S,6S")
  )
  expect_equal(
    parse_residue("a2122h-1x_1-?_2*NSO/3=O/3=O_6*OSO/3=O/3=O"),
    c(mono = "GlcN", anomer = "?1", sub = "2S,6S")
  )
})


test_that("parse_residue handles alditol residues", {
  expect_alditol_residue <- function(residue, mono) {
    anomer <- paste0("?", glyrepr::get_anomer_pos(mono))
    expect_equal(
      parse_residue(residue),
      c(mono = mono, anomer = anomer, sub = "")
    )
  }

  alditol_residues <- c(
    Glc = "h2122h",
    Man = "h1122h",
    Gal = "h2112h",
    Gul = "h2212h",
    Alt = "h2221h",
    All = "h2222h",
    Tal = "h1222h",
    Ido = "h2121h",
    GlcNAc = "h2122h_2*NCC/3=O",
    GalNAc = "h2112h_2*NCC/3=O",
    ManNAc = "h1122h_2*NCC/3=O",
    GulNAc = "h2212h_2*NCC/3=O",
    AltNAc = "h2221h_5*NCC/3=O",
    AllNAc = "h2222h_2*NCC/3=O",
    TalNAc = "h1222h_5*NCC/3=O",
    IdoNAc = "h2121h_2*NCC/3=O",
    GlcN = "h2122h_2*N",
    ManN = "h1122h_2*N",
    GalN = "h2112h_2*N",
    GulN = "h2212h_2*N",
    AltN = "h2221h_5*N",
    AllN = "h2222h_2*N",
    TalN = "h1222h_5*N",
    IdoN = "h2121h_2*N",
    Fuc = "h1221m",
    Qui = "h2122m",
    Rha = "h2211m",
    `6dGul` = "h2212m",
    `6dAlt` = "h2111m",
    `6dTal` = "h1112m",
    FucNAc = "h1221m_2*NCC/3=O",
    QuiNAc = "h2122m_2*NCC/3=O",
    RhaNAc = "h2211m_2*NCC/3=O",
    `6dAltNAc` = "h2111m_2*NCC/3=O",
    `6dTalNAc` = "h1112m_2*NCC/3=O",
    Oli = "hd122m",
    Tyv = "h1d22m",
    Abe = "h2d12m",
    Par = "h2d22m",
    Dig = "hd222m",
    Col = "h1d21m",
    Lyx = "h221h",
    Xyl = "h212h",
    Rib = "h222h",
    Neu5Ac = "hUd21122h_5*NCC/3=O",
    Neu5Gc = "hUd21122h_5*NCCO/3=O",
    Kdn = "hUd21122h",
    Neu = "hUd21122h_5*N",
    Pse = "hUd22111m_5*N_7*N",
    Leg = "hUd21122m_5*N_7*N",
    Aci = "hUd21111m_5*N_7*N",
    `4eLeg` = "hUd11122m_5*N_7*N",
    Bac = "h2122m_2*N_4*N",
    LDmanHep = "h21122h",
    Kdo = "hUd1122h",
    Dha = "A122dUh",
    DDmanHep = "h11222h",
    MurNAc = "h2122h_2*NCC/3=O_3*OC^RCO/4=O/3C",
    MurNGc = "h2122h_2*NCCO/3=O_3*OC^RCO/4=O/3C",
    Mur = "h2122h_3*OC^RCO/4=O/3C",
    Fru = "hU122h",
    Tag = "hU112h",
    Sor = "hU121h",
    Psi = "hU222h"
  )

  purrr::iwalk(alditol_residues, ~ expect_alditol_residue(.x, .y))

  # GlycanFormatConverter emits h221h for both Ara-ol and Lyx-ol. WURCS
  # canonicalizes that alditol descriptor back to Lyx-ol, so parse the distinct
  # descriptor deterministically.
  expect_alditol_residue("h221h", "Lyx")
})


test_that("parse_wurcs warns and uses unknown reducing-end anomers for alditols", {
  glcnac_alditol <- "WURCS=2.0/1,1,0/[h2122h_2*NCC/3=O]/1/"
  expect_warning(
    glcnac_structure <- parse_wurcs(glcnac_alditol),
    "regular reducing-end glycans with unknown anomer configurations"
  )
  expect_equal(as.character(glcnac_structure), "GlcNAc(?1-")

  linked_alditol <- "WURCS=2.0/2,2,1/[h2122h_2*NCC/3=O][a2112h-1b_1-5]/1-2/a4-b1"
  expect_warning(
    linked_structure <- parse_wurcs(linked_alditol),
    "regular reducing-end glycans with unknown anomer configurations"
  )
  expect_equal(as.character(linked_structure), "Gal(b1-4)GlcNAc(?1-")
})


test_that("parse_wurcs correctly handles unknown linkages", {
  expect_equal(
    as.character(parse_wurcs(
      "WURCS=2.0/2,2,1/[a2112h-1a_1-5_2*NCC/3=O][a2112h-1a_1-5]/1-2/b1-a3|a4"
    )),
    "Gal(a1-3/4)GalNAc(a1-"
  )
  expect_equal(
    as.character(parse_wurcs(
      "WURCS=2.0/2,2,1/[a2112h-1a_1-5_2*NCC/3=O][a2112h-1a_1-5]/1-2/a?-b1"
    )),
    "Gal(a1-?)GalNAc(a1-"
  )
  expect_equal(
    as.character(parse_wurcs(
      "WURCS=2.0/2,2,1/[a2112h-1a_1-5_2*NCC/3=O][a2112h-1a_1-5]/1-2/a3-b?"
    )),
    "Gal(a?-3)GalNAc(a1-"
  )
})


test_that("parse_wurcs handles ambiguous u residues", {
  expect_equal(
    as.character(parse_wurcs(
      "WURCS=2.0/3,4,3/[u2122h][a2112h-1b_1-5][a2122h-1b_1-5_2*NCC/3=O]/1-2-3-2/a4-b1_b3-c1_c4-d1"
    )),
    "Gal(b1-4)GlcNAc(b1-3)Gal(b1-4)Glc(??-"
  )
  expect_equal(
    as.character(parse_wurcs(
      "WURCS=2.0/2,2,1/[u2112h_2*NCC/3=O][a2112h-1a_1-5_2*NCC/3=O]/1-2/a6-b1"
    )),
    "GalNAc(a1-6)GalNAc(??-"
  )
  expect_equal(
    as.character(parse_wurcs(
      "WURCS=2.0/2,2,1/[u2122A][a2122h-1x_1-5_2*NCC/3=O]/1-2/a?-b1"
    )),
    "GlcNAc(?1-?)GlcA(??-"
  )
})


test_that("parse_wurcs handles unknown ring closure", {
  expect_equal(
    as.character(parse_wurcs(
      "WURCS=2.0/5,6,5/[a2122h-1x_1-?_2*NCC/3=O][a1221m-1a_1-5][a2122h-1b_1-5_2*NCC/3=O][a1122h-1b_1-5][a1122h-1a_1-5]/1-2-3-4-5-5/a4-c1_c4-d1_d3-e1_d6-f1_a?-b1"
    )),
    "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)[Fuc(a1-?)]GlcNAc(?1-"
  )
})


test_that("parse_wurcs handles N-sulfate on amino sugars", {
  expect_equal(
    as.character(parse_wurcs(
      "WURCS=2.0/2,2,1/[u2122h_2*NSO/3=O/3=O][a2122A-1x_1-5]/1-2/a?-b1"
    )),
    "GlcA(?1-?)GlcN2S(??-"
  )
  expect_equal(
    as.character(parse_wurcs(
      "WURCS=2.0/2,2,1/[u2122A][a2122h-1x_1-5_2*NSO/3=O/3=O]/1-2/a?-b1"
    )),
    "GlcN2S(?1-?)GlcA(??-"
  )

  structure <- parse_wurcs(
    "WURCS=2.0/1,1,0/[u2122h_2*NSO/3=O/3=O_6*OSO/3=O/3=O]/1/"
  )
  graph <- glyrepr::get_structure_graphs(structure)
  expect_equal(igraph::vertex_attr(graph, "mono"), "GlcN")
  expect_equal(igraph::vertex_attr(graph, "sub"), "2S,6S")
  expect_equal(graph$anomer, "??")
})


test_that("anomer is corrected", {
  expect_anomer <- function(wurcs, anomer) {
    structure <- parse_wurcs(wurcs)
    graph <- glyrepr::get_structure_graphs(structure)
    expect_equal(graph$anomer, anomer)
  }
  wurcs_a <- "WURCS=2.0/2,2,1/[a2122h-1a_1-5_2*NCC/3=O][a2122h-1b_1-5_2*NCC/3=O]/1-2/a4-b1"
  wurcs_b <- "WURCS=2.0/2,2,1/[a2122h-1b_1-5_2*NCC/3=O][a2122h-1a_1-5_2*NCC/3=O]/1-2/a4-b1"
  wurcs_x <- "WURCS=2.0/2,2,1/[a2122h-1x_1-5_2*NCC/3=O][a2122h-1a_1-5_2*NCC/3=O]/1-2/a4-b1"
  expect_anomer(wurcs_a, "a1")
  expect_anomer(wurcs_b, "b1")
  expect_anomer(wurcs_x, "?1")
})


test_that("glycan with only one monosacharide", {
  wurcs <- "WURCS=2.0/1,1,0/[a2122h-1b_1-5_2*NCC/3=O]/1/"
  expect_snapshot(parse_wurcs(wurcs))
})


# A basic example
test_that("N-glycan core", {
  wurcs <- "WURCS=2.0/4,5,4/[a2122h-1x_1-5_2*NCC/3=O][a2122h-1b_1-5_2*NCC/3=O][a1122h-1b_1-5][a1122h-1a_1-5]/1-2-3-4-4/a4-b1_b4-c1_c3-d1_c6-e1"
  expect_snapshot(parse_wurcs(wurcs))
})


# A more complex example
test_that("H5H4F1S2", {
  wurcs <- "WURCS=2.0/6,12,11/[a2122h-1b_1-5_2*NCC/3=O][a1122h-1b_1-5][a1122h-1a_1-5][a2112h-1b_1-5][Aad21122h-2a_2-6_5*NCC/3=O][a1221m-1a_1-5]/1-1-2-3-1-4-5-3-1-4-5-6/a4-b1_a6-l1_b4-c1_c3-d1_c6-h1_d2-e1_e4-f1_f3-g2_h2-i1_i4-j1_j3-k2"
  expect_snapshot(parse_wurcs(wurcs))
})
