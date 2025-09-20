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


test_that("parse_wurcs correctly handles unknown linkages", {
  expect_equal(
    as.character(parse_wurcs("WURCS=2.0/2,2,1/[a2112h-1a_1-5_2*NCC/3=O][a2112h-1a_1-5]/1-2/b1-a3|a4")),
    "Gal(a1-3/4)GalNAc(a1-"
  )
  expect_equal(
    as.character(parse_wurcs("WURCS=2.0/2,2,1/[a2112h-1a_1-5_2*NCC/3=O][a2112h-1a_1-5]/1-2/a?-b1")),
    "Gal(a1-?)GalNAc(a1-"
  )
  expect_equal(
    as.character(parse_wurcs("WURCS=2.0/2,2,1/[a2112h-1a_1-5_2*NCC/3=O][a2112h-1a_1-5]/1-2/a3-b?")),
    "Gal(a?-3)GalNAc(a1-"
  )
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
