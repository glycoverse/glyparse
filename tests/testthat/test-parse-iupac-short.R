test_that("Monosaccharides are correctly parsed", {
  expect_mono <- function(x, mono) {
    structure <- parse_iupac_short(x)
    graph <- glyrepr::get_structure_graphs(structure)
    expect_equal(igraph::V(graph)$mono, mono)
  }
  expect_mono("Glc", "Glc")
  expect_mono("GlcNAc", "GlcNAc")
  expect_mono("GlcA", "GlcA")
  expect_mono("GlcN", "GlcN")
  expect_mono("Neu5Ac", "Neu5Ac")
})


test_that("substituents are correctly parsed", {
  expect_sub <- function(x, sub) {
    structure <- parse_iupac_short(x)
    graph <- glyrepr::get_structure_graphs(structure)
    expect_equal(igraph::V(graph)$sub, sub)
  }
  expect_sub("Glc", "")
  expect_sub("GlcNAc", "")
  expect_sub("GlcA", "")
  expect_sub("GlcN", "")
  expect_sub("Neu5Ac", "")
  expect_sub("Glc3Me", "3Me")
  expect_sub("GlcNAc4S", "4S")
  expect_sub("Neu5Ac9Ac", "9Ac")
  expect_sub("Glc?Me", "?Me")
  expect_sub("Glc3Me6S", "3Me,6S")
})


test_that("anomers are correctly parsed", {
  expect_anomer <- function(x, anomer) {
    structure <- parse_iupac_short(x)
    graph <- glyrepr::get_structure_graphs(structure)
    expect_equal(graph$anomer, anomer)
  }
  expect_anomer("Glc", "?1")
  expect_anomer("Neu5Ac", "?2")
  expect_anomer("Glca-", "a1")
  expect_anomer("GlcAa-", "a1")
  expect_anomer("GlcAb-", "b1")
  expect_anomer("Glc?-", "?1")
  expect_anomer("Neu5Aca-", "a2")
})


test_that("linkages are correctly parsed", {
  expect_linkages <- function(x, linkages) {
    structure <- parse_iupac_short(x)
    graph <- glyrepr::get_structure_graphs(structure)
    expect_equal(igraph::E(graph)$linkage, linkages)
  }
  expect_linkages("Mana3Man", "a1-3")
  expect_linkages("GlcNAcb4GlcNAc", "b1-4")
  expect_linkages("Neu5Aca3Gal", "a2-3")
  expect_linkages("Glc?3Glc", "?1-3")
  expect_linkages("Neu5Aca?Gal", "a2-?")
})


test_that("parse_iupac_short works for a complex example", {
  x <- "Neu5Aca3Galb4GlcNAcb2Mana3(Neu5Aca3Galb4GlcNAcb2Mana6)Manb4GlcNAcb4(Fuca6)GlcNAcb-"
  expect_snapshot(parse_iupac_short(x))
})


test_that("parse_iupac_short works for a complex example with multiple substituents", {
  x <- "Glc3Me6Sa3Man"
  structure <- parse_iupac_short(x)
  expect_equal(as.character(structure), "Glc3Me6S(a1-3)Man(?1-")
})
