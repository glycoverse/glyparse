test_that("monosaccharides are parsed correctly", {
  expect_mono <- function(iupac_ext, mono) {
    structure <- parse_iupac_extended(iupac_ext)
    graph <- glyrepr::get_structure_graphs(structure)
    expect_equal(igraph::V(graph)$mono, mono)
  }
  expect_mono("α-D-Glcp-(1→", "Glc")
  expect_mono("α-D-GlcpNAc-(1→", "GlcNAc")
  expect_mono("α-D-GlcpA-(1→", "GlcA")
  expect_mono("α-D-GlcpN-(1→", "GlcN")
  expect_mono("α-D-Neup5Ac-(2→", "Neu5Ac")
  expect_mono("α-D-Neup5Gc-(2→", "Neu5Gc")
  expect_mono("α-D-MurpN2Gc-(1→", "MurNGc")
  expect_mono("α-L-Eryp3CMeOH-(1→", "Api")
  expect_mono("L-gro-α-D-manHepp-(1→", "LDmanHep")
  expect_mono("D-gro-α-D-manHepp-(1→", "DDmanHep")
})


test_that("generic monosaccharides are parsed correctly", {
  expect_mono <- function(iupac_ext, mono) {
    structure <- parse_iupac_extended(iupac_ext)
    graph <- glyrepr::get_structure_graphs(structure)
    expect_equal(igraph::V(graph)$mono, mono)
  }
  expect_mono("α-?-Hexp-(1→", "Hex")
  expect_mono("α-?-HexpNAc-(1→", "HexNAc")
  expect_mono("α-?-HexpA-(1→", "HexA")
  expect_mono("α-?-HexpN-(1→", "HexN")
})


test_that("substituents are parsed correctly", {
  expect_mono_and_sub <- function(iupac_ext, mono, sub) {
    structure <- parse_iupac_extended(iupac_ext)
    graph <- glyrepr::get_structure_graphs(structure)
    expect_equal(igraph::V(graph)$mono, mono)
    expect_equal(igraph::V(graph)$sub, sub)
  }
  expect_mono_and_sub("α-D-Glcp3Me-(1→", "Glc", "3Me")
  expect_mono_and_sub("α-D-GlcpNAc9Ac-(1→", "GlcNAc", "9Ac")
  expect_mono_and_sub("α-D-GlcpA2N-(1→", "GlcA", "2N")
  expect_mono_and_sub("α-D-Glcp?Me-(1→", "Glc", "?Me")
  expect_mono_and_sub("α-D-Glcp3Me6S-(1→", "Glc", "3Me,6S")
})


test_that("anomers are parsed correctly", {
  expect_anomer <- function(iupac_ext, anomer) {
    structure <- parse_iupac_extended(iupac_ext)
    graph <- glyrepr::get_structure_graphs(structure)
    expect_equal(graph$anomer, anomer)
  }
  expect_anomer("α-D-Glcp-(1→", "a1")
  expect_anomer("β-D-Glcp-(1→", "b1")
  expect_anomer("?-D-Glcp-(1→", "?1")
})


test_that("linkages are parsed correctly", {
  expect_linkage <- function(iupac_ext, linkage) {
    structure <- parse_iupac_extended(iupac_ext)
    graph <- glyrepr::get_structure_graphs(structure)
    expect_equal(igraph::E(graph)$linkage, linkage)
  }
  expect_linkage("α-D-Galp-(1→4)-β-D-Galp-(1→", "a1-4")
  expect_linkage("β-D-Galp-(1→4)-β-D-Galp-(1→", "b1-4")
  expect_linkage("?-D-Galp-(1→4)-β-D-Galp-(1→", "?1-4")
  expect_linkage("α-D-Galp-(1→?)-β-D-Galp-(1→", "a1-?")
  expect_linkage("α-D-Galp-(1→3/4)-β-D-Galp-(1→", "a1-3/4")
})


test_that("parsing N-glycan core", {
  iupac <- "α-D-Manp-(1→3)[α-D-Manp-(1→6)]-β-D-Manp-(1→4)-β-D-GlcpNAc-(1→4)-β-D-GlcpNAc-(1→"
  expect_snapshot(parse_iupac_extended(iupac))
})
