test_that("IUPAC-compact parses representative corpus examples", {
  skip_on_old_win()

  to_parse <- c(
    alias = "NeuGc?2-?NeuGc?2-?Gal?1-?Glc",
    simple = "Glcb1-3Glcb1-3Glcb",
    branched = "Mana1-3(Mana1-6)Manb1-4GlcNAcb",
    linkage_alternatives = "GlcNAcb1-2|4Mana1-3(Mana1-3|6Mana1-6)Manb1-4GlcNAcb1-4GlcNAc",
    complex = "NeuAca2-3|6Galb1-4GlcNAcb1-2Mana1-3(Galb1-3Galb1-4GlcNAcb1-2(Galb1-3Galb1-4GlcNAcb1-4)Mana1-6)Manb1-4GlcNAcb1-4(Fuca1-6)GlcNAcb",
    terminal_plain = "Galb1-3(Fuca1-4)GlcNAc"
  )

  parsed <- parse_iupac_compact(to_parse)

  expect_s3_class(parsed, "glyrepr_structure")
  expect_named(parsed, names(to_parse))
  expect_identical(
    as.character(parsed),
    c(
      alias = "Neu5Gc(?2-?)Neu5Gc(?2-?)Gal(?1-?)Glc(?1-",
      simple = "Glc(b1-3)Glc(b1-3)Glc(b1-",
      branched = "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-",
      linkage_alternatives = "GlcNAc(b1-2/4)Man(a1-3)[Man(a1-3/6)Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(?1-",
      complex = "Neu5Ac(a2-3/6)Gal(b1-4)GlcNAc(b1-2)Man(a1-3)[Gal(b1-3)Gal(b1-4)GlcNAc(b1-2)[Gal(b1-3)Gal(b1-4)GlcNAc(b1-4)]Man(a1-6)]Man(b1-4)GlcNAc(b1-4)[Fuc(a1-6)]GlcNAc(b1-",
      terminal_plain = "Gal(b1-3)[Fuc(a1-4)]GlcNAc(?1-"
    )
  )
})

test_that("IUPAC-compact preserves NA and name semantics", {
  skip_on_old_win()

  to_parse <- c(valid = "Galb1-3GalNAca", missing = NA_character_)

  parsed <- parse_iupac_compact(to_parse)

  expect_named(parsed, names(to_parse))
  expect_identical(
    as.character(parsed),
    c(valid = "Gal(b1-3)GalNAc(a1-", missing = NA_character_)
  )
})

test_that("IUPAC-compact vectorizes terminal anomer positions", {
  expect_identical(
    iupac_compact_default_anomer_pos(c("Glc", "Neu5Ac", "Hex")),
    c("1", "2", "1")
  )

  parsed <- parse_iupac_compact(c("Glcb", "Neu5Aca"))
  expect_identical(as.character(parsed), c("Glc(b1-", "Neu5Ac(a2-"))
})

test_that("IUPAC-compact supports on_failure handling", {
  skip_on_old_win()

  parsed <- parse_iupac_compact(
    c(valid = "Glcb1-3Glcb", invalid = "not a glycan"),
    on_failure = "na"
  )

  expect_identical(
    as.character(parsed),
    c(valid = "Glc(b1-3)Glc(b1-", invalid = NA_character_)
  )
})

test_that("IUPAC-compact converts corpus modifier notation", {
  skip_on_old_win()

  to_parse <- c(
    sulfate = "Gal?1-?GlcNAc?1-3(Gal?1-3(6S)GlcNAc?1-6)GalNAca",
    unknown_sulfate_position = "Gal?1-?(S)GlcNAc?1-?GalNAca",
    terminal_sulfate = "(6S)Galb1-4(6S)GlcNAc",
    terminal_sulfate_anomer = "Galb1-4(6S)GlcNAca",
    sialic_acetate = "Neu5,9Ac2?2-?Gal?1-?Glc"
  )

  parsed <- parse_iupac_compact(to_parse)

  expect_identical(
    as.character(parsed),
    c(
      sulfate = "Gal(?1-?)GlcNAc(?1-3)[Gal(?1-3)GlcNAc6S(?1-6)]GalNAc(a1-",
      unknown_sulfate_position = "Gal(?1-?)GlcNAc?S(?1-?)GalNAc(a1-",
      terminal_sulfate = "Gal6S(b1-4)GlcNAc6S(?1-",
      terminal_sulfate_anomer = "Gal(b1-4)GlcNAc6S(a1-",
      sialic_acetate = "Neu5Ac9Ac(?2-?)Gal(?1-?)Glc(?1-"
    )
  )
})

test_that("IUPAC-compact warns and parses alditols as regular reducing ends", {
  skip_on_old_win()

  simple_alditol <- "Fuca1-3(Galb1-4)Glc+aldi"
  expect_warning(
    simple_structure <- parse_iupac_compact(simple_alditol),
    "regular reducing-end glycans with unknown anomer configurations"
  )
  expect_equal(as.character(simple_structure), "Fuc(a1-3)[Gal(b1-4)]Glc(?1-")

  linked_alditol <- "Galb1-3(Gal?1-?GlcNAcb1-3Galb1-4GlcNAcb1-6)GalNAc+aldi"
  expect_warning(
    linked_structure <- parse_iupac_compact(linked_alditol),
    "regular reducing-end glycans with unknown anomer configurations"
  )
  expect_equal(
    as.character(linked_structure),
    "Gal(?1-?)GlcNAc(b1-3)Gal(b1-4)GlcNAc(b1-6)[Gal(b1-3)]GalNAc(?1-"
  )
})
