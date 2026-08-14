test_that("GlycoWorkbench parses branches and ignores mass options", {
  input <- c(
    branched = paste0(
      "freeEnd--1b1D-GlcNAc,p(--6a1L-Fuc,p)",
      "--4b1D-Gal,p--3a2D-NeuAc,p$MONO,Und,0,0,freeEnd"
    )
  )

  parsed <- parse_gwb(input)

  expect_s3_class(parsed, "glyrepr_structure")
  expect_named(parsed, "branched")
  expect_identical(
    as.character(parsed),
    c(branched = "Neu5Ac(a2-3)Gal(b1-4)[Fuc(a1-6)]GlcNAc(b1-")
  )
})

test_that("GlycoWorkbench agrees with normalized IUPAC corpus examples", {
  gwb <- c(
    paste0(
      "freeEnd--1b1D-Gal,p--4a1D-Gal,f--2a1D-Gal,f",
      "$MONO,Und,0,0,freeEnd"
    ),
    paste0(
      "freeEnd--1a1D-GalNAc,p(--3?1D-GlcNAc,p--??1D-Gal,p)",
      "--6?1D-GlcNAc,p(--6S)--3?1D-Gal,p$MONO,Und,0,0,freeEnd"
    ),
    paste0(
      "freeEnd--1?1D-GlcNAc,p(--??1D-GlcNAc,p--??1D-Man,p(",
      "--??1D-Man,p--??1D-GlcNAc,p--??1D-Gal,p--??2D-NeuAc,p",
      "--9Ac)--??1D-Man,p--??1D-GlcNAc,p--??1D-Gal,p",
      "--??2D-NeuAc,p)--??1L-Fuc,p$MONO,Und,0,0,freeEnd"
    )
  )
  iupac <- c(
    "Galf(a1-2)Galf(a1-4)Gal(b1-",
    "Gal(?1-?)GlcNAc(?1-3)[Gal(?1-3)GlcNAc6S(?1-6)]GalNAc(a1-",
    paste0(
      "Neu5Ac9Ac(?2-?)Gal(?1-?)GlcNAc(?1-?)Man(?1-?)[",
      "Neu5Ac(?2-?)Gal(?1-?)GlcNAc(?1-?)Man(?1-?)]Man(?1-?)",
      "GlcNAc(?1-?)[Fuc(?1-?)]GlcNAc(?1-"
    )
  )

  expect_identical(
    as.character(parse_gwb(gwb)),
    as.character(parse_iupac_condensed(iupac))
  )
})

test_that("GlycoWorkbench preserves modifications and configurations", {
  input <- c(
    substituents = paste0(
      "freeEnd--1a1D-GlcN,p((--2NS)--6S)",
      "--4b1D-Gal,p--9Ac$MONO,Und,0,0,freeEnd"
    ),
    deoxy = "freeEnd--1?1Hex--6m$MONO,Und,0,0,freeEnd",
    configurations = paste0(
      "freeEnd--1a1D-Fuc,p--3b1L-Gul,p",
      "$MONO,Und,0,0,freeEnd"
    )
  )

  expect_identical(
    as.character(parse_gwb(input[c("substituents", "configurations")])),
    c(
      substituents = "Gal9Ac(b1-4)GlcN2S6S(a1-",
      configurations = "L-Gul(b1-3)D-Fuc(a1-"
    )
  )
  expect_identical(
    as.character(parse_gwb(input["deoxy"])),
    c(deoxy = "dHex(?1-")
  )
})

test_that("GlycoWorkbench preserves reducing-end chemistry", {
  input <- c(
    reduced = paste0(
      "redEnd--?D-GlcNAc,o--4b1D-Gal,p",
      "$MONO,Und,0,0,redEnd"
    ),
    keto_marker = paste0(
      "freeEnd--?D-Fru(--2U)--??1D-Glc,p",
      "$MONO,Und,0,0,freeEnd"
    ),
    kdn_marker = "freeEnd--?D-Kdn--2U$MONO,Und,0,0,freeEnd"
  )

  parsed <- parse_gwb(input)

  expect_identical(
    as.character(parsed),
    c(
      reduced = "Gal(b1-4)GlcNAc-ol(?1-",
      keto_marker = "Glc(?1-?)Fru-ol(?2-",
      kdn_marker = "Kdn(?2-"
    )
  )
  expect_identical(
    unname(glyrepr::get_alditol(parsed)),
    c(TRUE, TRUE, FALSE)
  )
})

test_that("GlycoWorkbench rejects unrepresentable open-chain residues", {
  expect_error(
    parse_gwb("freeEnd--?D-Glc,o$MONO,Und,0,0,freeEnd"),
    "Can't parse"
  )
  expect_error(
    parse_gwb(
      paste0(
        "freeEnd--1a1D-Gal,p--6D-Glc,o",
        "$MONO,Und,0,0,freeEnd"
      )
    ),
    "Can't parse"
  )
})

test_that("GlycoWorkbench parses uncertain antennae", {
  input <- c(
    one_part = paste0(
      "freeEnd--1b1D-GlcNAc,p--4b1D-Gal,p}",
      "--3a2D-NeuAc,p$MONO,Und,0,0,freeEnd"
    ),
    multiple_parts = paste0(
      "freeEnd--1b1D-GlcNAc,p--4b1D-Gal,p}",
      "(--3a2D-NeuAc,p)--6a1L-Fuc,p$MONO,Und,0,0,freeEnd"
    ),
    substituent = paste0(
      "freeEnd--1b1D-GlcNAc,p--4b1D-Gal,p}",
      "--6S$MONO,Und,0,0,freeEnd"
    )
  )

  expect_identical(
    as.character(parse_gwb(input)),
    c(
      one_part = "{Neu5Ac(a2-3)}Gal(b1-4)GlcNAc(b1-",
      multiple_parts = paste0(
        "{Fuc(a1-6)|3,4}{Neu5Ac(a2-3)|3,4}",
        "Gal(b1-4)GlcNAc(b1-"
      ),
      substituent = "{6S}Gal(b1-4)GlcNAc(b1-"
    )
  )
})

test_that("GlycoWorkbench excludes occupied uncertain-antenna parents", {
  input <- c(
    monosaccharide = paste0(
      "freeEnd--1b1D-GlcNAc,p(--6a1L-Fuc,p)--4b1D-Gal,p}",
      "--6a1D-Man,p$MONO,Und,0,0,freeEnd"
    ),
    substituent = paste0(
      "freeEnd--1b1D-GlcNAc,p(--6a1L-Fuc,p)--4b1D-Gal,p}",
      "--6S$MONO,Und,0,0,freeEnd"
    )
  )

  expect_identical(
    as.character(parse_gwb(input)),
    c(
      monosaccharide = paste0(
        "{Man(a1-6)|2,3}Gal(b1-4)[Fuc(a1-6)]GlcNAc(b1-"
      ),
      substituent = "{6S|1,2}Gal(b1-4)[Fuc(a1-6)]GlcNAc(b1-"
    )
  )
})

test_that("GlycoWorkbench supports parser wrapper semantics", {
  input <- c(
    valid = "freeEnd--1b1D-Glc,p$MONO,Und,0,0,freeEnd",
    missing = NA_character_,
    invalid = "not a GlycoWorkbench structure"
  )

  parsed <- parse_gwb(input, on_failure = "na")

  expect_named(parsed, names(input))
  expect_identical(
    as.character(parsed),
    c(valid = "Glc(b1-", missing = NA_character_, invalid = NA_character_)
  )
})

test_that("GlycoWorkbench supports dropping generic structures", {
  input <- c(
    generic = "freeEnd--1?1Hex$MONO,Und,0,0,freeEnd",
    concrete = "freeEnd--1b1D-Glc,p$MONO,Und,0,0,freeEnd"
  )

  expect_error(parse_gwb(input), "same monosaccharide type")
  expect_message(
    parsed <- parse_gwb(input, drop_generic = TRUE),
    "Dropped 1 generic glycan"
  )
  expect_identical(
    as.character(parsed),
    c(generic = NA_character_, concrete = "Glc(b1-")
  )
})

test_that("GlycoWorkbench rejects malformed or unsupported constructs", {
  expect_error(parse_gwb("Glc"), "Can't parse")
  expect_error(
    parse_gwb("freeEnd--1b1D-Glc,p--2U$MONO,Und,0,0,freeEnd"),
    "Can't parse"
  )
  expect_error(
    parse_gwb("freeEnd--1b1D-Glc,p--1=2,3S$MONO,Und,0,0,freeEnd"),
    "Can't parse"
  )
})

test_that("auto_parse detects GlycoWorkbench structures", {
  input <- "freeEnd--1b1D-Gal,p--4b1D-Glc,p$MONO,Und,0,0,freeEnd"

  expect_identical(
    as.character(auto_parse(input)),
    as.character(parse_gwb(input))
  )
})
