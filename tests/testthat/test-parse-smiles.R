smiles_glucose <- "C([C@@H]1[C@H]([C@@H]([C@H]([C@@H](O1)O)O)O)O)O"

test_that("native SMILES matches real GlyGen WURCS residue graphs", {
  skip_if_not_installed("igraph")
  d <- read.csv(test_path("fixtures", "glygen-smiles-wurcs.csv"))
  a <- parse_smiles(d$sequence_smiles_isomeric, on_failure = "na")
  b <- suppressWarnings(parse_wurcs(d$sequence_wurcs, on_failure = "na"))
  rejected <- d$glytoucan_ac %in% c("G89097YO", "G27289CV")
  expect_equal(is.na(a), rejected)
  # These source SMILES contain no stereochemical information, unlike their
  # paired WURCS. Do not manufacture alpha/beta to match the database.
  unknown <- d$glytoucan_ac %in% c("G20934UH", "G71958ED")
  expect_true(all(grepl("?1-", as.character(a[unknown]), fixed = TRUE)))
  for (i in which(!rejected & !unknown)) {
    expect_identical(
      smiles_graph_identity(a[i]),
      smiles_graph_identity(b[i]),
      info = d$glytoucan_ac[i]
    )
  }
})

test_that("equivalent atom traversals and ring labels are invariant", {
  d <- read.csv(test_path("fixtures", "glygen-smiles-equivalent.csv"))
  expect_identical(
    as.character(parse_smiles(d$original, on_failure = "na")),
    as.character(parse_smiles(d$equivalent, on_failure = "na"))
  )
  expect_identical(
    as.character(parse_smiles(gsub("1", "%10", smiles_glucose, fixed = TRUE))),
    as.character(parse_smiles(smiles_glucose))
  )
})

test_that("stereochemistry comes from input rather than sugar defaults", {
  expect_identical(as.character(parse_smiles(smiles_glucose)), "Glc(b1-")
  no_stereo <- gsub("\\[C@@?H\\]", "C", smiles_glucose)
  expect_identical(as.character(parse_smiles(no_stereo)), "Hex(?1-")
  mirror <- gsub("@@", "#", smiles_glucose, fixed = TRUE)
  mirror <- gsub("@", "@@", mirror, fixed = TRUE)
  mirror <- gsub("#", "@", mirror, fixed = TRUE)
  expect_identical(as.character(parse_smiles(mirror)), "L-Glc(b1-")
})

test_that("SMILES vectors preserve missing values, names and failures", {
  x <- c(
    beta = smiles_glucose,
    missing = NA_character_,
    invalid = "CC",
    again = smiles_glucose
  )
  a <- parse_smiles(x, on_failure = "na")
  expect_identical(names(a), names(x))
  expect_identical(unname(is.na(a)), c(FALSE, TRUE, TRUE, FALSE))
  expect_identical(unname(as.character(a[c(1, 4)])), rep("Glc(b1-", 2))
  expect_length(parse_smiles(character()), 0L)
  expect_true(all(is.na(parse_smiles(c(NA_character_, NA_character_)))))
  expect_error(parse_smiles(x), "Can't parse")
  expect_error(parse_smiles(1), "character")
  expect_error(parse_smiles(x, on_failure = "ignore"))
  expect_error(parse_smiles(x, progress = NA))
})

test_that("malformed and out-of-scope SMILES fail without dropping atoms", {
  bad <- c(
    "",
    "C1",
    "C=",
    "C()",
    "C(C",
    "C)C",
    "C%1",
    "C11",
    "C1C1",
    "C(C)(C)(C)(C)C",
    "[C@H]",
    "[13C]",
    "[O-]",
    "[Na+]",
    "c1ccccc1",
    paste0(smiles_glucose, ".O"),
    paste0(smiles_glucose, " title"),
    sub("(O1)O", "(O1)OC", smiles_glucose, fixed = TRUE), # methyl glycoside
    sub("\\[C@@H\\]", "[C@TH1H]", smiles_glucose),
    "O=CC(O)C(O)C(O)C(O)CO" # open chain not currently represented
  )
  expect_true(all(is.na(parse_smiles(bad, on_failure = "na"))))
  expect_error(parse_smiles(bad), "Can't parse")
})

test_that("SMILES native batch execution is thread-independent", {
  d <- read.csv(test_path("fixtures", "glygen-smiles-wurcs.csv"))
  old <- options(glyparse.threads = 1L)
  on.exit(options(old))
  serial <- as.character(parse_smiles(
    d$sequence_smiles_isomeric,
    on_failure = "na"
  ))
  options(glyparse.threads = 2L)
  expect_identical(
    serial,
    as.character(parse_smiles(d$sequence_smiles_isomeric, on_failure = "na"))
  )
  expect_identical(
    serial,
    as.character(parse_smiles(
      d$sequence_smiles_isomeric,
      on_failure = "na",
      progress = TRUE
    ))
  )
})


test_that("symmetric alditol numbering includes the complete attached chemistry", {
  d <- read.csv(test_path("fixtures", "smiles-symmetric-alditols.csv"))
  for (group in unique(d$group)) {
    result <- parse_smiles(d$smiles[d$group == group])
    expect_false(anyNA(result))
    expect_length(unique(as.character(result)), 1L)
  }
})

test_that("WURCS explicit relative anomers are not inferred from unknown backbones", {
  specified <- c(
    "WURCS=2.0/1,1,0/[axxxh-1b_1-4]/1/",
    "WURCS=2.0/1,1,0/[axxxh-1a_1-5]/1/"
  )
  expect_identical(
    as.character(parse_wurcs(specified)),
    c("Pen(b1-", "Pen(a1-")
  )
  unspecified <- sub("-1[ab]_", "-1x_", specified)
  expect_identical(as.character(parse_wurcs(unspecified)), rep("Pen(?1-", 2))
})


test_that("supported methyl groups do not authorize dropping an aglycone", {
  expect_identical(
    as.character(parse_smiles(paste0(smiles_glucose, "C"))),
    "Glc6Me(b1-"
  )
  glycoside <- sub("(O1)O", "(O1)OC", smiles_glucose, fixed = TRUE)
  expect_true(is.na(parse_smiles(glycoside, on_failure = "na")))
})

test_that("non-root alditols fail rather than copying a lossy WURCS result", {
  d <- read.csv(test_path("fixtures", "glygen-smiles-wurcs.csv"))
  d <- d[d$glytoucan_ac == "G27289CV", ]
  expect_true(is.na(parse_smiles(
    d$sequence_smiles_isomeric,
    on_failure = "na"
  )))
  expect_warning(parse_wurcs(d$sequence_wurcs), "Only the main reducing-end")
})
