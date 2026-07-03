test_that("KCF parses a glycosphingolipid glycan from GlycanFormatConverter cases", {
  kcf <- paste0(
    "ENTRY       G00066                      Glycan\n",
    "NODE        6\n",
    "            1   Cer        18     0\n",
    "            2   Glc        12     0\n",
    "            3   Gal         6     0\n",
    "            4   GlcNAc     -2     0\n",
    "            5   Gal       -10     0\n",
    "            6   GlcNAc    -18     0\n",
    "EDGE        5\n",
    "            1     2:b1    1:1\n",
    "            2     3:b1    2:4\n",
    "            3     4:b1    3:3\n",
    "            4     5:b1    4:4\n",
    "            5     6:b1    5:3\n",
    "///"
  )

  result <- as.character(parse_kcf(kcf))

  expect_equal(
    result,
    "GlcNAc(b1-3)Gal(b1-4)GlcNAc(b1-3)Gal(b1-4)Glc(b1-"
  )
})

test_that("KCF parses N-glycan branches and aglycon roots", {
  kcf <- paste0(
    "ENTRY       G00204                      Glycan\n",
    "NODE        12\n",
    "            1   Asn        27     3\n",
    "            2   GlcNAc     20     3\n",
    "            3   LFuc       12     8\n",
    "            4   GlcNAc     11    -2\n",
    "            5   Man         3    -2\n",
    "            6   Man        -4     3\n",
    "            7   Man        -4    -7\n",
    "            8   GlcNAc    -12     3\n",
    "            9   GlcNAc    -12    -7\n",
    "            10  Gal       -20     3\n",
    "            11  Gal       -20    -7\n",
    "            12  Neu5Ac    -28     3\n",
    "EDGE        11\n",
    "            1     2:b1    1\n",
    "            2     3:a1    2:6\n",
    "            3     4:b1    2:4\n",
    "            4     5:b1    4:4\n",
    "            5     6:a1    5:6\n",
    "            6     7:a1    5:3\n",
    "            7     8:b1    6:2\n",
    "            8     9:b1    7:2\n",
    "            9    10:b1    8:4\n",
    "            10   11:b1    9:4\n",
    "            11   12:a2   10:3\n",
    "///"
  )

  result <- as.character(parse_kcf(kcf))

  expect_equal(
    result,
    paste0(
      "Neu5Ac(a2-3)Gal(b1-4)GlcNAc(b1-2)Man(a1-6)",
      "[Gal(b1-4)GlcNAc(b1-2)Man(a1-3)]Man(b1-4)",
      "GlcNAc(b1-4)[Fuc(a1-6)]GlcNAc(b1-"
    )
  )
})

test_that("KCF parses sulfate substituent nodes", {
  kcf <- paste0(
    "ENTRY       G00609                      Glycan\n",
    "NODE        10\n",
    "            1   GlcN       20     0\n",
    "            2   S          15     6\n",
    "            3   S          15    -6\n",
    "            4   LIdoA       7     0\n",
    "            5   S           2    -6\n",
    "            6   GlcN       -6     0\n",
    "            7   S          -6    -6\n",
    "            8   S         -12     6\n",
    "            9   S         -12    -6\n",
    "            10  GlcA      -20     0\n",
    "EDGE        9\n",
    "            1     2       1:6\n",
    "            2     3       1:2\n",
    "            3     4:a1    1:4\n",
    "            4     5       4:2\n",
    "            5     6:a1    4:4\n",
    "            6     8       6:6\n",
    "            7     9       6:3\n",
    "            8    10:b1    6:4\n",
    "            9     6:2     7\n",
    "///"
  )

  result <- as.character(parse_kcf(kcf))

  expect_equal(
    result,
    paste0(
      "GlcA(b1-4)GlcN2S3S6S(a1-4)IdoA2S(a1-4)",
      "GlcN2S6S(?1-"
    )
  )
})

test_that("KCF parses node labels with embedded substituents", {
  kcf <- paste0(
    "ENTRY       G00187                      Glycan\n",
    "NODE        9\n",
    "            1   Cer        23     0\n",
    "            2   Glc        17     0\n",
    "            3   Gal        10     0\n",
    "            4   GalNAc      1     5\n",
    "            5   Neu5Ac      1    -5\n",
    "            6   Gal        -7     5\n",
    "            7   Neu5Ac9Ac    -9    -5\n",
    "            8   Neu5Ac    -15     5\n",
    "            9   Neu5Ac    -24     5\n",
    "EDGE        8\n",
    "            1     2:b1    1:1\n",
    "            2     3:b1    2:4\n",
    "            3     4:b1    3:4\n",
    "            4     5:a2    3:3\n",
    "            5     6:b1    4:3\n",
    "            6     7:a2    5:8\n",
    "            7     8:a2    6:3\n",
    "            8     9:a2    8:8\n",
    "///"
  )

  result <- as.character(parse_kcf(kcf))

  expect_equal(
    result,
    paste0(
      "Neu5Ac(a2-8)Neu5Ac(a2-3)Gal(b1-3)GalNAc(b1-4)",
      "[Neu5Ac9Ac(a2-8)Neu5Ac(a2-3)]Gal(b1-4)Glc(b1-"
    )
  )
})

test_that("KCF parses a single monosaccharide", {
  kcf <- paste0(
    "ENTRY       G00000                      Glycan\n",
    "NODE        1\n",
    "            1   Glc         0     0\n",
    "///"
  )

  result <- as.character(parse_kcf(kcf))

  expect_equal(result, "Glc(?1-")
})

test_that("KCF parser preserves wrapper behavior", {
  kcf <- paste0(
    "ENTRY       G00000                      Glycan\n",
    "NODE        1\n",
    "            1   Gal         0     0\n",
    "///"
  )
  input <- c(first = kcf, invalid = "not kcf", missing = NA_character_)

  result <- parse_kcf(input, on_failure = "na")

  expect_equal(names(result), names(input))
  expect_equal(
    as.character(result),
    c(first = "Gal(?1-", invalid = NA, missing = NA)
  )
})

test_that("KCF parser does not silently drop unsupported nodes", {
  kcf <- paste0(
    "ENTRY       G04902                      Glycan\n",
    "NODE        2\n",
    "            1   GalNAc     24     1\n",
    "            2   L4-en-thrHexA   -24     1\n",
    "EDGE        1\n",
    "            1     2:a1    1:3\n",
    "///"
  )

  result <- parse_kcf(kcf, on_failure = "na")

  expect_true(is.na(result))
  expect_error(parse_kcf(kcf), "Can't parse")
})
