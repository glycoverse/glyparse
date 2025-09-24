test_that("Linear Code: Ma3(Ma6)Mb4GNb4GNb", {
  result <- as.character(parse_linear_code("Ma3(Ma6)Mb4GNb4GNb"))
  expected <- "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-"
  expect_equal(result, expected)
})

test_that("Linear Code: GNb2Ma3(Ab4GNb2Ma6)Mb4GNb4(Fa6)GNa", {
  result <- as.character(parse_linear_code("GNb2Ma3(Ab4GNb2Ma6)Mb4GNb4(Fa6)GNa"))
  # The main branch is switched here to Gal-GlcNAc-Man (the longest)
  expected <- "Gal(b1-4)GlcNAc(b1-2)Man(a1-6)[GlcNAc(b1-2)Man(a1-3)]Man(b1-4)GlcNAc(b1-4)[Fuc(a1-6)]GlcNAc(a1-"
  expect_equal(result, expected)
})

test_that("Linear Code: GNb2Ma3(NNa3Ab3GNb2Ma6)Mb4GNb", {
  result <- as.character(parse_linear_code("GNb2Ma3(NNa3Ab3GNb2Ma6)Mb4GNb"))
  expected <- "Neu5Ac(a2-3)Gal(b1-3)GlcNAc(b1-2)Man(a1-6)[GlcNAc(b1-2)Man(a1-3)]Man(b1-4)GlcNAc(b1-"
  expect_equal(result, expected)
})

test_that("Linear Code mono names are parsed correctly", {
  expect_equal(as.character(parse_linear_code("Ga")), "Glc(a1-")
  expect_equal(as.character(parse_linear_code("Aa")), "Gal(a1-")
  expect_equal(as.character(parse_linear_code("GNa")), "GlcNAc(a1-")
  expect_equal(as.character(parse_linear_code("ANa")), "GalNAc(a1-")
  expect_equal(as.character(parse_linear_code("Ma")), "Man(a1-")
  expect_equal(as.character(parse_linear_code("NNa")), "Neu5Ac(a2-")
  expect_equal(as.character(parse_linear_code("Na")), "Neu(a2-")
  expect_equal(as.character(parse_linear_code("Ka")), "Kdn(a2-")
  expect_equal(as.character(parse_linear_code("Wa")), "Kdo(a2-")
  expect_equal(as.character(parse_linear_code("La")), "GalA(a1-")
  expect_equal(as.character(parse_linear_code("Ia")), "Ido(a1-")
  expect_equal(as.character(parse_linear_code("Ha")), "Rha(a1-")
  expect_equal(as.character(parse_linear_code("Fa")), "Fuc(a1-")
  expect_equal(as.character(parse_linear_code("Xa")), "Xyl(a1-")
  expect_equal(as.character(parse_linear_code("Ba")), "Rib(a1-")
  expect_equal(as.character(parse_linear_code("Ra")), "Ara(a1-")
  expect_equal(as.character(parse_linear_code("Ua")), "GlcA(a1-")
  expect_equal(as.character(parse_linear_code("Oa")), "All(a1-")
  expect_equal(as.character(parse_linear_code("Pa")), "Api(a1-")
  expect_equal(as.character(parse_linear_code("Ea")), "Fru(a2-")
})

test_that("Linear Code: unknown linkages", {
  expect_equal(as.character(parse_linear_code("Ab?Gb")), "Gal(b1-?)Glc(b1-")
  expect_equal(as.character(parse_linear_code("A?3Gb")), "Gal(?1-3)Glc(b1-")
  expect_equal(as.character(parse_linear_code("Ab3/4Gb")), "Gal(b1-3/4)Glc(b1-")
  expect_equal(as.character(parse_linear_code("Ab3G?")), "Gal(b1-3)Glc(?1-")
})

test_that("Linear Code: substituents are parsed correctly", {
  result <- as.character(parse_linear_code("A[2P]b3Gb"))
  expected <- "Gal2P(b1-3)Glc(b1-"
  expect_equal(result, expected)
})

test_that("Linear Code: substituents with known positions are parsed correctly", {
  result <- as.character(parse_linear_code("A[?P]b3Gb"))
  expected <- "Gal?P(b1-3)Glc(b1-"
  expect_equal(result, expected)
})