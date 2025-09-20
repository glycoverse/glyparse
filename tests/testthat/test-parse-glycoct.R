test_that("GlycoCT: Gal(b1-3)GalNAc(a1-", {
  glycoct <- paste0(
    "RES\n",
    "1b:a-dgal-HEX-1:5\n",
    "2s:n-acetyl\n",
    "3b:b-dgal-HEX-1:5\n",
    "LIN\n",
    "1:1d(2+1)2n\n",
    "2:1o(3+1)3d"
  )
  result <- as.character(parse_glycoct(glycoct))
  expected <- "Gal(b1-3)GalNAc(a1-"
  expect_equal(result, expected)
})

test_that("GlycoCT: Neu5Ac(a2-3)Gal(b1-3)[Neu5Ac(a2-6)]GalNAc(a1-", {
  glycoct <- paste0(
    "RES\n",
    "1b:a-dgal-HEX-1:5\n",
    "2s:n-acetyl\n",
    "3b:b-dgal-HEX-1:5\n",
    "4b:a-dgro-dgal-NON-2:6|1:a|2:keto|3:d\n",
    "5s:n-acetyl\n",
    "6b:a-dgro-dgal-NON-2:6|1:a|2:keto|3:d\n",
    "7s:n-acetyl\n",
    "LIN\n",
    "1:1d(2+1)2n\n",
    "2:1o(3+1)3d\n",
    "3:3o(3+2)4d\n",
    "4:4d(5+1)5n\n",
    "5:1o(6+2)6d\n",
    "6:6d(5+1)7n"
  )
  result <- as.character(parse_glycoct(glycoct))
  expected <- "Neu5Ac(a2-3)Gal(b1-3)[Neu5Ac(a2-6)]GalNAc(a1-"
  expect_equal(result, expected)
})

test_that("GlycoCT: Man6", {
  glycoct <- paste0(
    "RES\n",
    "1b:b-dglc-HEX-1:5\n",
    "2s:n-acetyl\n",
    "3b:b-dglc-HEX-1:5\n",
    "4s:n-acetyl\n",
    "5b:b-dman-HEX-1:5\n",
    "6b:a-dman-HEX-1:5\n",
    "7b:a-dman-HEX-1:5\n",
    "8b:a-dman-HEX-1:5\n",
    "9b:a-dman-HEX-1:5\n",
    "10b:a-dman-HEX-1:5\n",
    "LIN\n",
    "1:1d(2+1)2n\n",
    "2:1o(4+1)3d\n",
    "3:3d(2+1)4n\n",
    "4:3o(4+1)5d\n",
    "5:5o(3+1)6d\n",
    "6:6o(2+1)7d\n",
    "7:5o(6+1)8d\n",
    "8:8o(3+1)9d\n",
    "9:8o(6+1)10d"
  )
  result <- as.character(parse_glycoct(glycoct))
  expected <- "Man(a1-2)Man(a1-3)[Man(a1-3)[Man(a1-6)]Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-"
  expect_equal(result, expected)
})

test_that("GlycoCT: GlcA3S(b1-3)Gal(b1-4)GlcNAc(b1- (GlcA and sulfate)", {
  glycoct <- paste0(
    "RES\n",
    "1b:b-dglc-HEX-1:5\n",
    "2s:n-acetyl\n",
    "3b:b-dgal-HEX-1:5\n",
    "4b:b-dglc-HEX-1:5|6:a\n",
    "5s:sulfate\n",
    "LIN\n",
    "1:1d(2+1)2n\n",
    "2:1o(4+1)3d\n",
    "3:3o(3+1)4d\n",
    "4:4o(3+1)5n"
  )
  result <- as.character(parse_glycoct(glycoct))
  expected <- "GlcA3S(b1-3)Gal(b1-4)GlcNAc(b1-"
  expect_equal(result, expected)
})

test_that("GlycoCT: GlcA?S(b1-3)Gal(b1-4)GlcNAc(b1- (Unknown sulfate position)", {
  glycoct <- paste0(
    "RES\n",
    "1b:b-dglc-HEX-1:5\n",
    "2s:n-acetyl\n",
    "3b:b-dgal-HEX-1:5\n",
    "4b:b-dglc-HEX-1:5|6:a\n",
    "5s:sulfate\n",
    "LIN\n",
    "1:1d(2+1)2n\n",
    "2:1o(4+1)3d\n",
    "3:3o(3+1)4d\n",
    "4:4o(-1+1)5n"
  )
  result <- as.character(parse_glycoct(glycoct))
  expected <- "GlcA?S(b1-3)Gal(b1-4)GlcNAc(b1-"
  expect_equal(result, expected)
})

test_that("GlycoCT: Neu4Ac5Ac(a2-", {
  glycoct <- paste0(
    "RES\n",
    "1b:a-dgro-dgal-NON-2:6|1:a|2:keto|3:d\n",
    "2s:acetyl\n",
    "3s:n-acetyl\n",
    "LIN\n",
    "1:1o(4+1)2n\n",
    "2:1d(5+1)3n\n"
  )
  result <- as.character(parse_glycoct(glycoct))
  expected <- "Neu5Ac4Ac(a2-"
  expect_equal(result, expected)
})

test_that("GlycoCT substituents", {
  expect_sub_equal <- function(glycoct_string, expected) {
    result <- parse_glycoct(glycoct_string)
    graph <- glyrepr::get_structure_graphs(result, return_list = FALSE)
    sub <- igraph::V(graph)$sub
    expect_equal(sub, expected)
  }

  sub_N <- "RES\n1b:a-dglc-HEX-1:5\n2s:amino\nLIN\n1:1d(3+1)2n\n"
  expect_sub_equal(sub_N, "3N")
  sub_Me <- "RES\n1b:a-dglc-HEX-1:5\n2s:acetyl\nLIN\n1:1o(3+1)2n\n"
  expect_sub_equal(sub_Me, "3Ac")
  sub_NAc <- "RES\n1b:a-dglc-HEX-1:5\n2s:n-acetyl\nLIN\n1:1d(3+1)2n\n"
  expect_sub_equal(sub_NAc, "3Ac")
  sub_P <- "RES\n1b:a-dglc-HEX-1:5\n2s:phosphate\nLIN\n1:1o(3+1)2n\n"
  expect_sub_equal(sub_P, "3P")
  sub_S <- "RES\n1b:a-dglc-HEX-1:5\n2s:sulfate\nLIN\n1:1o(3+1)2n\n"
  expect_sub_equal(sub_S, "3S")
  sub_PPEtn <- "RES\n1b:a-dglc-HEX-1:5\n2s:diphospho-ethanolamine\nLIN\n1:1o(3+1)2n\n"
  expect_sub_equal(sub_PPEtn, "3PPEtn")
  sub_PEtn <- "RES\n1b:a-dglc-HEX-1:5\n2s:phospho-ethanolamine\nLIN\n1:1o(3+1)2n\n"
  expect_sub_equal(sub_PEtn, "3PEtn")
})

test_that("GlycoCT: GlcA3S(?1-?)Gal(?1-?)GlcNAc(?1-, (Unknown linkages and anomers)", {
  glycoct <- paste0(
    "RES\n",
    "1b:x-dglc-HEX-1:5\n",
    "2s:n-acetyl\n",
    "3b:x-dgal-HEX-1:5\n",
    "4b:x-dglc-HEX-1:5|6:a\n",
    "5s:sulfate\n",
    "LIN\n",
    "1:1d(2+1)2n\n",
    "2:1o(-1+1)3d\n",
    "3:3o(3|6+1)4d\n",
    "4:4o(3+1)5n"
  )
  result <- as.character(parse_glycoct(glycoct))
  expected <- "GlcA3S(?1-3/6)Gal(?1-?)GlcNAc(?1-"
  expect_equal(result, expected)
})

test_that("all monosaccharides can be parsed", {
  expect_mono_equal <- function(x, expected) {
    graph <- glyrepr::get_structure_graphs(x, return_list = FALSE)
    mono <- igraph::V(graph)$mono
    expect_equal(mono, expected)
  }
  
  # Helper function to create GlycoCT from mapping
  create_glycoct_from_mapping <- function(mapping) {
    res_section <- paste(mapping$res, collapse = "\n")
    if (!is.null(mapping$lin) && length(mapping$lin) > 0) {
      lin_section <- paste(mapping$lin, collapse = "\n")
      paste0("RES\n", res_section, "\nLIN\n", lin_section)
    } else {
      paste0("RES\n", res_section)
    }
  }
  
  # Get all mappings
  mappings <- load_mono_mappings()
  
  # Test each monosaccharide
  for (mono_name in names(mappings)) {
    mapping <- mappings[[mono_name]]
    glycoct <- create_glycoct_from_mapping(mapping)
    expect_mono_equal(parse_glycoct(glycoct), mono_name)
  }
})

test_that("GlycoCT: a complex N-glycan example", {
  glycoct <- paste0(
    "RES\n",
    "1b:b-dglc-HEX-1:5\n",
    "2s:n-acetyl\n",
    "3b:b-dglc-HEX-1:5\n",
    "4s:n-acetyl\n",
    "5b:b-dman-HEX-1:5\n",
    "6b:a-dman-HEX-1:5\n",
    "7b:b-dglc-HEX-1:5\n",
    "8s:n-acetyl\n",
    "9b:b-dgal-HEX-1:5\n",
    "10b:a-dgro-dgal-NON-2:6|1:a|2:keto|3:d\n",
    "11s:n-acetyl\n",
    "12b:a-dman-HEX-1:5\n",
    "13b:b-dglc-HEX-1:5\n",
    "14s:n-acetyl\n",
    "15b:b-dgal-HEX-1:5\n",
    "LIN\n",
    "1:1d(2+1)2n\n",
    "2:1o(4+1)3d\n",
    "3:3d(2+1)4n\n",
    "4:3o(4+1)5d\n",
    "5:5o(3+1)6d\n",
    "6:6o(2+1)7d\n",
    "7:7d(2+1)8n\n",
    "8:7o(4+1)9d\n",
    "9:9o(3|6+2)10d\n",
    "10:10d(5+1)11n\n",
    "11:5o(6+1)12d\n",
    "12:12o(2+1)13d\n",
    "13:13d(2+1)14n\n",
    "14:13o(4+1)15d"
  )
  result <- as.character(parse_glycoct(glycoct))
  expected <- "Neu5Ac(a2-3/6)Gal(b1-4)GlcNAc(b1-2)Man(a1-3)[Gal(b1-4)GlcNAc(b1-2)Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-"
  expect_equal(result, expected)
})