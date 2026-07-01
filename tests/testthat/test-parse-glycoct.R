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

test_that("GlycoCT accepts space-separated records", {
  glycoct <- paste(
    "RES",
    "1b:a-dgal-HEX-1:5",
    "2s:n-acetyl",
    "3b:b-dgal-HEX-1:5",
    "LIN",
    "1:1d(2+1)2n",
    "2:1o(3+1)3d"
  )

  result <- as.character(parse_glycoct(glycoct))

  expect_equal(result, "Gal(b1-3)GalNAc(a1-")
})

test_that("GlycoCT maps generic HEX descriptors", {
  expect_equal(as.character(parse_glycoct("RES\n1b:x-HEX-x:x")), "Hex(??-")
  expect_equal(as.character(parse_glycoct("RES\n1b:x-HEX-1:x|6:d")), "dHex(?1-")

  hexnac <- paste0(
    "RES\n",
    "1b:x-HEX-1:x\n",
    "2s:n-acetyl\n",
    "LIN\n",
    "1:1d(2+1)2n"
  )
  hexnac_6s <- paste0(
    "RES\n",
    "1b:x-HEX-1:x\n",
    "2s:n-acetyl\n",
    "3s:sulfate\n",
    "LIN\n",
    "1:1d(2+1)2n\n",
    "2:1o(6+1)3n"
  )
  expect_equal(as.character(parse_glycoct(hexnac)), "HexNAc(?1-")
  expect_equal(as.character(parse_glycoct(hexnac_6s)), "HexNAc6S(?1-")
})

test_that("GlycoCT maps generic Neu5Ac descriptors", {
  neu5ac <- paste0(
    "RES\n",
    "1b:x-NON-2:6|1:a|2:keto|3:d\n",
    "2s:n-acetyl\n",
    "LIN\n",
    "1:1d(5+1)2n"
  )
  neu5gc <- paste0(
    "RES\n",
    "1b:x-NON-2:6|1:a|2:keto|3:d\n",
    "2s:n-glycolyl\n",
    "LIN\n",
    "1:1d(5+1)2n"
  )
  neu5ac_9ac <- paste0(
    "RES\n",
    "1b:x-NON-2:6|1:a|2:keto|3:d\n",
    "2s:n-acetyl\n",
    "3s:acetyl\n",
    "LIN\n",
    "1:1d(5+1)2n\n",
    "2:1o(9+1)3n"
  )
  neu5gc_9s <- paste0(
    "RES\n",
    "1b:x-NON-2:6|1:a|2:keto|3:d\n",
    "2s:n-glycolyl\n",
    "3s:sulfate\n",
    "LIN\n",
    "1:1d(5+1)2n\n",
    "2:1o(9+1)3n"
  )

  expect_equal(as.character(parse_glycoct(neu5ac)), "Neu5Ac(?2-")
  expect_equal(as.character(parse_glycoct(neu5gc)), "Neu5Gc(?2-")
  expect_equal(as.character(parse_glycoct(neu5ac_9ac)), "Neu5Ac9Ac(?2-")
  expect_equal(as.character(parse_glycoct(neu5gc_9s)), "Neu5Gc9S(?2-")
})

test_that("GlycoCT maps direct n-sulfate substituents", {
  glcns <- paste0(
    "RES\n",
    "1b:x-dglc-HEX-1:5\n",
    "2s:n-sulfate\n",
    "LIN\n",
    "1:1d(2+1)2n"
  )
  hexns <- paste0(
    "RES\n",
    "1b:x-HEX-1:x\n",
    "2s:n-sulfate\n",
    "LIN\n",
    "1:1d(2+1)2n"
  )

  expect_equal(as.character(parse_glycoct(glcns)), "GlcN2S(?1-")
  expect_equal(as.character(parse_glycoct(hexns)), "HexN2S(?1-")
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

test_that("GlycoCT handles unknown reducing-end ring bounds", {
  glycoct <- "RES\n1b:x-dglc-HEX-x:x"

  result <- parse_glycoct(glycoct)
  graph <- glyrepr::get_structure_graphs(result)

  expect_equal(as.character(result), "Glc(??-")
  expect_equal(graph$anomer, "??")
})

test_that("GlycoCT matches amino sugars with unknown ring bounds", {
  glycoct <- paste0(
    "RES\n",
    "1b:x-dglc-HEX-x:x\n",
    "2s:amino\n",
    "LIN\n",
    "1:1d(2+1)2n"
  )

  expect_equal(as.character(parse_glycoct(glycoct)), "GlcN(??-")
})

test_that("GlycoCT handles N-sulfated amino sugars with unknown ring bounds", {
  glycoct <- paste0(
    "RES\n",
    "1b:x-dglc-HEX-x:x\n",
    "2s:amino\n",
    "3s:sulfate\n",
    "LIN\n",
    "1:1d(2+1)2n\n",
    "2:1d(2+1)3n"
  )

  result <- parse_glycoct(glycoct)
  graph <- glyrepr::get_structure_graphs(result)

  expect_equal(as.character(result), "GlcN2S(??-")
  expect_equal(igraph::vertex_attr(graph, "mono"), "GlcN")
  expect_equal(igraph::vertex_attr(graph, "sub"), "2S")
})

test_that("GlycoCT warns and uses unknown reducing-end anomers for alditols", {
  glcnac_alditol <- paste0(
    "RES\n",
    "1b:o-dglc-HEX-0:0|1:aldi\n",
    "2s:n-acetyl\n",
    "LIN\n",
    "1:1d(2+1)2n"
  )
  expect_warning(
    glcnac_structure <- parse_glycoct(glcnac_alditol),
    "regular reducing-end glycans with unknown anomer configurations"
  )
  expect_equal(as.character(glcnac_structure), "GlcNAc(?1-")

  linked_alditol <- paste0(
    "RES\n",
    "1b:o-dglc-HEX-0:0|1:aldi\n",
    "2s:n-acetyl\n",
    "3b:b-dgal-HEX-1:5\n",
    "LIN\n",
    "1:1d(2+1)2n\n",
    "2:1o(4+1)3d"
  )
  expect_warning(
    linked_structure <- parse_glycoct(linked_alditol),
    "regular reducing-end glycans with unknown anomer configurations"
  )
  expect_equal(as.character(linked_structure), "Gal(b1-4)GlcNAc(?1-")
})

test_that("GlycoCT parses all distinct converter alditol descriptors", {
  alditol_mapping <- function(res, lin = NULL) {
    list(res = res, lin = lin)
  }
  create_glycoct_from_mapping <- function(mapping) {
    res_section <- paste(mapping$res, collapse = "\n")
    if (!is.null(mapping$lin) && length(mapping$lin) > 0) {
      lin_section <- paste(mapping$lin, collapse = "\n")
      paste0("RES\n", res_section, "\nLIN\n", lin_section)
    } else {
      paste0("RES\n", res_section)
    }
  }

  alditol_mappings <- list(
    Glc = alditol_mapping(c("1b:o-dglc-HEX-0:0|1:aldi"), NULL),
    Man = alditol_mapping(c("1b:o-dman-HEX-0:0|1:aldi"), NULL),
    Gal = alditol_mapping(c("1b:o-dgal-HEX-0:0|1:aldi"), NULL),
    Gul = alditol_mapping(c("1b:o-dgul-HEX-0:0|1:aldi"), NULL),
    Alt = alditol_mapping(c("1b:o-ltal-HEX-0:0|1:aldi"), NULL),
    All = alditol_mapping(c("1b:o-dall-HEX-0:0|1:aldi"), NULL),
    Tal = alditol_mapping(c("1b:o-dalt-HEX-0:0|1:aldi"), NULL),
    Ido = alditol_mapping(c("1b:o-lido-HEX-0:0|1:aldi"), NULL),
    GlcNAc = alditol_mapping(
      c("1b:o-dglc-HEX-0:0|1:aldi", "2s:n-acetyl"),
      c("1:1d(2+1)2n")
    ),
    GalNAc = alditol_mapping(
      c("1b:o-dgal-HEX-0:0|1:aldi", "2s:n-acetyl"),
      c("1:1d(2+1)2n")
    ),
    ManNAc = alditol_mapping(
      c("1b:o-dman-HEX-0:0|1:aldi", "2s:n-acetyl"),
      c("1:1d(2+1)2n")
    ),
    GulNAc = alditol_mapping(
      c("1b:o-dgul-HEX-0:0|1:aldi", "2s:n-acetyl"),
      c("1:1d(2+1)2n")
    ),
    AltNAc = alditol_mapping(
      c("1b:o-ltal-HEX-0:0|1:aldi", "2s:n-acetyl"),
      c("1:1d(5+1)2n")
    ),
    AllNAc = alditol_mapping(
      c("1b:o-dall-HEX-0:0|1:aldi", "2s:n-acetyl"),
      c("1:1d(2+1)2n")
    ),
    TalNAc = alditol_mapping(
      c("1b:o-dalt-HEX-0:0|1:aldi", "2s:n-acetyl"),
      c("1:1d(5+1)2n")
    ),
    IdoNAc = alditol_mapping(
      c("1b:o-lido-HEX-0:0|1:aldi", "2s:n-acetyl"),
      c("1:1d(2+1)2n")
    ),
    GlcN = alditol_mapping(
      c("1b:o-dglc-HEX-0:0|1:aldi", "2s:amino"),
      c("1:1d(2+1)2n")
    ),
    ManN = alditol_mapping(
      c("1b:o-dman-HEX-0:0|1:aldi", "2s:amino"),
      c("1:1d(2+1)2n")
    ),
    GalN = alditol_mapping(
      c("1b:o-dgal-HEX-0:0|1:aldi", "2s:amino"),
      c("1:1d(2+1)2n")
    ),
    GulN = alditol_mapping(
      c("1b:o-dgul-HEX-0:0|1:aldi", "2s:amino"),
      c("1:1d(2+1)2n")
    ),
    AltN = alditol_mapping(
      c("1b:o-ltal-HEX-0:0|1:aldi", "2s:amino"),
      c("1:1d(5+1)2n")
    ),
    AllN = alditol_mapping(
      c("1b:o-dall-HEX-0:0|1:aldi", "2s:amino"),
      c("1:1d(2+1)2n")
    ),
    TalN = alditol_mapping(
      c("1b:o-dalt-HEX-0:0|1:aldi", "2s:amino"),
      c("1:1d(5+1)2n")
    ),
    IdoN = alditol_mapping(
      c("1b:o-lido-HEX-0:0|1:aldi", "2s:amino"),
      c("1:1d(2+1)2n")
    ),
    Fuc = alditol_mapping(c("1b:o-lgal-HEX-0:0|1:aldi|6:d"), NULL),
    Qui = alditol_mapping(c("1b:o-dglc-HEX-0:0|1:aldi|6:d"), NULL),
    Rha = alditol_mapping(c("1b:o-lman-HEX-0:0|1:aldi|6:d"), NULL),
    `6dGul` = alditol_mapping(c("1b:o-dgul-HEX-0:0|1:aldi|6:d"), NULL),
    `6dAlt` = alditol_mapping(c("1b:o-lalt-HEX-0:0|1:aldi|6:d"), NULL),
    `6dTal` = alditol_mapping(c("1b:o-dtal-HEX-0:0|1:aldi|6:d"), NULL),
    FucNAc = alditol_mapping(
      c("1b:o-lgal-HEX-0:0|1:aldi|6:d", "2s:n-acetyl"),
      c("1:1d(2+1)2n")
    ),
    QuiNAc = alditol_mapping(
      c("1b:o-dglc-HEX-0:0|1:aldi|6:d", "2s:n-acetyl"),
      c("1:1d(2+1)2n")
    ),
    RhaNAc = alditol_mapping(
      c("1b:o-lman-HEX-0:0|1:aldi|6:d", "2s:n-acetyl"),
      c("1:1d(2+1)2n")
    ),
    `6dAltNAc` = alditol_mapping(
      c("1b:o-lalt-HEX-0:0|1:aldi|6:d", "2s:n-acetyl"),
      c("1:1d(2+1)2n")
    ),
    `6dTalNAc` = alditol_mapping(
      c("1b:o-dtal-HEX-0:0|1:aldi|6:d", "2s:n-acetyl"),
      c("1:1d(2+1)2n")
    ),
    Oli = alditol_mapping(c("1b:o-dara-HEX-0:0|1:aldi|2:d|6:d"), NULL),
    Tyv = alditol_mapping(c("1b:o-dara-HEX-0:0|1:aldi|3:d|6:d"), NULL),
    Abe = alditol_mapping(c("1b:o-dxyl-HEX-0:0|1:aldi|3:d|6:d"), NULL),
    Par = alditol_mapping(c("1b:o-drib-HEX-0:0|1:aldi|3:d|6:d"), NULL),
    Dig = alditol_mapping(c("1b:o-drib-HEX-0:0|1:aldi|2:d|6:d"), NULL),
    Col = alditol_mapping(c("1b:o-lxyl-HEX-0:0|1:aldi|3:d|6:d"), NULL),
    Lyx = alditol_mapping(c("1b:o-llyx-PEN-0:0|1:aldi"), NULL),
    Xyl = alditol_mapping(c("1b:o-dxyl-PEN-0:0|1:aldi"), NULL),
    Rib = alditol_mapping(c("1b:o-drib-PEN-0:0|1:aldi"), NULL),
    Bac = alditol_mapping(
      c("1b:o-dglc-HEX-0:0|1:aldi|6:d", "2s:amino", "3s:amino"),
      c("1:1d(2+1)2n", "2:1d(4+1)3n")
    ),
    LDmanHep = alditol_mapping(c("1b:o-dgro-dgal-HEP-0:0|1:aldi"), NULL),
    DDmanHep = alditol_mapping(c("1b:o-dgro-dman-HEP-0:0|1:aldi"), NULL),
    MurNAc = alditol_mapping(
      c(
        "1b:o-dglc-HEX-0:0|1:aldi",
        "2s:n-acetyl",
        "3s:(r)-carboxyethyl"
      ),
      c("1:1d(2+1)2n", "2:1o(3+1)3n")
    ),
    MurNGc = alditol_mapping(
      c(
        "1b:o-dglc-HEX-0:0|1:aldi",
        "2s:n-glycolyl",
        "3s:(r)-carboxyethyl"
      ),
      c("1:1d(2+1)2n", "2:1o(3+1)3n")
    ),
    Mur = alditol_mapping(
      c("1b:o-dglc-HEX-0:0|1:aldi", "2s:(r)-carboxyethyl"),
      c("1:1o(3+1)2n")
    )
  )

  purrr::iwalk(alditol_mappings, function(mapping, mono_name) {
    glycoct <- create_glycoct_from_mapping(mapping)
    expect_warning(
      result <- parse_glycoct(glycoct),
      "regular reducing-end glycans with unknown anomer configurations"
    )
    graph <- glyrepr::get_structure_graphs(result, return_list = FALSE)

    expect_equal(igraph::vertex_attr(graph, "mono"), mono_name)
    expect_equal(graph$anomer, paste0("?", glyrepr::get_anomer_pos(mono_name)))
    expect_false(isTRUE(graph$alditol))
  })

  # GlycanFormatConverter emits the same GlycoCT alditol descriptor for Ara-ol
  # and Lyx-ol, so the distinct descriptor is tested once as Lyx.
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
