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

test_that("GlycoCT: Fuc(a1-2)[Gal(a1-3)]Gal(b1-3)[GlcNAc6S(b1-6)]GalNAc(a1- (Sulfate)", {
  glycoct <- paste0(
    "RES\n",
    "1b:a-dgal-HEX-1:5\n",
    "2s:n-acetyl\n",
    "3b:b-dgal-HEX-1:5\n",
    "4b:a-lgal-HEX-1:5|6:d\n",
    "5b:a-dgal-HEX-1:5\n",
    "6b:b-dglc-HEX-1:5\n",
    "7s:n-acetyl\n",
    "8s:sulfate\n",
    "LIN\n",
    "1:1d(2+1)2n\n",
    "2:1o(3+1)3d\n",
    "3:3o(2+1)4d\n",
    "4:3o(3+1)5d\n",
    "5:1o(6+1)6d\n",
    "6:6d(2+1)7n\n",
    "7:6o(6+1)8n"
  )
  result <- as.character(parse_glycoct(glycoct))
  expected <- "Fuc(a1-2)[Gal(a1-3)]Gal(b1-3)[GlcNAc6S(b1-6)]GalNAc(a1-"
  expect_equal(result, expected)
})

test_that("GlycoCT: GlcA3S(b1-3)Gal(b1-4)GlcNAc(b1- (GlcA)", {
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
