# IUPAC-condensed: some O-glycan

    Code
      print(glycan, verbose = TRUE)
    Output
      Glycan Graph (NE)
      Fuc: 1, Gal: 2, GalNAc: 1, GlcNAc: 1, Neu5Ac: 2
      ------------------
      GalNAc
      ├─Gal (b1-3)
      │ └─Neu5Ac (a2-3)
      └─GlcNAc (b1-6)
        ├─Fuc (a1-3)
        └─Gal (b1-4)
          └─Neu5Ac (a2-3)

# IUPAC-condensed: H9N2

    Code
      print(glycan, verbose = TRUE)
    Output
      Glycan Graph (NE)
      GlcNAc: 2, Man: 9
      ------------------
      GlcNAc
      └─GlcNAc (b1-4)
        └─Man (b1-4)
          ├─Man (a1-6)
          │ ├─Man (a1-6)
          │ │ └─Man (a1-2)
          │ └─Man (a1-3)
          │   └─Man (a1-2)
          └─Man (a1-3)
            └─Man (a1-2)
              └─Man (a1-2)

# IUPAC-condensed: unknown linkages

    Code
      print(glycan, verbose = TRUE)
    Output
      Glycan Graph (NE)
      Man: 4
      ------------------
      Man
      └─Man (??-?)
        └─Man (?1-3)
          └─Man (a1-?)

# IUPAC-condensed: single monosaccharide

    Code
      print(glycan, verbose = TRUE)
    Output
      Glycan Graph (NE)
      Man: 1
      ------------------
      Man

# IUPAC-condensed: substituent

    Code
      print(glycan, verbose = TRUE)
    Output
      Glycan Graph (NE)
      Gal: 1, Man: 1
      ------------------
      Gal-6Ac
      └─Man-3S (a1-2)

# IUPAC-condensed: Neu5Ac

    Code
      print(glycan, verbose = TRUE)
    Output
      Glycan Graph (NE)
      Neu5Ac: 1
      ------------------
      Neu5Ac

# IUPAC-condensed: Neu5Ac9Ac

    Code
      print(glycan, verbose = TRUE)
    Output
      Glycan Graph (NE)
      Neu5Ac: 1
      ------------------
      Neu5Ac

