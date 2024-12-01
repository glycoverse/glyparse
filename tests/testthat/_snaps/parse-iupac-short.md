# parse_iupac_short works for a complex example

    Code
      parse_iupac_short(x)
    Output
      Glycan Graph (NE)
      Fuc: 1, Gal: 2, GlcNAc: 4, Man: 3, Neu5Ac: 2
      ------------------
      GlcNAc (b1-)
      ├─Fuc (a1-6)
      └─GlcNAc (b1-4)
        └─Man (b1-4)
          ├─Man (a1-6)
          │ └─GlcNAc (b1-2)
          │   └─Gal (b1-4)
          │     └─Neu5Ac (a2-3)
          └─Man (a1-3)
            └─GlcNAc (b1-2)
              └─Gal (b1-4)
                └─Neu5Ac (a2-3)

