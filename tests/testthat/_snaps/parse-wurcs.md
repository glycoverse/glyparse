# glycan with only one monosacharide

    Code
      parse_wurcs(wurcs)
    Output
      Glycan Graph (NE)
      GlcNAc: 1
      ------------------
      GlcNAc (b1-)

# N-glycan core

    Code
      parse_wurcs(wurcs)
    Output
      Glycan Graph (NE)
      GlcNAc: 2, Man: 3
      ------------------
      GlcNAc (?1-)
      └─GlcNAc (b1-4)
        └─Man (b1-4)
          ├─Man (a1-3)
          └─Man (a1-6)

# H5H4F1S2

    Code
      parse_wurcs(wurcs)
    Output
      Glycan Graph (NE)
      Fuc: 1, Gal: 2, GlcNAc: 4, Man: 3, Neu5Ac: 2
      ------------------
      GlcNAc (b1-)
      ├─GlcNAc (b1-4)
      │ └─Man (b1-4)
      │   ├─Man (a1-3)
      │   │ └─GlcNAc (b1-2)
      │   │   └─Gal (b1-4)
      │   │     └─Neu5Ac (a2-3)
      │   └─Man (a1-6)
      │     └─GlcNAc (b1-2)
      │       └─Gal (b1-4)
      │         └─Neu5Ac (a2-3)
      └─Fuc (a1-6)

