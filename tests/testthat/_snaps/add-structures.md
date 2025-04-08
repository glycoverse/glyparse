# add_structures works

    Code
      exp$glycan_structures
    Output
      $`(N(N(H(H(H))(H(H)))))`
      Glycan Graph (NE)
      H: 5, N: 2
      ------------------
      N
      └─N
        └─H
          ├─H
          │ └─H
          └─H
            └─H
      
      $`(N(N(H(H(H)))))`
      Glycan Graph (NE)
      H: 3, N: 2
      ------------------
      N
      └─N
        └─H
          └─H
            └─H
      

# override works

    Code
      exp$glycan_structures
    Output
      $`(N(N(H(H(H))(H(H)))))`
      Glycan Graph (NE)
      H: 5, N: 2
      ------------------
      N
      └─N
        └─H
          ├─H
          │ └─H
          └─H
            └─H
      
      $`(N(N(H(H(H)))))`
      Glycan Graph (NE)
      H: 3, N: 2
      ------------------
      N
      └─N
        └─H
          └─H
            └─H
      

# silence set to TRUE does not raise error

    Code
      exp$glycan_structures
    Output
      $H5N2
      Glycan Graph (NE)
      GlcNAc: 2, Man: 3
      ------------------
      GlcNAc (?1-)
      └─GlcNAc (b1-4)
        └─Man (b1-4)
          ├─Man (a1-3)
          └─Man (a1-6)
      
      $H4N2
      Glycan Graph (NE)
      GlcNAc: 2, Man: 3
      ------------------
      GlcNAc (?1-)
      └─GlcNAc (b1-4)
        └─Man (b1-4)
          ├─Man (a1-3)
          └─Man (a1-6)
      

# no structure column and aggregated to 'gf' raises error

    Code
      add_structures(exp)
    Condition
      Error in `add_structures()`:
      ! Structure column not found in the experiment.
      i The experiment has been aggregated to "gf" level, so structure information is no longer available. Please re-run `glyclean::aggregate()` or `glyclean::auto_clean()` with `to_level` = "gfs" or "gps". See `glyclean::aggregate()` for details.

