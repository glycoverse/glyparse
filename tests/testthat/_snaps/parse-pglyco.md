# (N(F)(N(H(H(N))(H(N(H))))))

    Code
      print(glycan, verbose = TRUE)
    Output
      Glycan Graph
      F: 1, H: 4, N: 4
      ------------------
      N
      ├─F
      └─N
        └─H
          ├─H
          │ └─N
          └─H
            └─N
              └─H

# (N(F)(N(H(H(N(H)))(H(N(H(A)))))))

    Code
      print(glycan, verbose = TRUE)
    Output
      Glycan Graph
      A: 1, F: 1, H: 5, N: 4
      ------------------
      N
      ├─F
      └─N
        └─H
          ├─H
          │ └─N
          │   └─H
          └─H
            └─N
              └─H
                └─A

# single monosaccharide

    Code
      print(glycan, verbose = TRUE)
    Output
      Glycan Graph
      N: 1
      ------------------
      N

