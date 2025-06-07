# A2B2C1D1E2F1G3gfF5fedD1E2F1feE2F1fedcba

    Code
      print(glycan, verbose = TRUE)
    Output
      Glycan Graph
      Hex: 6, HexNAc: 5, NeuAc: 1, dHex: 1
      ------------------
      HexNAc
      └─HexNAc
        └─Hex
          ├─Hex
          │ └─HexNAc
          │   ├─Hex
          │   │ └─NeuAc
          │   └─dHex
          └─Hex
            ├─HexNAc
            │ └─Hex
            └─HexNAc
              └─Hex

# A2B2C1D1E1F1fedD1E1F1feE1F1fedcba

    Code
      print(glycan, verbose = TRUE)
    Output
      Glycan Graph
      Hex: 9, HexNAc: 2
      ------------------
      HexNAc
      └─HexNAc
        └─Hex
          ├─Hex
          │ └─Hex
          │   └─Hex
          └─Hex
            ├─Hex
            │ └─Hex
            └─Hex
              └─Hex

