# IUPAC-condensed: some O-glycan

    Code
      print(glycan, verbose = TRUE)
    Output
      <glycan_structure[1]>
      [1] Neu5Ac(a2-3)Gal(b1-4)[Fuc(a1-3)]GlcNAc(b1-6)[Neu5Ac(a2-3)Gal(b1-3)]GalNAc(b1-
      # Unique structures: 1

# IUPAC-condensed can drop generic glycans

    Code
      parse_iupac_condensed(input)
    Condition
      Error in `validate_glycan_graph_vector()`:
      ! All structures must have the same monosaccharide type.
      x Found 1 concrete and 2 generic structure(s) in the same vector.
      i Use `convert_to_generic()` to convert concrete structures to generic type.

---

    Code
      result <- parse_iupac_condensed(input, drop_generic = TRUE)
    Message
      Dropped 2 generic glycans (replaced with `NA`).

# drop_generic drops generic-only parser output

    Code
      result <- parse_pglyco_struc(c("(N)", "(H)"), drop_generic = TRUE)
    Message
      Dropped 2 generic glycans (replaced with `NA`).

