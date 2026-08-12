# struc_parser_wrapper drops generic glycans by input position

    Code
      struc_parser_wrapper(input, do_parse_iupac_condensed)
    Condition
      Error in `glyrepr::validate_glycan_graph_vector()`:
      ! All structures must have the same monosaccharide type.
      x Found 1 concrete and 1 generic structure(s) in the same vector.
      i Use `convert_to_generic()` to convert concrete structures to generic type.

---

    Code
      result <- struc_parser_wrapper(input, do_parse_iupac_condensed, drop_generic = TRUE)
    Message
      Dropped 2 generic glycans (replaced with `NA`).
