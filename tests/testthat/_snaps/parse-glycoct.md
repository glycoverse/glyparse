# floating substituent positions preserve representable domains

    Code
      normalize_floating_substituent_parents(c(1L, 2L), c(1L, 2L), "3/6S", c("1\r3",
        "2\r6"), context = "test substituent")
    Condition
      Error:
      ! Can't represent the feasible parent-position combinations for a test substituent after excluding occupied carbon positions.

# GlycoCT UND donor alternatives fail explicitly

    Code
      parse_glycoct_und_block(und)
    Condition
      Error in `parse_glycoct_und_block()`:
      ! GlycoCT UND linkages with alternative donor positions are not supported: "o(3+1|2)d"

# GlycoCT UND parts require a feasible candidate parent

    Code
      filter_glycoct_und_parents(1L, "b1-3", "1\r3")
    Condition
      Error in `filter_glycoct_und_parents()`:
      ! No feasible parent remains for a GlycoCT UND part after excluding occupied acceptor positions.

