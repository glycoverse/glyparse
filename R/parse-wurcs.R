#' Parse WURCS Structures
#'
#' This function parses WURCS strings into a [glyrepr::glycan_structure()].
#' Currently, only WURCS 2.0 is supported.
#' For more information about WURCS, see [WURCS](https://github.com/glycoinfo/WURCS/wiki).
#' Main reducing-end alditol residues retain their alditol status and use an
#' unknown anomer configuration.
#' Ambiguous alternative linkage groups are represented as floating glycan
#' parts when their child residue or subtree is not localized to one parent.
#' Candidate parents may belong to the main glycan or another floating
#' component.
#' Floating substituents preserve their chemistry, carbon-position ambiguity,
#' and candidate parent residues across the complete structure.
#'
#' @param x A character vector of WURCS strings. NA values are allowed and will be returned as NA structures.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param progress Whether to show a progress bar while parsing.
#' @param validate Whether to validate parsed glycan graphs before constructing
#'   the result.
#' @param drop_generic Whether to replace parsed generic glycans with `NA`. A
#'   message reports the number replaced. By default, mixing generic and
#'   concrete glycans raises an error.
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' wurcs <- paste0(
#'   "WURCS=2.0/3,5,4/",
#'   "[a2122h-1b_1-5_2*NCC/3=O][a1122h-1b_1-5][a1122h-1a_1-5]/",
#'   "1-1-2-3-3/a4-b1_b4-c1_c3-d1_c6-e1"
#' )
#' parse_wurcs(wurcs)
#'
#' @export
parse_wurcs <- function(
  x,
  on_failure = "error",
  progress = FALSE,
  validate = TRUE,
  drop_generic = FALSE
) {
  residue_cache <- NULL
  parser <- function(value) {
    if (is.null(residue_cache)) {
      residue_cache <<- build_wurcs_residue_cache(x)
    }
    do_parse_wurcs(value, residue_cache = residue_cache)
  }

  struc_parser_wrapper(
    x,
    parser,
    on_failure = on_failure,
    progress = progress,
    validate = validate,
    drop_generic = drop_generic
  )
}


WURCS_MONO_REGEX <- c(
  "Glc" = "^a2122h-1[abx]_1-5(?!_2\\*N(CC/3=O)?|_3\\*OC\\^RCO/4=O/3C)",
  # Explaination to the regex:
  # - "^": This has to be the beginning of the string.
  # - "a2122h": The WURCS code for Glc.
  # - "-1[abx]": The anomeric carbon is C1,
  #    and the anomer is either "a", "b" or "x" (unknown).
  # - "_1-5": C1 and C5 are connected by a glycosidic bond to form a ring.
  # - "(?!_2\\*N(CC/3=O)?|_3\\*OC\\^RCO/4=O/3C)":
  #   This is for excluding GlcNAc, GlcN, and Mur.
  #   The code for "NAc" is "_2*NCC/3=O", and the code for "N" is "_2*N".
  #   Therefore, "_2\\*N(CC/3=O)?" excludes both.

  # From Man to Ido, the regex is similar to Glc.
  "Man" = "^a1122h-1[abx]_1-5(?!_2\\*N(CC/3=O)?)",
  "Gal" = "^a2112h-1[abx]_1-5(?!_2\\*N(CC/3=O)?)",
  "Gul" = "^a2212h-1[abx]_1-5(?!_2\\*N(CC/3=O)?)",
  "Alt" = "^a2111h-1[abx]_1-5(?!_2\\*N(CC/3=O)?)",
  "All" = "^a2222h-1[abx]_1-5(?!_2\\*N(CC/3=O)?)",
  "Tal" = "^a1112h-1[abx]_1-5(?!_2\\*N(CC/3=O)?)",
  "Ido" = "^a2121h-1[abx]_1-5(?!_2\\*N(CC/3=O)?)",

  # From GlcNAc to IdoNAc, the regex is almost the same to the original WRUCS code.
  # For GlcNAc, we have to differentiate it from MurNAc.
  "GlcNAc" = "^a2122h-1[abx]_1-5_2\\*NCC/3=O(?!_3\\*OC\\^RCO/4=O/3C)",
  "GalNAc" = "^a2112h-1[abx]_1-5_2\\*NCC/3=O",
  # GlycanFormatConverter also recognizes this relative-configuration
  # descriptor as ManNAc.
  "ManNAc" = "^a5122h-1[abx]_1-5_2\\*NCC/3=O",
  "ManNAc" = "^a1122h-1[abx]_1-5_2\\*NCC/3=O",
  "GulNAc" = "^a2212h-1[abx]_1-5_2\\*NCC/3=O",
  "AltNAc" = "^a2111h-1[abx]_1-5_2\\*NCC/3=O",
  "AllNAc" = "^a2222h-1[abx]_1-5_2\\*NCC/3=O",
  "TalNAc" = "^a1112h-1[abx]_1-5_2\\*NCC/3=O",
  "IdoNAc" = "^a2121h-1[abx]_1-5_2\\*NCC/3=O",

  # From GlcN to IdoN, we have to exclude the "Ac" part.
  # For GlcN, we have to differentiate it from NurNAc.
  "GlcN" = "^a2122h-1[abx]_1-5_2\\*N(?!CCO?/3=O(_3\\*OC\\^RCO/4=O/3C)?)",
  "ManN" = "^a1122h-1[abx]_1-5_2\\*N(?!CC/3=O)",
  "GalN" = "^a2112h-1[abx]_1-5_2\\*N(?!CC/3=O)",
  "GulN" = "^a2212h-1[abx]_1-5_2\\*N(?!CC/3=O)",
  "AltN" = "^a2111h-1[abx]_1-5_2\\*N(?!CC/3=O)",
  "AllN" = "^a2222h-1[abx]_1-5_2\\*N(?!CC/3=O)",
  "TalN" = "^a1112h-1[abx]_1-5_2\\*N(?!CC/3=O)",
  "IdoN" = "^a2121h-1[abx]_1-5_2\\*N(?!CC/3=O)",

  # From GlcA to IdoA.
  "GlcA" = "^a2122A-1[abx]_1-5",
  "ManA" = "^a1122A-1[abx]_1-5",
  "GalA" = "^a2112A-1[abx]_1-5",
  "GulA" = "^a2212A-1[abx]_1-5",
  "AltA" = "^a2111A-1[abx]_1-5",
  "AllA" = "^a2222A-1[abx]_1-5",
  "TalA" = "^a1112A-1[abx]_1-5",
  "IdoA" = "^a2121A-1[abx]_1-5",

  # From Fuc to 6dTal, we have to exclude the "NAc" part.
  "Fuc" = "^a1221m-1[abx]_1-5(?!_2\\*NCC/3=O)",
  # For Qui, we have to differentiate it from Bac.
  "Qui" = "^a2122m-1[abx]_1-5(?!_2\\*NCC/3=O|_2\\*N_4\\*N)",
  "Rha" = "^a2211m-1[abx]_1-5(?!_2\\*NCC/3=O)",
  "6dGul" = "^a2212m-1[abx]_1-5(?!_2\\*NCC/3=O)",
  "6dAlt" = "^a2111m-1[abx]_1-5(?!_2\\*NCC/3=O)",
  "6dTal" = "^a1112m-1[abx]_1-5(?!_2\\*NCC/3=O)",

  # From FucNAc to 6dTalNAc.
  "FucNAc" = "^a1221m-1[abx]_1-5_2\\*NCC/3=O",
  "QuiNAc" = "^a2122m-1[abx]_1-5_2\\*NCC/3=O",
  "RhaNAc" = "^a2211m-1[abx]_1-5_2\\*NCC/3=O",
  "6dAltNAc" = "^a2111m-1[abx]_1-5_2\\*NCC/3=O",
  "6dTalNAc" = "^a1112m-1[abx]_1-5_2\\*NCC/3=O",

  # From Oli to Rib
  "Oli" = "^ad122m-1[abx]_1-5",
  "Tyv" = "^a1d22m-1[abx]_1-5",
  "Abe" = "^a2d12m-1[abx]_1-5",
  "Par" = "^a2d22m-1[abx]_1-5",
  "Dig" = "^ad222m-1[abx]_1-5",
  "Col" = "^a1d21m-1[abx]_1-5",
  "Ara" = "^a211h-1[abx]_1-[45]",
  "Lyx" = "^a221h-1[abx]_1-[45]",
  "Xyl" = "^a212h-1[abx]_1-[45]",
  "Rib" = "^a222h-1[abx]_1-[45]",

  # Generic nonulosonic acids with unknown stereochemistry.
  "NeuAc" = "^Aadxxxxxh-2[abx]_2-6(?=.*_5\\*NCC/3=O)",
  "NeuGc" = "^Aadxxxxxh-2[abx]_2-6(?=.*_5\\*NCCO/3=O)",
  "gNeu" = "^Aadxxxxxh-2[abx]_2-6(?=.*_5\\*N(?!CC(O)?/3=O))",
  "gKdn" = "^Aadxxxxxh-2[abx]_2-6(?!.*_5\\*N)",

  # Neu5Ac and Neu5Gc - match if contains _5*NCC/3=O or _5*NCCO/3=O anywhere in the string
  # These must come before Kdn since they are more specific
  "Neu5Ac" = "^Aad21122h-2[abx]_2-6.*_5\\*NCC/3=O",
  "Neu5Gc" = "^Aad21122h-2[abx]_2-6.*_5\\*NCCO/3=O",

  # Kdn: exclude N, Ac, and Gc
  "Kdn" = "^Aad21122h-2[abx]_2-6(?!.*_5\\*N(CC(O)?/3=O)?)",

  # Neu: exclude Ac and Gc
  "Neu" = "^Aad21122h-2[abx]_2-6(?=.*_5\\*N(?!CC(O)?/3=O))",

  # Rest of the monosaccharides are themselves.
  "Pse" = "^had22111m-2[abx]_2-6_5\\*N_7\\*N",
  "Leg" = "^Aad21122m-2[abx]_2-6_5\\*N_7\\*N",
  "Aci" = "^Aad21111m-2[abx]_2-6_5\\*N_7\\*N",
  "4eLeg" = "^Aad11122m-2[abx]_2-6_5\\*N_7\\*N",
  "Bac" = "^a2122m-1[abx]_1-5_2\\*N_4\\*N",
  "LDmanHep" = "^a11221h-1[abx]_1-5",
  "Kdo" = "^Aad1122h-2[abx]_2-6",
  "Dha" = "^Aad112A-2[abx]_2-6",
  "DDmanHep" = "^a11222h-1[abx]_1-5",
  "MurNAc" = "^a2122h-1[abx]_1-5_2\\*NCC/3=O_3\\*OC\\^RCO/4=O/3C",
  "MurNGc" = "^a2122h-1[abx]_1-5_2\\*NCCO/3=O_3\\*OC\\^RCO/4=O/3C",
  "Mur" = "^a2122h-1[abx]_1-5_3\\*OC\\^RCO/4=O/3C",
  "HexNAc" = "^axxxxh-1[abx]_1-5_2\\*NCC/3=O",
  "HexN" = "^axxxxh-1[abx]_1-5_2\\*N(?!CC/3=O)",
  "HexA" = "^axxxxA-1[abx]_1-5",
  "Hex" = "^axxxxh-1[abx]_1-5",
  "dHexNAc" = "^axxxxm-1[abx]_1-5_2\\*NCC/3=O",
  "dHex" = "^axxxxm-1[abx]_1-5",
  "Pen" = "^axxxh-1[abx]_1-5",
  "dHexNAc" = "^a2112m-1[abx]_1-5_2\\*NCC/3=O",
  "dHex" = "^a2112m-1[abx]_1-5",
  "Fru" = "^ha122h-2[abx]_2-6",
  "Tag" = "^ha112h-2[abx]_2-6",
  "Sor" = "^ha121h-2[abx]_2-6",
  "Psi" = "^ha222h-2[abx]_2-6"
)


WURCS_UNKNOWN_RING_MONO_REGEX <- c(
  "Neu5Ac" = "^Aad21122h-2[abx]_2-\\?(?=.*_5\\*NCC/3=O)",
  "Neu5Gc" = "^Aad21122h-2[abx]_2-\\?(?=.*_5\\*NCCO/3=O)",
  "Neu" = "^Aad21122h-2[abx]_2-\\?(?=.*_5\\*N(?!CC(O)?/3=O))",
  "Kdn" = "^Aad21122h-2[abx]_2-\\?(?!.*_5\\*N)",
  "NeuAc" = "^Aadxxxxxh-2[abx]_2-\\?(?=.*_5\\*NCC/3=O)",
  "NeuGc" = "^Aadxxxxxh-2[abx]_2-\\?(?=.*_5\\*NCCO/3=O)",
  "gNeu" = "^Aadxxxxxh-2[abx]_2-\\?(?=.*_5\\*N(?!CC(O)?/3=O))",
  "gKdn" = "^Aadxxxxxh-2[abx]_2-\\?(?!.*_5\\*N)",

  "GlcNAc" = "^a2122h-1[abx]_1-\\?_2\\*NCC/3=O",
  "GalNAc" = "^a2112h-1[abx]_1-\\?_2\\*NCC/3=O",
  "ManNAc" = "^a1122h-1[abx]_1-\\?_2\\*NCC/3=O",
  "GulNAc" = "^a2212h-1[abx]_1-\\?_2\\*NCC/3=O",
  "AltNAc" = "^a2111h-1[abx]_1-\\?_2\\*NCC/3=O",
  "AllNAc" = "^a2222h-1[abx]_1-\\?_2\\*NCC/3=O",
  "TalNAc" = "^a1112h-1[abx]_1-\\?_2\\*NCC/3=O",
  "IdoNAc" = "^a2121h-1[abx]_1-\\?_2\\*NCC/3=O",

  "GlcN" = "^a2122h-1[abx]_1-\\?_2\\*N(?!CC/3=O)",
  "ManN" = "^a1122h-1[abx]_1-\\?_2\\*N(?!CC/3=O)",
  "GalN" = "^a2112h-1[abx]_1-\\?_2\\*N(?!CC/3=O)",
  "GulN" = "^a2212h-1[abx]_1-\\?_2\\*N(?!CC/3=O)",
  "AltN" = "^a2111h-1[abx]_1-\\?_2\\*N(?!CC/3=O)",
  "AllN" = "^a2222h-1[abx]_1-\\?_2\\*N(?!CC/3=O)",
  "TalN" = "^a1112h-1[abx]_1-\\?_2\\*N(?!CC/3=O)",
  "IdoN" = "^a2121h-1[abx]_1-\\?_2\\*N(?!CC/3=O)",

  "GlcA" = "^a2122A-1[abx]_1-\\?",
  "ManA" = "^a1122A-1[abx]_1-\\?",
  "GalA" = "^a2112A-1[abx]_1-\\?",
  "GulA" = "^a2212A-1[abx]_1-\\?",
  "AltA" = "^a2111A-1[abx]_1-\\?",
  "AllA" = "^a2222A-1[abx]_1-\\?",
  "TalA" = "^a1112A-1[abx]_1-\\?",
  "IdoA" = "^a2121A-1[abx]_1-\\?",

  "Glc" = "^a2122h-1[abx]_1-\\?(?!_2\\*N(CC/3=O)?)",
  "Man" = "^a1122h-1[abx]_1-\\?(?!_2\\*N(CC/3=O)?)",
  "Gal" = "^a2112h-1[abx]_1-\\?(?!_2\\*N(CC/3=O)?)",
  "Gul" = "^a2212h-1[abx]_1-\\?(?!_2\\*N(CC/3=O)?)",
  "Alt" = "^a2111h-1[abx]_1-\\?(?!_2\\*N(CC/3=O)?)",
  "All" = "^a2222h-1[abx]_1-\\?(?!_2\\*N(CC/3=O)?)",
  "Tal" = "^a1112h-1[abx]_1-\\?(?!_2\\*N(CC/3=O)?)",
  "Ido" = "^a2121h-1[abx]_1-\\?(?!_2\\*N(CC/3=O)?)",

  "FucNAc" = "^a1221m-1[abx]_1-\\?_2\\*NCC/3=O",
  "QuiNAc" = "^a2122m-1[abx]_1-\\?_2\\*NCC/3=O",
  "RhaNAc" = "^a2211m-1[abx]_1-\\?_2\\*NCC/3=O",
  "6dAltNAc" = "^a2111m-1[abx]_1-\\?_2\\*NCC/3=O",
  "6dTalNAc" = "^a1112m-1[abx]_1-\\?_2\\*NCC/3=O",

  "Fuc" = "^a1221m-1[abx]_1-\\?(?!_2\\*NCC/3=O)",
  "Qui" = "^a2122m-1[abx]_1-\\?(?!_2\\*NCC/3=O|_2\\*N_4\\*N)",
  "Rha" = "^a2211m-1[abx]_1-\\?(?!_2\\*NCC/3=O)",
  "6dGul" = "^a2212m-1[abx]_1-\\?",
  "6dAlt" = "^a2111m-1[abx]_1-\\?(?!_2\\*NCC/3=O)",
  "6dTal" = "^a1112m-1[abx]_1-\\?(?!_2\\*NCC/3=O)",

  "Oli" = "^ad122m-1[abx]_1-\\?",
  "Tyv" = "^a1d22m-1[abx]_1-\\?",
  "Abe" = "^a2d12m-1[abx]_1-\\?",
  "Par" = "^a2d22m-1[abx]_1-\\?",
  "Dig" = "^ad222m-1[abx]_1-\\?",
  "Col" = "^a1d21m-1[abx]_1-\\?",
  "Ara" = "^a211h-1[abx]_1-\\?",
  "Lyx" = "^a221h-1[abx]_1-\\?",
  "Xyl" = "^a212h-1[abx]_1-\\?",
  "Rib" = "^a222h-1[abx]_1-\\?",
  "HexNAc" = "^axxxxh-1[abx]_1-\\?_2\\*NCC/3=O",
  "HexN" = "^axxxxh-1[abx]_1-\\?_2\\*N(?!CC/3=O)",
  "HexA" = "^axxxxA-1[abx]_1-\\?",
  "Hex" = "^axxxxh-1[abx]_1-\\?",
  "dHexNAc" = "^axxxxm-1[abx]_1-\\?_2\\*NCC/3=O",
  "dHex" = "^axxxxm-1[abx]_1-\\?",
  "Pen" = "^axxxh-1[abx]_1-\\?",
  "dHexNAc" = "^a2112m-1[abx]_1-\\?_2\\*NCC/3=O",
  "dHex" = "^a2112m-1[abx]_1-\\?",
  "dHex" = "^a4334m-1[abx]_1-\\?"
)


WURCS_AMBIGUOUS_MONO_REGEX <- c(
  "NeuAc" = "^AUdxxxxxh(?=.*_5\\*NCC/3=O)",
  "NeuGc" = "^AUdxxxxxh(?=.*_5\\*NCCO/3=O)",
  "gNeu" = "^AUdxxxxxh(?=.*_5\\*N(?!CC(O)?/3=O))",
  "gKdn" = "^AUdxxxxxh(?!.*_5\\*N)",

  "Neu5Ac" = "^AUd21122h.*_5\\*NCC/3=O",
  "Neu5Gc" = "^AUd21122h.*_5\\*NCCO/3=O",
  "Neu" = "^AUd21122h.*_5\\*N",
  "Kdn" = "^AUd21122h",

  "GlcNAc" = "^u2122h_2\\*NCC/3=O",
  "GalNAc" = "^u2112h_2\\*NCC/3=O",
  "ManNAc" = "^u1122h_2\\*NCC/3=O",
  "GulNAc" = "^u2212h_2\\*NCC/3=O",
  "AltNAc" = "^u2111h_2\\*NCC/3=O",
  "AllNAc" = "^u2222h_2\\*NCC/3=O",
  "TalNAc" = "^u1112h_2\\*NCC/3=O",
  "IdoNAc" = "^u2121h_2\\*NCC/3=O",

  "GlcN" = "^u2122h_2\\*N(?!CC/3=O)",
  "ManN" = "^u1122h_2\\*N(?!CC/3=O)",
  "GalN" = "^u2112h_2\\*N(?!CC/3=O)",
  "GulN" = "^u2212h_2\\*N(?!CC/3=O)",
  "AltN" = "^u2111h_2\\*N(?!CC/3=O)",
  "AllN" = "^u2222h_2\\*N(?!CC/3=O)",
  "TalN" = "^u1112h_2\\*N(?!CC/3=O)",
  "IdoN" = "^u2121h_2\\*N(?!CC/3=O)",

  "GlcA" = "^u2122A",
  "ManA" = "^u1122A",
  "GalA" = "^u2112A",
  "GulA" = "^u2212A",
  "AltA" = "^u2111A",
  "AllA" = "^u2222A",
  "TalA" = "^u1112A",
  "IdoA" = "^u2121A",

  "Glc" = "^u2122h(?!_2\\*N(CC/3=O)?)",
  "Man" = "^u1122h(?!_2\\*N(CC/3=O)?)",
  "Gal" = "^u2112h(?!_2\\*N(CC/3=O)?)",
  "Gul" = "^u2212h(?!_2\\*N(CC/3=O)?)",
  "Alt" = "^u2111h(?!_2\\*N(CC/3=O)?)",
  "All" = "^u2222h(?!_2\\*N(CC/3=O)?)",
  "Tal" = "^u1112h(?!_2\\*N(CC/3=O)?)",
  "Ido" = "^u2121h(?!_2\\*N(CC/3=O)?)",

  "FucNAc" = "^u1221m_2\\*NCC/3=O",
  "QuiNAc" = "^u2122m_2\\*NCC/3=O",
  "RhaNAc" = "^u2211m_2\\*NCC/3=O",
  "6dAltNAc" = "^u2111m_2\\*NCC/3=O",
  "6dTalNAc" = "^u1112m_2\\*NCC/3=O",

  "Fuc" = "^u1221m(?!_2\\*NCC/3=O)",
  "Qui" = "^u2122m(?!_2\\*NCC/3=O|_2\\*N_4\\*N)",
  "Rha" = "^u2211m(?!_2\\*NCC/3=O)",
  "6dGul" = "^u2212m",
  "6dAlt" = "^u2111m(?!_2\\*NCC/3=O)",
  "6dTal" = "^u1112m(?!_2\\*NCC/3=O)",

  "Oli" = "^ud122m",
  "Tyv" = "^u1d22m",
  "Abe" = "^u2d12m",
  "Par" = "^u2d22m",
  "Dig" = "^ud222m",
  "Col" = "^u1d21m",
  "Ara" = "^u211h",
  "Lyx" = "^u221h",
  "Xyl" = "^u212h",
  "Rib" = "^u222h",
  "HexNAc" = "^uxxxxh_2\\*NCC/3=O",
  "HexN" = "^uxxxxh_2\\*N(?!CC/3=O)",
  "HexA" = "^uxxxxA",
  "Hex" = "^uxxxxh",
  "dHexNAc" = "^uxxxxm_2\\*NCC/3=O",
  "dHex" = "^uxxxxm",
  "Pen" = "^uxxxh",
  "dHexNAc" = "^u2112m_2\\*NCC/3=O",
  "dHex" = "^u2112m"
)


WURCS_ALDITOL_MONO_REGEX <- c(
  "MurNAc" = "^h2122h_2\\*NCC/3=O_3\\*OC\\^RCO/4=O/3C",
  "MurNGc" = "^h2122h_2\\*NCCO/3=O_3\\*OC\\^RCO/4=O/3C",
  "Mur" = "^h2122h_3\\*OC\\^RCO/4=O/3C",

  "GlcNAc" = "^h2122h_2\\*NCC/3=O",
  "GalNAc" = "^h2112h_2\\*NCC/3=O",
  "ManNAc" = "^h1122h_2\\*NCC/3=O",
  "GulNAc" = "^h2212h_2\\*NCC/3=O",
  "AltNAc" = "^h2221h_5\\*NCC/3=O",
  "AllNAc" = "^h2222h_2\\*NCC/3=O",
  "TalNAc" = "^h1222h_5\\*NCC/3=O",
  "IdoNAc" = "^h2121h_2\\*NCC/3=O",

  "GlcN" = "^h2122h_2\\*N",
  "ManN" = "^h1122h_2\\*N",
  "GalN" = "^h2112h_2\\*N",
  "GulN" = "^h2212h_2\\*N",
  "AltN" = "^h2221h_5\\*N",
  "AllN" = "^h2222h_2\\*N",
  "TalN" = "^h1222h_5\\*N",
  "IdoN" = "^h2121h_2\\*N",

  "Glc" = "^h2122h",
  "Man" = "^h1122h",
  "Gal" = "^h2112h",
  "Gul" = "^h2212h",
  "Alt" = "^h2221h",
  "All" = "^h2222h",
  "Tal" = "^h1222h",
  "Ido" = "^h2121h",

  "FucNAc" = "^h1221m_2\\*NCC/3=O",
  "QuiNAc" = "^h2122m_2\\*NCC/3=O",
  "RhaNAc" = "^h2211m_2\\*NCC/3=O",
  "6dAltNAc" = "^h2111m_2\\*NCC/3=O",
  "6dTalNAc" = "^h1112m_2\\*NCC/3=O",

  "Bac" = "^h2122m_2\\*N_4\\*N",
  "Fuc" = "^h1221m",
  "Qui" = "^h2122m",
  "Rha" = "^h2211m",
  "6dGul" = "^h2212m",
  "6dAlt" = "^h2111m",
  "6dTal" = "^h1112m",

  "Oli" = "^hd122m",
  "Tyv" = "^h1d22m",
  "Abe" = "^h2d12m",
  "Par" = "^h2d22m",
  "Dig" = "^hd222m",
  "Col" = "^h1d21m",
  "Lyx" = "^h221h",
  "Xyl" = "^h212h",
  "Rib" = "^h222h",

  "NeuAc" = "^hUdxxxxxh(?=.*_5\\*NCC/3=O)",
  "NeuGc" = "^hUdxxxxxh(?=.*_5\\*NCCO/3=O)",
  "gNeu" = "^hUdxxxxxh(?=.*_5\\*N(?!CC(O)?/3=O))",
  "gKdn" = "^hUdxxxxxh(?!.*_5\\*N)",

  "Neu5Ac" = "^hUd21122h_5\\*NCC/3=O",
  "Neu5Gc" = "^hUd21122h_5\\*NCCO/3=O",
  "Neu" = "^hUd21122h_5\\*N",
  "Kdn" = "^hUd21122h",
  "Pse" = "^hUd22111m_5\\*N_7\\*N",
  "Leg" = "^hUd21122m_5\\*N_7\\*N",
  "Aci" = "^hUd21111m_5\\*N_7\\*N",
  "4eLeg" = "^hUd11122m_5\\*N_7\\*N",
  "LDmanHep" = "^h21122h",
  "Kdo" = "^hUd1122h",
  "Dha" = "^A122dUh",
  "DDmanHep" = "^h11222h",
  "Fru" = "^hU122h",
  "Tag" = "^hU112h",
  "Sor" = "^hU121h",
  "Psi" = "^hU222h",
  "HexNAc" = "^hxxxxh_2\\*NCC/3=O",
  "HexN" = "^hxxxxh_2\\*N(?!CC/3=O)",
  "HexA" = "^hxxxxA",
  "Hex" = "^hxxxxh",
  "dHexNAc" = "^hxxxxm_2\\*NCC/3=O",
  "dHex" = "^hxxxxm",
  "Pen" = "^hxxxh"
)


#' Invert the stereochemical backbone in a WURCS pattern
#'
#' @param pattern A WURCS monosaccharide regular expression.
#'
#' @return A pattern for the opposite absolute configuration.
#' @noRd
invert_wurcs_pattern_configuration <- function(pattern) {
  backbone <- stringr::str_extract(pattern, "(?<=\\^)[[:alnum:]]+")
  inverted <- chartr("12", "21", backbone)
  stringr::str_replace(pattern, stringr::fixed(backbone), inverted)
}


#' Add unusual configurations to WURCS monosaccharide patterns
#'
#' @param patterns Named WURCS monosaccharide regular expressions.
#'
#' @return Patterns including configuration-inverted counterparts.
#' @noRd
add_unusual_wurcs_patterns <- function(patterns) {
  unusual_map <- unusual_configuration_monosaccharide_map()
  configurable <- names(patterns) %in% names(unusual_map)
  unusual <- purrr::map_chr(
    patterns[configurable],
    invert_wurcs_pattern_configuration
  )
  names(unusual) <- unname(unusual_map[names(patterns)[configurable]])

  # Preserve a configuration-specific pattern when it collides with a generic
  # fallback (for example D-FucNAc and dHexNAc). Indistinguishable concrete
  # aliases keep the existing canonical monosaccharide name.
  duplicate_index <- match(unname(unusual), unname(patterns))
  duplicate_names <- names(patterns)[duplicate_index]
  generic <- glyrepr::available_monosaccharides("generic")
  unusual <- unusual[is.na(duplicate_names) | duplicate_names %in% generic]

  c(unusual, patterns)
}


WURCS_MONO_REGEX <- add_unusual_wurcs_patterns(WURCS_MONO_REGEX)
WURCS_UNKNOWN_RING_MONO_REGEX <- add_unusual_wurcs_patterns(
  WURCS_UNKNOWN_RING_MONO_REGEX
)
WURCS_AMBIGUOUS_MONO_REGEX <- add_unusual_wurcs_patterns(
  WURCS_AMBIGUOUS_MONO_REGEX
)
WURCS_ALDITOL_MONO_REGEX <- add_unusual_wurcs_patterns(
  WURCS_ALDITOL_MONO_REGEX
)


WURCS_SUB_REGEX <- c(
  "Me" = "OC",
  "Ac" = "OCC/3=O",
  "NAc" = "NCC/3=O",
  "P" = "(?:OPO/3O/3=O|PO/2O/2=O)",
  "S" = "OSO/3=O/3=O",
  "Pyr" = "OCCC/4=O/3=O",
  "PC" = "OP(\\^X)?OCCNC/7C/7C/3O/3=O",
  "PPEtn" = "OP(\\^X)?OP(\\^X)?OCCN/5O/5=O/3O/3=O",
  "PEtn" = "OP(\\^X)?OCCN/3O/3=O",
  "N" = "N"
)

wurcs_regex_prefixes <- function(patterns) {
  patterns <- sub("^\\^", "", patterns)
  prefixes <- regmatches(
    patterns,
    regexpr("^[A-Za-z0-9_-]+", patterns)
  )
  prefixes[prefixes == ""] <- NA_character_
  prefixes
}

WURCS_MONO_PREFIXES <- wurcs_regex_prefixes(WURCS_MONO_REGEX)
WURCS_UNKNOWN_RING_MONO_PREFIXES <- wurcs_regex_prefixes(
  WURCS_UNKNOWN_RING_MONO_REGEX
)
WURCS_ALDITOL_MONO_PREFIXES <- wurcs_regex_prefixes(
  WURCS_ALDITOL_MONO_REGEX
)
WURCS_AMBIGUOUS_MONO_PREFIXES <- wurcs_regex_prefixes(
  WURCS_AMBIGUOUS_MONO_REGEX
)

detect_wurcs_pattern <- function(residue, patterns, prefixes) {
  candidates <- which(
    is.na(prefixes) |
      startsWith(residue, prefixes)
  )
  if (length(candidates) == 0L) {
    candidates <- seq_along(patterns)
  }

  matched <- which(stringr::str_detect(residue, patterns[candidates]))
  if (length(matched) == 0L) {
    return(0L)
  }
  candidates[[matched[[1L]]]]
}


#' Detect whether a WURCS residue is an alditol descriptor.
#'
#' @param residue A WURCS monosaccharide residue.
#'
#' @return A logical scalar.
#' @noRd
is_wurcs_alditol_residue <- function(residue) {
  detect_wurcs_pattern(
    residue,
    WURCS_ALDITOL_MONO_REGEX,
    WURCS_ALDITOL_MONO_PREFIXES
  ) >
    0L
}


#' Warn about non-root alditol normalization in WURCS parsing.
#'
#' @return `NULL`, invisibly.
#' @noRd
warn_wurcs_non_root_alditol <- function() {
  cli::cli_warn(c(
    "Only the main reducing-end WURCS residue can retain alditol status.",
    "i" = "Non-root alditol residues are parsed as regular residues."
  ))
  invisible(NULL)
}


#' Restore WURCS N-sulfate as a sulfate substituent.
#'
#' @param residue A WURCS monosaccharide residue.
#' @param sub_code The substituent part left after removing the matched
#'   monosaccharide pattern.
#'
#' @return A normalized substituent code.
#' @noRd
normalize_n_sulfate_sub_code <- function(residue, sub_code) {
  n_sulfate_pos <- stringr::str_extract(
    residue,
    "_(\\d+|\\?)\\*NSO/3=O/3=O",
    group = 1
  )
  if (is.na(n_sulfate_pos)) {
    return(sub_code)
  }

  if (!stringr::str_starts(sub_code, "SO/3=O/3=O")) {
    return(sub_code)
  }

  n_sulfate_sub_code <- stringr::str_glue(
    "_{n_sulfate_pos}*OSO/3=O/3=O"
  )
  stringr::str_replace(sub_code, "^SO/3=O/3=O", n_sulfate_sub_code)
}


#' Get the anomeric position used for alditol normalization.
#'
#' @param mono A monosaccharide name.
#'
#' @return A character scalar containing the anomeric position.
#' @noRd
wurcs_anomer_pos <- function(mono) {
  switch(
    mono,
    Hex = "1",
    HexNAc = "1",
    HexN = "1",
    HexA = "1",
    dHex = "1",
    dHexNAc = "1",
    Pen = "1",
    Neu5Ac = "2",
    Neu5Gc = "2",
    Neu = "2",
    Kdn = "2",
    glyrepr::get_anomer_pos(mono)
  )
}


parse_residue_details <- function(residue) {
  # This function accepts a WURCS residue (something in "[]"),
  # and returns a named vector of c(mono, anomer, sub)
  # `mono`: the IUPAC monosaccharide name
  # `anomer`: the anomer, e.g. "a1", "b2", "?1"
  # `sub`: the substituent, e.g. "3Me", "2Ac", "4NAc", "6P", "?P"
  #        for multiple substituents, they are separated by commas, e.g. "3Me,6S"

  is_alditol <- FALSE
  is_furanose <- stringr::str_detect(
    residue,
    "-(?:1[abx]_1-4|2[abx]_2-5)(?:_|$)"
  )
  matching_residue <- residue |>
    stringr::str_replace("(-1[abx]_1)-4", "\\1-5") |>
    stringr::str_replace("(-2[abx]_2)-5", "\\1-6")

  # Get monosaacharide name
  mono_idx <- detect_wurcs_pattern(
    matching_residue,
    WURCS_MONO_REGEX,
    WURCS_MONO_PREFIXES
  )
  if (mono_idx == 0) {
    unknown_ring_mono_idx <- detect_wurcs_pattern(
      matching_residue,
      WURCS_UNKNOWN_RING_MONO_REGEX,
      WURCS_UNKNOWN_RING_MONO_PREFIXES
    )
    if (unknown_ring_mono_idx > 0) {
      mono <- names(WURCS_UNKNOWN_RING_MONO_REGEX)[[unknown_ring_mono_idx]]
      mono_pattern <- WURCS_UNKNOWN_RING_MONO_REGEX[[unknown_ring_mono_idx]]
      anomer_code <- stringr::str_extract(residue, "-(\\d+[abx])_", group = 1)
      anomer <- stringr::str_replace(anomer_code, "x", "?")
      anomer <- paste0(
        stringr::str_sub(anomer, 2),
        stringr::str_sub(anomer, 1, 1)
      )
    } else {
      alditol_mono_idx <- detect_wurcs_pattern(
        matching_residue,
        WURCS_ALDITOL_MONO_REGEX,
        WURCS_ALDITOL_MONO_PREFIXES
      )
      if (alditol_mono_idx > 0) {
        mono <- names(WURCS_ALDITOL_MONO_REGEX)[[alditol_mono_idx]]
        mono_pattern <- WURCS_ALDITOL_MONO_REGEX[[alditol_mono_idx]]
        anomer <- paste0("?", wurcs_anomer_pos(mono))
        is_alditol <- TRUE
      } else {
        ambiguous_mono_idx <- detect_wurcs_pattern(
          matching_residue,
          WURCS_AMBIGUOUS_MONO_REGEX,
          WURCS_AMBIGUOUS_MONO_PREFIXES
        )
        if (ambiguous_mono_idx == 0) {
          cli::cli_abort("Unable to parse residue: {.str {residue}}")
        }
        mono <- names(WURCS_AMBIGUOUS_MONO_REGEX)[[ambiguous_mono_idx]]
        mono_pattern <- WURCS_AMBIGUOUS_MONO_REGEX[[ambiguous_mono_idx]]
        anomer <- if (mono %in% c("Hex", "HexNAc", "HexN")) {
          paste0("?", wurcs_anomer_pos(mono))
        } else {
          "??"
        }
      }
    }
  } else {
    mono <- names(WURCS_MONO_REGEX)[[mono_idx]]
    mono_pattern <- WURCS_MONO_REGEX[[mono_idx]]

    # Get anomeric carbon and anomer
    anomer_code <- stringr::str_extract(residue, "-(\\d+[abx])_", group = 1)
    anomer <- stringr::str_replace(anomer_code, "x", "?")
    anomer <- paste0(
      stringr::str_sub(anomer, 2),
      stringr::str_sub(anomer, 1, 1)
    )
  }

  # Get substituent(s)
  # For Neu5Ac and Neu5Gc, we need special handling since the 5-position NAc/NGc
  # is part of the monosaccharide itself, not an additional substituent
  unusual_map <- unusual_configuration_monosaccharide_map()
  identity_mono <- names(unusual_map)[
    match(mono, unname(unusual_map))
  ]
  if (is.na(identity_mono)) {
    identity_mono <- mono
  }
  if (
    identity_mono %in%
      c("Neu5Ac", "Neu5Gc", "Neu") &&
      !is_alditol &&
      stringr::str_starts(matching_residue, "Aad")
  ) {
    # For Neu5Ac/Neu5Gc, remove the base Kdn structure and the characteristic 5-position modification
    backbone <- stringr::str_extract(
      mono_pattern,
      "(?<=\\^)[[:alnum:]]+"
    )
    base_kdn_pattern <- paste0(
      "^",
      backbone,
      "-2[abx]_2-(?:6|\\?)"
    )
    if (identity_mono == "Neu5Ac") {
      # Remove the base Kdn pattern and the 5*NCC/3=O
      sub_code <- stringr::str_remove(matching_residue, base_kdn_pattern)
      sub_code <- stringr::str_remove(sub_code, "_5\\*NCC/3=O")
    } else if (identity_mono == "Neu5Gc") {
      # Remove the base Kdn pattern and the 5*NCCO/3=O
      sub_code <- stringr::str_remove(matching_residue, base_kdn_pattern)
      sub_code <- stringr::str_remove(sub_code, "_5\\*NCCO/3=O")
    } else {
      sub_code <- stringr::str_remove(matching_residue, base_kdn_pattern)
      sub_code <- stringr::str_remove(
        sub_code,
        "_5\\*N(?!CC(O)?/3=O)"
      )
    }
  } else if (
    identity_mono %in%
      c("Neu5Ac", "Neu5Gc", "Neu") &&
      stringr::str_starts(matching_residue, "AUd")
  ) {
    backbone <- stringr::str_extract(
      mono_pattern,
      "(?<=\\^)[[:alnum:]]+"
    )
    base_kdn_pattern <- paste0("^", backbone)
    sub_code <- stringr::str_remove(matching_residue, base_kdn_pattern)
    if (identity_mono == "Neu5Ac") {
      sub_code <- stringr::str_remove(sub_code, "_5\\*NCC/3=O")
    } else if (identity_mono == "Neu5Gc") {
      sub_code <- stringr::str_remove(sub_code, "_5\\*NCCO/3=O")
    } else {
      sub_code <- stringr::str_remove(sub_code, "_5\\*N(?!CC(O)?/3=O)")
    }
  } else if (mono %in% c("NeuAc", "NeuGc", "gNeu")) {
    sub_code <- stringr::str_remove(matching_residue, mono_pattern)
    if (mono == "NeuAc") {
      sub_code <- stringr::str_remove(sub_code, "_5\\*NCC/3=O")
    } else if (mono == "NeuGc") {
      sub_code <- stringr::str_remove(sub_code, "_5\\*NCCO/3=O")
    } else {
      sub_code <- stringr::str_remove(sub_code, "_5\\*N(?!CC(O)?/3=O)")
    }
  } else {
    # For other monosaccharides, use the standard approach
    sub_code <- stringr::str_remove(matching_residue, mono_pattern)
  }
  sub_code <- normalize_n_sulfate_sub_code(residue, sub_code)

  if (sub_code == "") {
    sub <- ""
  } else {
    # Split multiple substituents by "_" and process each one
    sub_parts <- stringr::str_split_1(sub_code, "_")
    # Remove empty strings (from leading "_")
    sub_parts <- sub_parts[sub_parts != ""]

    # Process each substituent part
    substituents <- purrr::map_chr(sub_parts, function(sub_part) {
      # Add back the leading "_" for pattern matching
      sub_part_with_underscore <- paste0("_", sub_part)

      sub_patterns <- stringr::str_glue(
        "^_((?:\\d+(?:\\|\\d+)*)|\\?)\\*{WURCS_SUB_REGEX}$"
      )
      sub_idx <- purrr::detect_index(
        sub_patterns,
        ~ stringr::str_detect(sub_part_with_underscore, .x)
      )
      if (sub_idx == 0) {
        cli::cli_abort(
          "Unable to parse substituent: {.str {sub_part_with_underscore}}"
        )
      }
      sub_name <- names(WURCS_SUB_REGEX)[[sub_idx]]
      sub_pos <- stringr::str_extract(
        sub_part_with_underscore,
        "_((?:\\d+(?:\\|\\d+)*)|\\?)",
        group = 1
      )
      sub_pos <- stringr::str_replace_all(sub_pos, "\\|", "/")
      paste0(sub_pos, sub_name)
    })

    # Join multiple substituents with commas
    sub <- paste(substituents, collapse = ",")
  }

  if (is_furanose) {
    mono <- as_furanose_monosaccharide(mono)
  }

  list(
    value = c(mono = mono, anomer = anomer, sub = sub),
    alditol = is_alditol
  )
}

parse_residue <- function(residue) {
  parse_residue_details(residue)$value
}


#' Extract WURCS residue descriptors.
#'
#' @param x A WURCS string or unique residue part.
#'
#' @return A character vector of residue descriptors without square brackets.
#' @noRd
extract_wurcs_residues <- function(x) {
  residues <- stringr::str_extract_all(x, "\\[.*?\\]")[[1]]
  stringr::str_sub(residues, 2, -2)
}


build_wurcs_residue_cache <- function(x) {
  descriptors <- stringr::str_extract_all(
    x[!is.na(x)],
    "\\[.*?\\]"
  )
  descriptors <- unique(unlist(descriptors, use.names = FALSE))
  descriptors <- stringr::str_sub(descriptors, 2, -2)
  details <- lapply(descriptors, function(descriptor) {
    tryCatch(
      parse_residue_details(descriptor),
      error = identity
    )
  })
  list(descriptors = descriptors, details = details)
}

get_wurcs_residue_details <- function(descriptor, residue_cache = NULL) {
  if (is.null(residue_cache)) {
    return(parse_residue_details(descriptor))
  }

  index <- match(descriptor, residue_cache$descriptors)
  if (is.na(index)) {
    return(parse_residue_details(descriptor))
  }

  details <- residue_cache$details[[index]]
  if (inherits(details, "error")) {
    stop(details)
  }
  details
}

parse_unique_residue_details <- function(x, residue_cache = NULL) {
  residues <- extract_wurcs_residues(x)
  details <- lapply(
    residues,
    get_wurcs_residue_details,
    residue_cache = residue_cache
  )
  list(
    values = lapply(details, `[[`, "value"),
    alditols = vapply(details, `[[`, logical(1), "alditol")
  )
}

parse_unique_residues <- function(x, residue_cache = NULL) {
  # Input: a string of WURCS unique residues part
  # Output: a list of named vectors, each vector contains `mono`, `anomer`, and `sub`
  parse_unique_residue_details(x, residue_cache)$values
}


parse_residue_sequence <- function(x) {
  # Input: a string of WURCS residue sequence part
  # Output: a vector of monosaccharide names
  as.integer(stringr::str_split_1(x, "-"))
}


parse_linkages <- function(x) {
  # Input: a string of WURCS linkages part
  # Output: a list of named lists, each list contains `from`, `to`, and `linkage`
  # `from` and `to` are the indices of monosaccharides in the sequence
  # `linkage` is the linkage strings without anomer, e.g. "1-2", "1-3"
  linkages <- stringr::str_split_1(x, "_")
  purrr::map(linkages, parse_one_linkage)
}


parse_wurcs_linkages <- function(
  x,
  residues,
  alditols = rep(FALSE, length(residues))
) {
  linkages <- stringr::str_split_1(x, "_")
  is_floating <- stringr::str_detect(linkages, stringr::fixed("}"))
  floating <- purrr::map(
    linkages[is_floating],
    parse_wurcs_floating_linkage
  )
  floating_types <- purrr::map_chr(floating, "type")

  list(
    ordinary = purrr::map(
      linkages[!is_floating],
      parse_one_linkage,
      anomers = purrr::map_chr(residues, "anomer"),
      alditols = alditols
    ),
    floating = purrr::map(
      floating[floating_types == "part"],
      "metadata"
    ),
    floating_substituents = purrr::map(
      floating[floating_types == "substituent"],
      "metadata"
    )
  )
}


parse_wurcs_floating_linkage <- function(x) {
  brace_parts <- stringr::str_split_1(x, stringr::fixed("}"))
  if (length(brace_parts) != 2L) {
    cli::cli_abort(
      "Can't parse floating WURCS linkage: {.str {x}}"
    )
  }

  if (stringr::str_starts(brace_parts[[2]], stringr::fixed("*"))) {
    candidates <- parse_wurcs_floating_candidates(
      brace_parts[[1]],
      x,
      context = "substituent"
    )
    sub_code <- stringr::str_remove(brace_parts[[2]], "^\\*")
    substituent <- parse_wurcs_substituent_name(sub_code)
    position <- collapse_floating_substituent_positions(
      candidates$positions
    )

    return(list(
      type = "substituent",
      metadata = list(
        substituent = paste0(position, substituent),
        parents = candidates$parents
      )
    ))
  }
  if (brace_parts[[2]] != "") {
    cli::cli_abort(
      "Can't parse floating WURCS linkage: {.str {x}}"
    )
  }

  parts <- stringr::str_split_1(brace_parts[[1]], stringr::fixed("-"))
  if (length(parts) != 2L) {
    cli::cli_abort(
      "Can't parse floating WURCS linkage: {.str {x}}"
    )
  }
  child <- parse_wurcs_linkage_endpoint(parts[[1]])
  candidates <- parse_wurcs_floating_candidates(
    parts[[2]],
    x,
    context = "linkage"
  )

  list(
    type = "part",
    metadata = list(
      root = child$node,
      child_position = child$position,
      parent_positions = candidates$positions,
      parents = candidates$parents
    )
  )
}


parse_wurcs_floating_candidates <- function(x, source, context) {
  candidates <- purrr::map(
    stringr::str_split_1(x, stringr::fixed("|")),
    parse_wurcs_linkage_endpoint
  )
  candidate_nodes <- purrr::map_int(candidates, "node")
  positions_by_parent <- split(
    purrr::map_chr(candidates, "position"),
    candidate_nodes
  )
  position_sets <- purrr::map(
    positions_by_parent,
    ~ sort(unique(.x))
  )
  if (length(unique(position_sets)) != 1L) {
    cli::cli_abort(
      "Floating WURCS {context}s with parent-specific positions are not supported: {.str {source}}"
    )
  }

  list(
    positions = position_sets[[1]],
    parents = unique(candidate_nodes)
  )
}


parse_wurcs_substituent_name <- function(x) {
  x <- stringr::str_replace(
    x,
    "^NSO/3=O/3=O$",
    "OSO/3=O/3=O"
  )
  patterns <- paste0("^(?:", WURCS_SUB_REGEX, ")$")
  sub_idx <- purrr::detect_index(
    patterns,
    ~ stringr::str_detect(x, .x)
  )
  if (sub_idx == 0L) {
    cli::cli_abort("Unable to parse floating substituent: {.str {x}}")
  }
  names(WURCS_SUB_REGEX)[[sub_idx]]
}


parse_wurcs_linkage_endpoint <- function(x) {
  list(
    node = letter_to_int(stringr::str_sub(x, 1, 1)),
    position = stringr::str_sub(x, 2, -1)
  )
}


parse_one_linkage <- function(x, anomers = NULL, alditols = NULL) {
  # Input: a string of one WURCS linkage, e.g. "a4-b1"
  # Output: a named list of `from`, `to`, and `linkage`
  spl <- stringr::str_split_1(x, "-")
  if (is.null(anomers)) {
    swap_endpoints <- stringr::str_detect(
      spl[[2]],
      stringr::fixed("|")
    )
  } else {
    left_donor <- wurcs_endpoint_can_be_donor(
      spl[[1]],
      anomers,
      alditols
    )
    right_donor <- wurcs_endpoint_can_be_donor(
      spl[[2]],
      anomers,
      alditols
    )
    swap_endpoints <- left_donor &&
      !right_donor ||
      !left_donor &&
        !right_donor &&
        stringr::str_detect(spl[[2]], stringr::fixed("|"))
  }
  if (swap_endpoints) {
    spl <- rev(spl)
  }

  handle_parallel_pos <- function(part) {
    if (stringr::str_detect(part, "|")) {
      parts <- stringr::str_split_1(part, stringr::fixed("|"))
      pos <- stringr::str_sub(parts, 2, -1)
      idx_part <- stringr::str_sub(part, 1, 1)
      pos_part <- if ("?" %in% pos) {
        "?"
      } else {
        stringr::str_c(pos, collapse = "/")
      }
      stringr::str_c(idx_part, pos_part)
    } else {
      part
    }
  }

  from_part <- handle_parallel_pos(spl[[1]])
  to_part <- handle_parallel_pos(spl[[2]])

  from_idx <- letter_to_int(stringr::str_sub(from_part, 1, 1))
  to_idx <- letter_to_int(stringr::str_sub(to_part, 1, 1))
  linkage <- paste0(
    stringr::str_sub(to_part, 2, -1),
    "-",
    stringr::str_sub(from_part, 2, -1)
  )
  list(from = from_idx, to = to_idx, linkage = linkage)
}


wurcs_endpoint_can_be_donor <- function(endpoint, anomers, alditols = NULL) {
  alternatives <- stringr::str_split_1(endpoint, stringr::fixed("|"))
  node <- letter_to_int(stringr::str_sub(alternatives[[1]], 1, 1))
  if (!is.null(alditols) && isTRUE(alditols[[node]])) {
    return(FALSE)
  }
  positions <- stringr::str_sub(alternatives, 2, -1)
  anomer_position <- stringr::str_sub(anomers[[node]], 2, -1)

  any(anomer_position != "?" & positions == anomer_position) ||
    all(positions == "?")
}

letter_to_int <- function(letter) {
  if (stringr::str_detect(letter, "^[a-z]$")) {
    return(utf8ToInt(letter) - utf8ToInt("a") + 1)
  }
  if (stringr::str_detect(letter, "^[A-Z]$")) {
    return(utf8ToInt(letter) - utf8ToInt("A") + 27)
  }
  cli::cli_abort("Invalid WURCS residue ID: {.str {letter}}")
}


prepare_graph_dfs <- function(
  residues,
  linkages,
  alditols = rep(FALSE, length(residues))
) {
  # Generate edgelist dataframe and vertex dataframe.
  # `edgelist_df`: "from", "to", "linkage".
  # `vertex_df`: "name", "mono", "anomer", "sub".
  # Note that the "anomer" column is not need in a `glycan_graph` object.
  vertex_df <- purrr::list_rbind(purrr::map(
    residues,
    ~ data.frame(as.list(.x))
  ))
  vertex_df$alditol <- alditols
  if (length(linkages) == 0L) {
    edgelist_df <- data.frame(
      from = integer(),
      to = integer(),
      linkage = character()
    )
  } else {
    edgelist_df <- purrr::list_rbind(purrr::map(linkages, data.frame))
    # Add anomer to "linkage" column in `edgelist_df`.
    edgelist_df$linkage <- stringr::str_c(
      stringr::str_sub(vertex_df$anomer[edgelist_df$to], 1, 1),
      edgelist_df$linkage
    )
  }
  vertex_df$name <- rownames(vertex_df)
  list(edgelist = edgelist_df, vertex = vertex_df)
}


build_glycan_graph <- function(
  edgelist_df,
  vertex_df,
  floating = list(),
  floating_substituents = list()
) {
  # For format of input values, see `prepare_graph_dfs`.
  graph <- igraph::graph_from_data_frame(
    edgelist_df,
    vertices = vertex_df[c("name", "mono", "sub")]
  )
  if (length(floating) > 0) {
    graph <- annotate_wurcs_floating_parts(graph, vertex_df, floating)
  }
  if (length(floating_substituents) > 0) {
    graph <- annotate_wurcs_floating_substituents(
      graph,
      floating_substituents
    )
  }
  core_node <- find_wurcs_core_node(graph, floating)
  core_anomer <- vertex_df$anomer[core_node]
  graph$anomer <- core_anomer
  graph$alditol <- isTRUE(vertex_df$alditol[core_node])
  graph
}


#' Find the main reducing-end WURCS node
#'
#' @param graph A parsed WURCS graph.
#' @param floating Parsed floating-part metadata.
#'
#' @return The numeric vertex index of the main reducing end.
#' @noRd
find_wurcs_core_node <- function(graph, floating = list()) {
  floating_roots <- purrr::map_int(floating, "root")
  as.integer(igraph::V(graph)[
    igraph::degree(graph, mode = "in") == 0 &
      !seq_len(igraph::vcount(graph)) %in% floating_roots
  ])
}


annotate_wurcs_floating_substituents <- function(
  graph,
  substituents
) {
  all_vertices <- seq_len(igraph::vcount(graph))
  occupied_slots <- definitely_occupied_carbon_slots(
    graph,
    all_vertices
  )

  graph$floating_substituents <- purrr::map(
    substituents,
    function(metadata) {
      parents <- metadata$parents
      domain <- normalize_floating_substituent_parents(
        parents,
        all_vertices,
        metadata$substituent,
        occupied_slots,
        context = "WURCS floating substituent"
      )

      list(
        substituent = domain$substituent,
        parents = as.integer(domain$parents)
      )
    }
  )

  graph
}


annotate_wurcs_floating_parts <- function(graph, vertex_df, floating) {
  components <- igraph::components(graph, mode = "weak")$membership
  floating_components <- components[purrr::map_int(floating, "root")]
  floating_nodes <- purrr::map(
    floating_components,
    ~ as.integer(which(components == .x))
  )
  all_vertices <- seq_len(igraph::vcount(graph))
  occupied_slots <- definitely_occupied_acceptor_slots(
    graph,
    all_vertices
  )

  graph$floating_parts <- purrr::map2(
    floating,
    floating_nodes,
    function(metadata, nodes) {
      parents <- setdiff(metadata$parents, nodes)
      if (length(parents) == 0) {
        cli::cli_abort(
          "A WURCS floating part has no candidate parent outside its own component."
        )
      }

      linkage <- paste0(
        stringr::str_sub(vertex_df$anomer[[metadata$root]], 1, 1),
        metadata$child_position,
        "-",
        paste(metadata$parent_positions, collapse = "/")
      )
      parents <- normalize_floating_part_parents(
        parents,
        setdiff(all_vertices, nodes),
        linkage,
        occupied_slots,
        context = "WURCS floating part"
      )

      list(
        root = as.integer(metadata$root),
        nodes = nodes,
        linkage = linkage,
        parents = as.integer(parents)
      )
    }
  )

  graph
}


do_parse_wurcs <- function(x, residue_cache = NULL) {
  wurcs_regex <- stringr::regex(
    "
    ^WURCS=2\\.0         # WURCS version
    /\\d+,\\d+,\\d+      # unique residue count, residue count, linkage count
    /((?:\\[.*?\\])+)    # unique residues
    /((?:\\d+-)*\\d+)    # residue sequence
    (?:/(.*))?           # linkages, omitted for one residue sequence
    ",
    comments = TRUE
  )
  # Here we assume all characters after "residue sequence" are valid linkages.

  if (!stringr::str_detect(x, wurcs_regex)) {
    cli::cli_abort("Invalid WURCS string: {.str {x}}")
  }

  # unique_residues: a list of named character vectors,
  # each vector contains `mono` ("GlcNAc"), `anomer` ("b1"), and `sub` ("3Me")
  unique_residue_part <- stringr::str_extract(x, wurcs_regex, group = 1)
  residue_details <- parse_unique_residue_details(
    unique_residue_part,
    residue_cache = residue_cache
  )
  unique_residues <- residue_details$values

  # residue_sequence: an integer vector of monosaccharide indices,
  # referring to the order of unique_residues, repeated monosaccharides allowed.
  # e.g. c(1, 1, 2, 3, 3)
  residue_sequence_part <- stringr::str_extract(x, wurcs_regex, group = 2)
  residue_sequence <- parse_residue_sequence(residue_sequence_part)
  residues <- unique_residues[residue_sequence]
  alditols <- residue_details$alditols[residue_sequence]

  # linkages: a list of named lists, each list contains `from`, `to`, and `linkage`.
  # `from` and `to` are the indices of monosaccharides in the sequence.
  # `linkage` is the linkage strings without anomer, e.g. "1-2", "1-3"
  linkage_part <- stringr::str_extract(x, wurcs_regex, group = 3)
  if (linkage_part == "") {
    linkages <- NULL
    floating <- list()
    floating_substituents <- list()
  } else {
    linkage_data <- parse_wurcs_linkages(
      linkage_part,
      residues,
      alditols = alditols
    )
    linkages <- linkage_data$ordinary
    floating <- linkage_data$floating
    floating_substituents <- linkage_data$floating_substituents
  }

  graph_dfs <- prepare_graph_dfs(residues, linkages, alditols = alditols)
  graph <- build_glycan_graph(
    graph_dfs$edgelist,
    graph_dfs$vertex,
    floating = floating,
    floating_substituents = floating_substituents
  )
  core_node <- find_wurcs_core_node(graph, floating)
  if (any(alditols[-core_node])) {
    warn_wurcs_non_root_alditol()
  }
  graph
}
