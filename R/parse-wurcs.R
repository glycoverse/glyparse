#' Parse WURCS Structures
#'
#' This function parses WURCS strings into a [glyrepr::glycan_structure()].
#' Currently, only WURCS 2.0 is supported.
#' For more information about WURCS, see [WURCS](https://github.com/glycoinfo/WURCS/wiki).
#' Alditol residues are parsed as regular reducing-end glycans with unknown
#' anomer configurations.
#'
#' @param x A character vector of WURCS strings. NA values are allowed and will be returned as NA structures.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param progress Whether to show a progress bar while parsing.
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
parse_wurcs <- function(x, on_failure = "error", progress = FALSE) {
  struc_parser_wrapper(
    x,
    do_parse_wurcs,
    on_failure = on_failure,
    progress = progress
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
  "Ara" = "^a211h-1[abx]_1-5",
  "Lyx" = "^a221h-1[abx]_1-5",
  "Xyl" = "^a212h-1[abx]_1-5",
  "Rib" = "^a222h-1[abx]_1-5",

  # Neu5Ac and Neu5Gc - match if contains _5*NCC/3=O or _5*NCCO/3=O anywhere in the string
  # These must come before Kdn since they are more specific
  "Neu5Ac" = "^Aad21122h-2[abx]_2-6.*_5\\*NCC/3=O",
  "Neu5Gc" = "^Aad21122h-2[abx]_2-6.*_5\\*NCCO/3=O",

  # Kdn: exclude N, Ac, and Gc
  "Kdn" = "^Aad21122h-2[abx]_2-6(?!_5\\*N(CC(O)?/3=O)?)",

  # Neu: exclude Ac and Gc
  "Neu" = "^Aad21122h-2[abx]_2-6_5\\*N(?!CC(O)?/3=O)",

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


WURCS_SUB_REGEX <- c(
  "Me" = "OC",
  "Ac" = "OCC/3=O",
  "NAc" = "NCC/3=O",
  "P" = "OPO/3O/3=O",
  "S" = "OSO/3=O/3=O",
  "Pyr" = "OCCC/4=O/3=O",
  "PC" = "OP(\\^X)?OCCNC/7C/7C/3O/3=O",
  "PPEtn" = "OP(\\^X)?OP(\\^X)?OCCN/5O/5=O/3O/3=O",
  "PEtn" = "OP(\\^X)?OCCN/3O/3=O",
  "N" = "N"
)


#' Detect whether a WURCS residue is an alditol descriptor.
#'
#' @param residue A WURCS monosaccharide residue.
#'
#' @return A logical scalar.
#' @noRd
is_wurcs_alditol_residue <- function(residue) {
  purrr::detect_index(
    WURCS_ALDITOL_MONO_REGEX,
    ~ stringr::str_detect(residue, .x)
  ) >
    0
}


#' Warn about alditol normalization in WURCS parsing.
#'
#' @return `NULL`, invisibly.
#' @noRd
warn_wurcs_alditol <- function() {
  cli::cli_warn(c(
    "Alditol WURCS residues are parsed as regular reducing-end glycans with unknown anomer configurations.",
    "i" = "For example, GlcNAc-ol is returned as GlcNAc(?1-."
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


parse_residue <- function(residue) {
  # This function accepts a WURCS residue (something in "[]"),
  # and returns a named vector of c(mono, anomer, sub)
  # `mono`: the IUPAC monosaccharide name
  # `anomer`: the anomer, e.g. "a1", "b2", "?1"
  # `sub`: the substituent, e.g. "3Me", "2Ac", "4NAc", "6P", "?P"
  #        for multiple substituents, they are separated by commas, e.g. "3Me,6S"

  is_alditol <- FALSE

  # Get monosaacharide name
  mono_idx <- purrr::detect_index(
    WURCS_MONO_REGEX,
    ~ stringr::str_detect(residue, .x)
  )
  if (mono_idx == 0) {
    unknown_ring_mono_idx <- purrr::detect_index(
      WURCS_UNKNOWN_RING_MONO_REGEX,
      ~ stringr::str_detect(residue, .x)
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
      alditol_mono_idx <- purrr::detect_index(
        WURCS_ALDITOL_MONO_REGEX,
        ~ stringr::str_detect(residue, .x)
      )
      if (alditol_mono_idx > 0) {
        mono <- names(WURCS_ALDITOL_MONO_REGEX)[[alditol_mono_idx]]
        mono_pattern <- WURCS_ALDITOL_MONO_REGEX[[alditol_mono_idx]]
        anomer <- paste0("?", wurcs_anomer_pos(mono))
        is_alditol <- TRUE
      } else {
        ambiguous_mono_idx <- purrr::detect_index(
          WURCS_AMBIGUOUS_MONO_REGEX,
          ~ stringr::str_detect(residue, .x)
        )
        if (ambiguous_mono_idx == 0) {
          cli::cli_abort("Unable to parse residue: {.str {residue}}")
        }
        mono <- names(WURCS_AMBIGUOUS_MONO_REGEX)[[ambiguous_mono_idx]]
        mono_pattern <- WURCS_AMBIGUOUS_MONO_REGEX[[ambiguous_mono_idx]]
        anomer <- "??"
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
  if (
    mono %in%
      c("Neu5Ac", "Neu5Gc") &&
      !is_alditol &&
      stringr::str_starts(residue, "Aad")
  ) {
    # For Neu5Ac/Neu5Gc, remove the base Kdn structure and the characteristic 5-position modification
    base_kdn_pattern <- "^Aad21122h-2[abx]_2-6"
    if (mono == "Neu5Ac") {
      # Remove the base Kdn pattern and the 5*NCC/3=O
      sub_code <- stringr::str_remove(residue, base_kdn_pattern)
      sub_code <- stringr::str_remove(sub_code, "_5\\*NCC/3=O")
    } else {
      # Neu5Gc
      # Remove the base Kdn pattern and the 5*NCCO/3=O
      sub_code <- stringr::str_remove(residue, base_kdn_pattern)
      sub_code <- stringr::str_remove(sub_code, "_5\\*NCCO/3=O")
    }
  } else if (
    mono %in%
      c("Neu5Ac", "Neu5Gc", "Neu") &&
      stringr::str_starts(residue, "AUd")
  ) {
    base_kdn_pattern <- "^AUd21122h"
    sub_code <- stringr::str_remove(residue, base_kdn_pattern)
    if (mono == "Neu5Ac") {
      sub_code <- stringr::str_remove(sub_code, "_5\\*NCC/3=O")
    } else if (mono == "Neu5Gc") {
      sub_code <- stringr::str_remove(sub_code, "_5\\*NCCO/3=O")
    } else {
      sub_code <- stringr::str_remove(sub_code, "_5\\*N(?!CC(O)?/3=O)")
    }
  } else {
    # For other monosaccharides, use the standard approach
    sub_code <- stringr::str_remove(residue, mono_pattern)
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

      sub_patterns <- stringr::str_glue("^_(\\d+|\\?)\\*{WURCS_SUB_REGEX}$")
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
        "_(\\d+|\\?)",
        group = 1
      )
      paste0(sub_pos, sub_name)
    })

    # Join multiple substituents with commas
    sub <- paste(substituents, collapse = ",")
  }

  c(mono = mono, anomer = anomer, sub = sub)
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


parse_unique_residues <- function(x) {
  # Input: a string of WURCS unique residues part
  # Output: a list of named vectors, each vector contains `mono`, `anomer`, and `sub`
  residues <- extract_wurcs_residues(x)
  purrr::map(residues, parse_residue)
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


parse_one_linkage <- function(x) {
  # Input: a string of one WURCS linkage, e.g. "a4-b1"
  # Output: a named list of `from`, `to`, and `linkage`
  spl <- stringr::str_split_1(x, "-")

  handle_parallel_pos <- function(part) {
    if (stringr::str_detect(part, "|")) {
      parts <- stringr::str_split_1(part, stringr::fixed("|"))
      pos <- stringr::str_sub(parts, 2, -1)
      idx_part <- stringr::str_sub(part, 1, 1)
      pos_part <- stringr::str_c(pos, collapse = "/")
      stringr::str_c(idx_part, pos_part)
    } else {
      part
    }
  }

  from_part <- handle_parallel_pos(spl[[1]])
  to_part <- handle_parallel_pos(spl[[2]])
  if (stringr::str_detect(to_part, stringr::fixed("/"))) {
    # WURCS has a strange linkage rule:
    # In the normal case, the linkage positions are opposite to the orders of IUPAC.
    # For example, "a4-b1" means "1-4" in IUPAC.
    # However, when second position is a parallel position, e.g. "b2|b2",
    # the linkage positions are the same as the orders of IUPAC.
    # For example, "f2-g3|g6" means "2-3/6" in IUPAC.
    # In this case, we need to swap from_part and to_part.
    temp <- from_part
    from_part <- to_part
    to_part <- temp
  }

  from_idx <- letter_to_int(stringr::str_sub(from_part, 1, 1))
  to_idx <- letter_to_int(stringr::str_sub(to_part, 1, 1))
  linkage <- paste0(
    stringr::str_sub(to_part, 2, -1),
    "-",
    stringr::str_sub(from_part, 2, -1)
  )
  list(from = from_idx, to = to_idx, linkage = linkage)
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


prepare_graph_dfs <- function(residues, linkages) {
  # Generate edgelist dataframe and vertex dataframe.
  # `edgelist_df`: "from", "to", "linkage".
  # `vertex_df`: "name", "mono", "anomer", "sub".
  # Note that the "anomer" column is not need in a `glycan_graph` object.
  vertex_df <- purrr::list_rbind(purrr::map(
    residues,
    ~ data.frame(as.list(.x))
  ))
  if (is.null(linkages)) {
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


build_glycan_graph <- function(edgelist_df, vertex_df) {
  # For format of input values, see `prepare_graph_dfs`.
  graph <- igraph::graph_from_data_frame(
    edgelist_df,
    vertices = vertex_df[c("name", "mono", "sub")]
  )
  core_node <- igraph::V(graph)[igraph::degree(graph, mode = "in") == 0]
  core_anomer <- vertex_df$anomer[as.numeric(core_node)]
  graph$anomer <- core_anomer
  graph$alditol <- FALSE # Not implemented yet
  graph
}


do_parse_wurcs <- function(x) {
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
  unique_residue_descriptors <- extract_wurcs_residues(unique_residue_part)
  unique_residues <- parse_unique_residues(unique_residue_part)
  if (
    any(purrr::map_lgl(unique_residue_descriptors, is_wurcs_alditol_residue))
  ) {
    warn_wurcs_alditol()
  }

  # residue_sequence: an integer vector of monosaccharide indices,
  # referring to the order of unique_residues, repeated monosaccharides allowed.
  # e.g. c(1, 1, 2, 3, 3)
  residue_sequence_part <- stringr::str_extract(x, wurcs_regex, group = 2)
  residue_sequence <- parse_residue_sequence(residue_sequence_part)

  # linkages: a list of named lists, each list contains `from`, `to`, and `linkage`.
  # `from` and `to` are the indices of monosaccharides in the sequence.
  # `linkage` is the linkage strings without anomer, e.g. "1-2", "1-3"
  linkage_part <- stringr::str_extract(x, wurcs_regex, group = 3)
  if (linkage_part == "") {
    linkages <- NULL
  } else {
    linkages <- parse_linkages(linkage_part)
  }

  residues <- unique_residues[residue_sequence]
  graph_dfs <- prepare_graph_dfs(residues, linkages)
  graph <- build_glycan_graph(graph_dfs$edgelist, graph_dfs$vertex)
  graph
}
