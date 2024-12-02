# The anomer positions are fix for known monosaccharides.
# Except for the monosaccharides with C2 anomer, all others are on C1.
decide_anomer_pos <- function(mono) {
  anomer_on_pos2 <- c(
    "Neu5Ac", "Neu5Gc", "Neu", "Kdn", "Pse", "Leg", "Aci",
    "4eLeg", "Kdo", "Dha", "Fru", "Tag", "Sor", "Psi"
  )
  dplyr::if_else(mono %in% anomer_on_pos2, "2", "1")
}
