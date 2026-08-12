# The anomer positions are fixed for concrete monosaccharides.
# Generic monosaccharides default to C1.
decide_anomer_pos <- function(mono) {
  anomer_pos <- rep("1", length(mono))
  concrete <- mono %in% glyrepr::available_monosaccharides("concrete")
  anomer_pos[concrete] <- glyrepr::get_anomer_pos(mono[concrete])
  anomer_pos
}
