#' Map ringless concrete monosaccharides to their furanose forms
#'
#' @return A named character vector.
#' @noRd
furanose_monosaccharide_map <- local({
  map <- NULL

  function() {
    if (is.null(map)) {
      monos <- glyrepr::available_monosaccharides("concrete")
      furanose <- monos[stringr::str_detect(monos, stringr::fixed("f"))]
      ringless <- stringr::str_remove(furanose, stringr::fixed("f"))
      known_ringless <- ringless %in% monos
      map <<- rlang::set_names(
        furanose[known_ringless],
        ringless[known_ringless]
      )
    }
    map
  }
})

#' Convert concrete monosaccharides to their furanose forms
#'
#' Generic and already-furanose monosaccharides are returned unchanged.
#'
#' @param mono A character vector of monosaccharide names.
#'
#' @return A character vector.
#' @noRd
as_furanose_monosaccharide <- function(mono) {
  converted <- unname(furanose_monosaccharide_map()[mono])
  converted[is.na(converted)] <- mono[is.na(converted)]
  converted
}

#' Convert concrete furanose monosaccharides to their ringless forms
#'
#' @param mono A character vector of monosaccharide names.
#'
#' @return A character vector.
#' @noRd
as_ringless_monosaccharide <- function(mono) {
  map <- furanose_monosaccharide_map()
  converted <- unname(rlang::set_names(names(map), unname(map))[mono])
  converted[is.na(converted)] <- mono[is.na(converted)]
  converted
}

#' Add furanose source labels to a monosaccharide map
#'
#' Source formats such as GlyCAM IUPAC and LINUCS insert the ring marker by
#' replacing the last `p` in a pyranose label with `f`.
#'
#' @param map A named character vector from source labels to glyrepr names.
#'
#' @return A named character vector containing furanose aliases.
#' @noRd
add_furanose_monosaccharide_mappings <- function(map) {
  mapped <- unname(map)
  can_convert <- mapped %in%
    names(furanose_monosaccharide_map()) &
    stringr::str_detect(names(map), "p")
  furanose_names <- stringr::str_replace(
    names(map)[can_convert],
    "p([^p]*)$",
    "f\\1"
  )
  map[furanose_names] <- as_furanose_monosaccharide(mapped[can_convert])
  map
}
