#' Map natural monosaccharide names to their unusual configurations
#'
#' @return A named character vector from natural to unusual configurations.
#' @noRd
unusual_configuration_monosaccharide_map <- local({
  map <- NULL

  function() {
    if (is.null(map)) {
      monos <- glyrepr::available_monosaccharides("concrete")
      unusual <- monos[
        stringr::str_detect(monos, "^[DL]") &
          stringr::str_sub(monos, 2) %in% monos
      ]
      map <<- rlang::set_names(unusual, stringr::str_sub(unusual, 2))
    }
    map
  }
})


#' Apply an explicit monosaccharide configuration
#'
#' @param mono A glyrepr monosaccharide name in its natural configuration.
#' @param configuration An explicit `D`, `L`, or unknown configuration.
#'
#' @return A glyrepr monosaccharide name.
#' @noRd
apply_monosaccharide_configuration <- function(mono, configuration) {
  map <- unusual_configuration_monosaccharide_map()
  natural <- names(map)[order(nchar(names(map)), decreasing = TRUE)]

  purrr::map2_chr(mono, configuration, function(value, config) {
    matched <- natural[stringr::str_starts(value, stringr::fixed(natural))]
    if (length(matched) == 0) {
      return(value)
    }

    matched <- matched[[1]]
    unusual <- unname(map[[matched]])
    if (is.na(config) || stringr::str_sub(unusual, 1, 1) != config) {
      return(value)
    }

    suffix <- stringr::str_sub(value, nchar(matched) + 1)
    paste0(unusual, suffix)
  })
}


#' Swap explicit D and L configuration markers
#'
#' @param x A character vector containing single-letter configurations.
#'
#' @return A character vector with `D` and `L` exchanged.
#' @noRd
invert_configuration <- function(x) {
  chartr("DLdl", "LDld", x)
}


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
  source_names <- names(map)
  mapped <- unname(map)
  ring_suffix <- stringr::str_extract(
    source_names,
    "(?<=p)[^p]*$"
  )
  has_pyranose_marker <- !is.na(ring_suffix) &
    (ring_suffix == "" | endsWith(mapped, ring_suffix))
  can_convert <- mapped %in%
    names(furanose_monosaccharide_map()) &
    has_pyranose_marker
  furanose_names <- stringr::str_replace(
    source_names[can_convert],
    "p([^p]*)$",
    "f\\1"
  )
  map[furanose_names] <- as_furanose_monosaccharide(mapped[can_convert])
  map
}
