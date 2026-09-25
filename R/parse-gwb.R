#' Parse GlycoWorkbench Structures
#'
#' Parse GlycoWorkbench (GWB/GWS) structure strings into a
#' [glyrepr::glycan_structure()].
#'
#' @details
#' GlycoWorkbench writes glycans from the reducing end towards the
#' non-reducing ends. Residues include their anomer, configuration, and ring
#' form, for example `"--4b1D-Gal,p"`. Branches are enclosed in parentheses,
#' and the structure is followed by mass options after `$`.
#'
#' The parser normalizes the glycan tree to IUPAC-condensed notation before
#' constructing the glycan structure. GlycoWorkbench substituent nodes such as
#' `"--6S"` and `"--9Ac"` are retained as monosaccharide substituents. Mass
#' options are ignored because they are not part of the glycan graph.
#' Explicit open-chain residues (`,o`) are supported only for a reduced
#' `redEnd` root; other open-chain forms cannot be represented by `glyrepr`.
#'
#' @param x A character vector of GlycoWorkbench strings. NA values are allowed
#'   and will be returned as NA structures.
#' @param on_failure How to handle parsing failures. `"error"` aborts when a
#'   structure cannot be parsed. `"na"` returns `NA` at invalid positions.
#' @param progress Whether to show a progress bar while parsing.
#'
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' gwb <- paste0(
#'   "freeEnd--1b1D-GlcNAc,p(--6a1L-Fuc,p)",
#'   "--4b1D-Gal,p--3a2D-NeuAc,p$MONO,Und,0,0,freeEnd"
#' )
#' parse_gwb(gwb)
#'
#' @seealso [parse_iupac_condensed()]
#'
#' @export
parse_gwb <- function(
  x,
  on_failure = "error",
  progress = FALSE
) {
  normalized_struc_parser_wrapper(
    x,
    convert_gwb_to_condensed,
    on_failure = on_failure,
    progress = progress
  )
}
