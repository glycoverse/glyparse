#' Parse Glycan SMILES
#'
#' Parse supported glycan SMILES into a [glyrepr::glycan_structure()] using
#' native R/C++ code. No chemical toolkit, network request, or database lookup
#' is required at runtime.
#'
#' @details
#' The parser reads the atom/bond graph, identifies sugar carbon backbones,
#' computes tetrahedral configurations from the SMILES neighbor order, and
#' constructs WURCS residue and linkage descriptors. These descriptors use
#' the same native residue mappings as [parse_wurcs()]. Different atom
#' traversals, branch orders, and ring labels do not change the result.
#'
#' Supported inputs are connected tree glycans with unbranched, saturated
#' carbon backbones of 5--9 atoms and five- or six-membered sugar rings.
#' Supported chemistry includes the monosaccharides recognized by
#' `parse_wurcs()` within this scope, such as hexoses, pentoses, deoxy sugars,
#' hexosamines, uronic acids, and sialic acids. Supported substituents include
#' amino, N- and O-acetyl/glycolyl, and neutral sulfate/phosphate groups.
#' Glycosidic bridges must be O-linked with at least one anomeric endpoint.
#'
#' Tetrahedral `@` and `@@` tags are interpreted relative to their neighbors,
#' including ring closures. Missing stereochemistry is never inferred from
#' common sugar identities or a database accession. An unknown configuration
#' is retained where the `glyrepr` vocabulary can express it; otherwise the
#' input fails. For non-reducing glycans with anomeric-to-anomeric bonds, a
#' deterministic root is chosen from the labeled residue graph. It may
#' differ from a WURCS file's conventional root without changing the glycan.
#'
#' This is not a general-purpose SMILES reader. Unsupported syntax or chemistry
#' fails rather than being silently removed: examples include charges,
#' isotopes, separate explicit hydrogen atoms, aromatic atoms, stereochemical
#' classes other than tetrahedral `@`/`@@`, disconnected salts, aglycones,
#' open-chain sugars/alditols, non-O glycosidic bonds, cyclic residue graphs,
#' and unrecognized modifications. Only neutral C, N, O, S, and P atoms are
#' accepted. Limits are 10,000 atoms and 52 residues per input. SMILES are not
#' detected by [auto_parse()]; call this function explicitly.
#'
#' @inheritParams parse_glycam_iupac
#' @return A [glyrepr::glycan_structure()] object.
#'
#' @examples
#' # Beta-D-glucose, GlyGen G71142DF / PubChem 64689
#' glucose <- "C([C@@H]1[C@H]([C@@H]([C@H]([C@@H](O1)O)O)O)O)O"
#' parse_smiles(glucose)
#' parse_smiles(c(glucose, NA, "not SMILES"), on_failure = "na")
#'
#' @seealso [parse_wurcs()]
#' @export
parse_smiles <- function(x, on_failure = "error", progress = FALSE) {
  struc_parser_wrapper(
    x,
    "smiles",
    on_failure = on_failure,
    progress = progress
  )
}
