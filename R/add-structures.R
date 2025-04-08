#' Add Structures to Experiment
#'
#' Parse glycan structures and store the results as the
#' `glycan_structures` attribute of the experiment.
#' Please make sure a "glycan_structure" column is in the `var_info` tibble.
#' This can be checked by interactively printing the experiment.
#'
#' @details
#' The `glycan_structures` attribute will be a named list of glycan graph objects.
#' See [glyrepr::as_glycan_graph()] for more information about this data structure.
#' Names are the same as the glycan structure strings.
#'
#' @param exp A `glyexp_experiment` object.
#' @param mode A character string, either "ne" or "dn". Default to "ne".
#'   See [glyrepr::as_glycan_graph()] for more information about graph modes.
#' @param override A logical value. If `TRUE`, override existing glycan structures.
#' @param silence A logical value. Controls how to behave when glycan structures
#'   already exist and `override` is `FALSE`.
#'   If `TRUE`, do nothing and return `exp` unchanged immediately.
#'   If `FALSE`, throw an error.
#'   Setting to `TRUE` also suppresses alerts when `override` is `TRUE`.
#'   Use this argument with caution.
#'
#' @return A `glyexp_experiment` object with a `glycan_structures` attribute.
#' @export
add_structures <- function(exp, mode = "ne", override = FALSE, silence = FALSE) {
  # Check arguments
  checkmate::assert_class(exp, "glyexp_experiment")
  checkmate::assert_flag(override)
  checkmate::assert_flag(silence)
  if (!"glycan_structure" %in% colnames(exp$var_info)) {
    error_msg <- "Structure column not found in the experiment."
    if (!is.null(exp$meta_data$aggr_level) && exp$meta_data$aggr_level %in% c("gp", "gf")) {
      hint <- paste0(
        "The experiment has been aggregated to {.val {exp$meta_data$aggr_level}} level, ",
        "so structure information is no longer available. ",
        "Please re-run {.fun glyclean::aggregate} or {.fun glyclean::auto_clean} ",
        "with {.arg to_level} = {.val gfs} or {.val gps}. ",
        "See {.fun glyclean::aggregate} for details."
      )
      cli::cli_abort(c(error_msg, i = hint))
    }
    cli::cli_abort(error_msg)
  }
  if (!is.null(exp$glycan_structures)) {
    if (override) {
      if (!silence) {
        cli::cli_alert_info("Overriding existing glycan structures.")
      }
      exp$glycan_structures <- NULL
    } else {
      if (silence) {
        return(exp)
      }
      cli::cli_abort("The experiment already has glycan structures.")
    }
  }

  # Parse structures
  if (is.null(exp$meta_data$structure_type)) {
    cli::cli_abort("The experiment does not have a structure type.")
  }
  parser <- switch(
    exp$meta_data$structure_type,
    pglyco = parse_pglyco_struc,
    strucgp = parse_strucgp_struc,
    msfragger = parse_wurcs,
    function(...) rlang::abort("Unsupported structure type.")
  )
  structure_strings <- unique(exp$var_info$glycan_structure)
  glycan_structures <- parser(structure_strings, mode = mode)
  exp$glycan_structures <- glycan_structures
  exp
}
