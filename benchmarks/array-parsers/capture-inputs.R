# Capture evaluated inputs (including pasted WURCS/GlycoCT records) from the
# unchanged baseline test suite. Run before.R from a checkout of baseline HEAD.
args <- commandArgs(TRUE)
pkgload::load_all(args[[1]], quiet = TRUE)
inputs <- list()
exports <- getNamespaceExports("glyparse")
capture_inputs <- function(x, calls) {
  if (!is.character(x)) {
    return(invisible(NULL))
  }
  called <- vapply(
    calls,
    function(call) as.character(call[[1]])[[1]],
    character(1)
  )
  called <- called[called %in% exports]
  if (!length(called)) {
    return(invisible(NULL))
  }
  key <- tail(called, 1)
  inputs[[key]] <<- unique(c(inputs[[key]], unname(x)))
}
for (fun in c("struc_parser_wrapper", "normalized_struc_parser_wrapper")) {
  trace(
    fun,
    tracer = quote(.GlobalEnv$capture_inputs(x, sys.calls())),
    print = FALSE,
    where = asNamespace("glyparse")
  )
}
testthat::test_dir(file.path(args[[1]], "tests/testthat"), reporter = "silent")
saveRDS(inputs, args[[2]], version = 3)
print(lengths(inputs))
