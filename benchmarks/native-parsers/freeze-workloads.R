# Run once against the R-parser revision (1cc34cd) before timing either build.
args <- commandArgs(TRUE)
pkgload::load_all(args[[1]], quiet = TRUE)
inputs <- readRDS("benchmarks/array-parsers/inputs.rds")
workloads <- lapply(names(inputs), function(name) {
  x <- unique(inputs[[name]])
  x[
    !is.na(x) &
      !is.na(suppressWarnings(getExportedValue("glyparse", name)(
        x,
        on_failure = "na"
      )))
  ]
}) |>
  stats::setNames(names(inputs))
previous <- readRDS("benchmarks/array-parsers/workloads.rds")
workloads$parse_pglyco_struc <- previous$parse_pglyco_struc
workloads$parse_strucgp_struc <- previous$parse_strucgp_struc
workloads$auto_parse <- unique(unlist(
  workloads[names(workloads) != "auto_parse"],
  use.names = FALSE
))
workloads$auto_parse <- workloads$auto_parse[
  !is.na(suppressWarnings(auto_parse(workloads$auto_parse, on_failure = "na")))
]
workloads$auto_iupac <- previous$auto_iupac
saveRDS(workloads, "benchmarks/native-parsers/workloads.rds", version = 3)
print(lengths(workloads))
