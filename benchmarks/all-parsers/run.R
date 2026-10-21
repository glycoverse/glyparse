args <- commandArgs(TRUE)
stopifnot(length(args) == 3L, args[1] %in% c("baseline", "candidate"))
phase <- args[1]
out <- normalizePath(args[2])
.libPaths(c(normalizePath(args[3]), .libPaths()))
library(glyparse)
options(
  glyparse.threads = as.integer(Sys.getenv(
    "GLYPARSE_THREADS",
    as.character(getOption("glyparse.threads", 2L))
  ))
)
files <- list.files(file.path(out, 'data'), full.names = TRUE, pattern = 'csv$')
inputs <- list()
sources <- character()
for (file in files) {
  fmt <- sub(
    '^(glycan_sequences_|derived_)',
    '',
    sub('.csv$', '', basename(file))
  )
  p <- paste0('parse_', fmt)
  inputs[[p]] <- read.csv(file, stringsAsFactors = FALSE)[[2]]
  sources[p] <- if (startsWith(basename(file), 'derived')) {
    'derived from supplied WURCS'
  } else {
    'GlyGen 2.12.1'
  }
}
set.seed(20261021)
inputs$auto_parse <- unname(unlist(
  lapply(inputs, function(x) sample(x, min(length(x), 500))),
  use.names = FALSE
))
sources['auto_parse'] <- 'mixed corpus, <=500 per format'
# Initialize namespace-dependent vocabulary outside the warm timings.
invisible(parse_iupac_condensed('Glc'))
rows <- list()
meta <- list()
selection <- strsplit(Sys.getenv('ONLY_PARSERS'), ',', fixed = TRUE)[[1]]
for (p in if (length(selection)) selection else names(inputs)) {
  x <- inputs[[p]]
  f <- getExportedValue('glyparse', p)
  gc()
  t <- system.time(y <- suppressWarnings(f(x, on_failure = 'na')))
  cat(phase, p, 'first', length(x), t[['elapsed']], sum(!is.na(y)), '\n')
  flush.console()
  if (phase == 'baseline') {
    saveRDS(
      y,
      file.path(out, paste0(p, '-full-baseline.rds')),
      compress = FALSE
    )
  } else {
    b <- readRDS(file.path(out, paste0(p, '-full-baseline.rds')))
    stopifnot(
      identical(as.character(y), as.character(b)),
      identical(is.na(y), is.na(b))
    )
    agraphs <- attr(y, 'graphs')
    bgraphs <- attr(b, 'graphs')
    stopifnot(identical(names(agraphs), names(bgraphs)))
    for (i in seq_along(agraphs)) {
      stopifnot(
        identical(
          igraph::vertex_attr(agraphs[[i]]),
          igraph::vertex_attr(bgraphs[[i]])
        ),
        identical(
          igraph::edge_attr(agraphs[[i]]),
          igraph::edge_attr(bgraphs[[i]])
        ),
        identical(
          igraph::graph_attr(agraphs[[i]]),
          igraph::graph_attr(bgraphs[[i]])
        ),
        identical(
          igraph::as_edgelist(agraphs[[i]], names = FALSE),
          igraph::as_edgelist(bgraphs[[i]], names = FALSE)
        )
      )
    }
    cat(p, 'full graph parity OK\n')
    flush.console()
    rm(b, agraphs, bgraphs)
  }
  mask <- is.na(y)
  meta[[p]] <- data.frame(
    parser = p,
    rows = length(x),
    unique = length(unique(x)),
    successful = sum(!mask),
    failed = sum(mask),
    source = sources[p]
  )
  rm(y)
  for (i in 1:3) {
    gc()
    t <- system.time(y <- suppressWarnings(f(x, on_failure = 'na')))
    stopifnot(identical(is.na(y), mask))
    rows[[length(rows) + 1L]] <- data.frame(
      parser = p,
      iteration = i,
      rows = length(x),
      elapsed = t[['elapsed']]
    )
    cat(p, i, t[['elapsed']], '\n')
    flush.console()
    rm(y)
  }
  timings <- do.call(rbind, rows)
  oldfile <- file.path(out, paste0(phase, '-timings.csv'))
  if (length(selection) && file.exists(oldfile)) {
    old <- read.csv(oldfile)
    timings <- rbind(old[!old$parser %in% selection, ], timings)
  }
  write.csv(timings, oldfile, row.names = FALSE)
  metadata <- do.call(rbind, meta)
  oldfile <- file.path(out, paste0(phase, '-metadata.csv'))
  if (length(selection) && file.exists(oldfile)) {
    old <- read.csv(oldfile)
    metadata <- rbind(old[!old$parser %in% selection, ], metadata)
  }
  write.csv(metadata, oldfile, row.names = FALSE)
}
capture.output(
  list(native_workers = getOption("glyparse.threads", 2L)),
  sessionInfo(),
  file = file.path(out, paste0(phase, '-session.txt'))
)
