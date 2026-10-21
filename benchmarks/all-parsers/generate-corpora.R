args <- commandArgs(TRUE)
stopifnot(length(args) == 3L)
.libPaths(c(normalizePath(args[3]), .libPaths()))
library(glyparse)
input <- read.csv(args[1], stringsAsFactors = FALSE)
output <- args[2]
dir.create(output, recursive = TRUE, showWarnings = FALSE)
source <- suppressWarnings(parse_wurcs(input[[2]], on_failure = "na"))
graphs <- attr(source, 'graphs')
set.seed(20261021)
keys <- sample(names(graphs), min(12000, length(graphs)))
acc <- input[[1]]
ids <- acc[match(keys, as.character(source))]
formats <- c(
  'iupac_short',
  'linear_code',
  'linucs',
  'kcf',
  'pglyco_struc',
  'strucgp_struc'
)
result <- setNames(lapply(formats, function(f) character()), formats)
source_ids <- result
lc <- c(
  Glc = 'G',
  Gal = 'A',
  Man = 'M',
  GlcNAc = 'GN',
  GalNAc = 'AN',
  Fuc = 'F',
  Neu5Ac = 'NN',
  Neu = 'N',
  Kdn = 'K',
  Kdo = 'W',
  GalA = 'L',
  Ido = 'I',
  Rha = 'H',
  Xyl = 'X',
  Rib = 'B',
  Ara = 'R',
  GlcA = 'U',
  All = 'O',
  Api = 'P',
  Fru = 'E'
)
lin <- c(
  Glc = 'D-Glcp',
  Gal = 'D-Galp',
  Man = 'D-Manp',
  GlcNAc = 'D-GlcpNAc',
  GalNAc = 'D-GalpNAc',
  Fuc = 'L-Fucp',
  Neu5Ac = 'Neup5Ac',
  Neu5Gc = 'Neup5Gc',
  Xyl = 'D-Xylp'
)
generic <- c(
  Glc = 'H',
  Gal = 'H',
  Man = 'H',
  Hex = 'H',
  GlcNAc = 'N',
  GalNAc = 'N',
  HexNAc = 'N',
  Fuc = 'F',
  dHex = 'F',
  Neu5Ac = 'A',
  NeuAc = 'A',
  Neu5Gc = 'G',
  NeuGc = 'G'
)
for (k in seq_along(keys)) {
  g <- graphs[[keys[k]]]
  ga <- igraph::graph_attr(g)
  v <- igraph::vertex_attr(g)
  e <- igraph::as_edgelist(g, names = FALSE)
  links <- igraph::edge_attr(g, 'linkage')
  n <- length(v$mono)
  if (
    length(ga$floating_parts) ||
      length(ga$floating_substituents) ||
      isTRUE(ga$alditol) ||
      any(nzchar(v$sub)) ||
      n > 40
  ) {
    next
  }
  root <- setdiff(seq_len(n), e[, 2])
  if (length(root) != 1) {
    next
  }
  children <- lapply(seq_len(n), function(i) e[e[, 1] == i, 2])
  incoming <- match(seq_len(n), e[, 2])
  a <- rep(substr(ga$anomer, 1, 1), n)
  d <- rep(substr(ga$anomer, 2, 2), n)
  pos <- rep('', n)
  a[-root] <- substr(links[incoming[-root]], 1, 1)
  d[-root] <- substr(links[incoming[-root]], 2, 2)
  pos[-root] <- substring(links[incoming[-root]], 4)
  emit <- function(i, label, branch = c('(', ')'), suffix = function(i) '') {
    arms <- vapply(
      children[[i]],
      function(j) emit(j, label, branch, suffix),
      character(1)
    )
    paste0(
      if (length(arms)) {
        paste0(
          arms[1],
          if (length(arms) > 1) {
            paste0(branch[1], arms[-1], branch[2], collapse = '')
          } else {
            ''
          }
        )
      } else {
        ''
      },
      label[i],
      suffix(i)
    )
  }
  add <- function(f, s) {
    result[[f]] <<- c(result[[f]], s)
    source_ids[[f]] <<- c(source_ids[[f]], ids[k])
  }
  if (all(d %in% c('1', '2'))) {
    s <- emit(root, v$mono, c('(', ')'), function(i) {
      paste0(a[i], if (i == root) '-' else pos[i])
    })
    add('iupac_short', s)
  }
  if (all(v$mono %in% names(lc)) && all(nchar(pos[-root]) == 1)) {
    add(
      'linear_code',
      emit(root, unname(lc[v$mono]), suffix = function(i) paste0(a[i], pos[i]))
    )
  }
  if (
    all(v$mono %in% names(lin)) &&
      all(d %in% c('1', '2', '?')) &&
      all(nchar(pos[-root]) == 1)
  ) {
    rec <- function(i) {
      paste0(
        if (i == root) '[]' else paste0('[(', pos[i], '+', d[i], ')]'),
        '[',
        a[i],
        '-',
        lin[v$mono[i]],
        ']{',
        paste0(vapply(children[[i]], rec, character(1)), collapse = ''),
        '}'
      )
    }
    add('linucs', rec(root))
  }
  if (all(nchar(pos[-root]) == 1)) {
    nodes <- paste(seq_len(n), v$mono, 0, 0)
    # A terminal aglycon carries the original reducing-end anomer in KCF.
    nodes <- c(nodes, paste(n + 1, 'R', 0, 0))
    edge_lines <- if (nrow(e)) {
      paste(
        seq_len(nrow(e)),
        paste0(e[, 2], ':', a[e[, 2]], d[e[, 2]]),
        paste0(e[, 1], ':', pos[e[, 2]])
      )
    } else {
      character()
    }
    edge_lines <- c(
      edge_lines,
      paste(length(edge_lines) + 1, paste0(root, ':', a[root], d[root]), n + 1)
    )
    add(
      'kcf',
      paste(
        c(
          'ENTRY benchmark Glycan',
          paste('NODE', n + 1),
          nodes,
          paste('EDGE', length(edge_lines)),
          edge_lines,
          '///'
        ),
        collapse = '\n'
      )
    )
  }
  if (all(v$mono %in% names(generic))) {
    codes <- unname(generic[v$mono])
    rec <- function(i) {
      paste0(
        '(',
        codes[i],
        paste0(vapply(children[[i]], rec, character(1)), collapse = ''),
        ')'
      )
    }
    add('pglyco_struc', rec(root))
    nums <- c(H = 1, N = 2, A = 3, G = 4, F = 5)[codes]
    rec <- function(i, depth = 1L) {
      if (depth > 26) {
        stop('depth')
      }
      paste0(
        LETTERS[depth],
        nums[i],
        paste0(
          vapply(children[[i]], function(j) rec(j, depth + 1L), character(1)),
          collapse = ''
        ),
        letters[depth]
      )
    }
    if (max(igraph::distances(g, v = root, mode = 'out')) < 26) {
      add('strucgp_struc', rec(root))
    }
  }
}
for (f in formats) {
  keep <- !duplicated(result[[f]])
  x <- data.frame(
    glytoucan_ac = source_ids[[f]][keep],
    sequence = result[[f]][keep]
  )
  if (nrow(x) > 3000) {
    x <- x[sample.int(nrow(x), 3000), ]
  }
  write.csv(
    x,
    file.path(output, paste0('derived_', f, '.csv')),
    row.names = FALSE
  )
  cat(f, nrow(x), 'unique derived inputs\n')
}
