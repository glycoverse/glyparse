## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(glyrepr)

## -----------------------------------------------------------------------------
glycan <- n_glycan_core()
graph <- get_structure_graphs(glycan)
graph

## -----------------------------------------------------------------------------
plot(graph)

## -----------------------------------------------------------------------------
igraph::V(graph)$name

## -----------------------------------------------------------------------------
igraph::V(graph)$mono

## -----------------------------------------------------------------------------
igraph::V(graph)$sub

## -----------------------------------------------------------------------------
glycan2 <- as_glycan_structure("Glc3Me6S(a1-")
graph2 <- get_structure_graphs(glycan2)
igraph::V(graph2)$sub

## -----------------------------------------------------------------------------
glycan3 <- as_glycan_structure("Gal(a1-3)GalNAc(b1-")
graph3 <- get_structure_graphs(glycan3)
igraph::E(graph3)$linkage

## -----------------------------------------------------------------------------
graph$anomer

## -----------------------------------------------------------------------------
alditol <- as_glycan_structure("Gal(b1-4)GlcNAc-ol(a1-")
alditol_graph <- get_structure_graphs(alditol)
alditol_graph$alditol
get_alditol(alditol)

## -----------------------------------------------------------------------------
floating <- as_glycan_structure(
  "{Neu5Ac(a2-3)|2,3}Gal(b1-3)[Gal(b1-4)]GlcNAc(a1-"
)
floating_graph <- get_structure_graphs(floating)

igraph::components(floating_graph, mode = "weak")$no
igraph::graph_attr(floating_graph, "floating_parts")
structure_floating_parts(floating)

## -----------------------------------------------------------------------------
floating_sub <- as_glycan_structure(
  "{6S|1,2}Gal(a1-3)Glc(a1-3)Man(a1-"
)
floating_sub_graph <- get_structure_graphs(floating_sub)
igraph::graph_attr(floating_sub_graph, "floating_substituents")
structure_floating_substituents(floating_sub)

## -----------------------------------------------------------------------------
sum(igraph::degree(graph, mode = "out") > 1)

## -----------------------------------------------------------------------------
bfs_result <- igraph::bfs(graph, root = 1, mode = "out")
bfs_result$order

## -----------------------------------------------------------------------------
library(purrr)

glycans <- c(n_glycan_core(), o_glycan_core_1(), o_glycan_core_2())
graphs <- get_structure_graphs(glycans)  # Extract graphs first
map_int(graphs, ~ igraph::vcount(.x))    # Then analyze

## -----------------------------------------------------------------------------
smap_int(glycans, ~ igraph::vcount(.x))  # Direct analysis, no intermediate step.

