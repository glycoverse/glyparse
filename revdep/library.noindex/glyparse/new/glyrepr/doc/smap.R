## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(glyrepr)

## -----------------------------------------------------------------------------
# Our test data: some common glycan structures
iupacs <- c(
  "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-",  # N-glycan core
  "Gal(b1-3)GalNAc(a1-",                                    # O-glycan core 1
  "Gal(b1-3)[GlcNAc(b1-6)]GalNAc(a1-",                     # O-glycan core 2
  "Man(a1-3)[Man(a1-6)]Man(a1-3)[Man(a1-6)]Man(a1-",          # Branched mannose
  "GlcNAc6Ac(b1-4)Glc3Me(a1-"                              # With decorations
)

struc <- as_glycan_structure(iupacs)

# Now create a realistic dataset with lots of repetition.
large_struc <- rep(struc, 1000)  # 5,000 total structures
large_struc

## -----------------------------------------------------------------------------
# Only 5 unique graphs are stored internally
length(attr(large_struc, "graphs"))

# But we have 5,000 total elements
length(large_struc)

## -----------------------------------------------------------------------------
library(lobstr)
obj_sizes(struc, large_struc)

## -----------------------------------------------------------------------------
# This will not work and will raise an error.
tryCatch(
  purrr::map_int(large_struc, ~ igraph::vcount(.x)),
  error = function(e) cat("Error:", rlang::cnd_message(e))
)

## -----------------------------------------------------------------------------
vertex_counts <- smap_int(large_struc, ~ igraph::vcount(.x))
vertex_counts[1:10]

## -----------------------------------------------------------------------------
vertex_counts <- smap_int(large_struc, igraph::vcount)
summary(vertex_counts)

## -----------------------------------------------------------------------------
has_many_vertices <- smap_lgl(large_struc, ~ igraph::vcount(.x) > 4)
sum(has_many_vertices)

## -----------------------------------------------------------------------------
degree_sequences <- smap(large_struc, ~ igraph::degree(.x))
degree_sequences[1:3]

## -----------------------------------------------------------------------------
ssome(large_struc, ~ any(igraph::degree(.x) == 0))

## -----------------------------------------------------------------------------
severy(large_struc, ~ igraph::is_connected(.x))

## -----------------------------------------------------------------------------
# smap2: Apply function with additional parameters
thresholds <- c(3, 4, 5)
large_enough <- smap2_lgl(struc[1:3], thresholds, function(g, threshold) {
  igraph::vcount(g) >= threshold
})
large_enough

## -----------------------------------------------------------------------------
# simap: Include position information
indexed_report <- simap_chr(large_struc[1:3], function(g, i) {
  paste0("#", i, ": ", igraph::vcount(g), " vertices")
})
indexed_report

## -----------------------------------------------------------------------------
# Create a large dataset with high redundancy
huge_struc <- rep(struc, 5000)  # 25,000 structures, only 5 unique

cat("Dataset size:", length(huge_struc), "structures\n")
cat("Unique structures:", length(attr(huge_struc, "graphs")), "\n")
cat("Redundancy factor:", length(huge_struc) / length(attr(huge_struc, "graphs")), "x\n")

library(tictoc)

# Optimized approach: smap only processes 5 unique structures
tic("smap_int (optimized)")
vertex_counts_optimized <- smap_int(huge_struc, igraph::vcount)
toc()

# Naive approach: extract all graphs and process each one
tic("Naive approach (all graphs)")
all_graphs <- get_structure_graphs(huge_struc)  # Extracts all 25,000 graphs
vertex_counts_naive <- purrr::map_int(all_graphs, igraph::vcount)
toc()

# Verify results are equivalent (though data types may differ)
all.equal(vertex_counts_optimized, vertex_counts_naive)

## -----------------------------------------------------------------------------
# Calculate clustering coefficient for each structure
clustering_coeffs <- smap_dbl(large_struc, ~ igraph::transitivity(.x, type = "global"))
summary(clustering_coeffs)

## -----------------------------------------------------------------------------
# Create a compact structure summary.
structure_analysis <- smap(large_struc, function(g) {
  list(
    vertices = igraph::vcount(g),
    edges = igraph::ecount(g),
    diameter = ifelse(igraph::is_connected(g), igraph::diameter(g), NA),
    clustering = igraph::transitivity(g, type = "global")
  )
})

# Convert to a more usable format
analysis_df <- do.call(rbind, lapply(structure_analysis, data.frame))
head(analysis_df)

## -----------------------------------------------------------------------------
# Find only structures with exactly 5 vertices
has_five_vertices <- smap_lgl(large_struc, ~ igraph::vcount(.x) == 5)
five_vertex_structures <- large_struc[has_five_vertices]

cat("Found", sum(has_five_vertices), "structures with exactly 5 vertices\n")

## -----------------------------------------------------------------------------
# Custom motif detector
detect_branching <- function(g) {
  degrees <- igraph::degree(g)
  any(degrees >= 3)
}

# Apply to a large dataset using unique structure optimization.
has_branching <- smap_lgl(large_struc, detect_branching)
cat("Structures with branching:", sum(has_branching), "out of", length(large_struc), "\n")

# Use smap2 to check structures against complexity thresholds
complexity_thresholds <- rep(c(3, 4, 5, 2, 4), 1000)  # Thresholds for each structure
meets_threshold <- smap2_lgl(large_struc, complexity_thresholds, function(g, threshold) {
  igraph::vcount(g) >= threshold
})
cat("Structures meeting complexity threshold:", sum(meets_threshold), "out of", length(large_struc), "\n")

## -----------------------------------------------------------------------------
sessionInfo()

