# Array parser migration

The six direct parsers (pGlyco, StrucGP, KCF, LINUCS, GlycoCT, WURCS) produce residue/edge records and call `glyrepr::structure_from_arrays()` once per unique-input batch. Floating components, candidate parents, substituents, and reducing-end properties are constructed in R arrays. No igraph construction or graph canonicalization remains in glyparse's production R code.

IUPAC and formats normalized to IUPAC keep `as_glycan_structure()`, using glyrepr's existing native IUPAC backend. `auto_parse()` groups unique inputs by format and calls the vectorized public parser once per group. Detection precedence is unchanged.

`validate` remains accepted by the six parsers for source compatibility, but arrays are always validated by glyrepr, even for `validate = FALSE`. The minimum glyrepr version is now 1.0.0.9000; this development version must include the new exported array API. The format-specific syntax parsers remain R code.

## Frozen comparison

- Baseline: glyparse `06ca81b` (before this migration), exported with `git archive`.
- Both revisions use the same installed glyrepr 1.0.0.9000 and igraph; the adjacent glyrepr checkout is `0fe266a`.
- `inputs.rds`: 1,875 distinct-per-entrypoint input values captured from evaluated baseline tests, including dynamically assembled WURCS/GlycoCT records. The capture function also observes test calls through exported parsers; it is a regression corpus, not a sampled biological population.
- `reference.rds` and `current.rds`: named canonical keys and full graph signatures (vertex/edge/graph attributes, including floating metadata), plus session metadata. Each parser's inputs add duplicates and a missing position.
- `parity.csv`: all 1,931 positions and graph signatures agree across 14 entrypoints; failed and missing positions agree too.
- `workloads.rds`: frozen end-to-end workloads. Six families use valid regression inputs; pGlyco and StrucGP also include deterministic synthetic chains because their original fixtures are small. `auto_parse` uses the union of these six workloads. `auto_iupac` uses the first 500 distinct non-missing strings in glyrepr's frozen compact-IUPAC corpus. Workload counts and valid-output counts are reported separately.
- `timing-before.rds` and `timing-after.rds`: one warm-up and five timed runs per workload, garbage collection outside the measured region, same machine and R library. The retained runs execute sequentially after package checks complete. These are test-derived workloads, not general throughput claims.
- `backend-results.rds`: paired, alternating measurements after format parsing. The legacy route constructs graphs then validates, canonicalizes, serializes, deduplicates and constructs the vector. The new route calls `structure_from_arrays()`. This excludes format parsing and is not the end-to-end speedup.
- `hashes.csv`: MD5 checksums of the frozen RDS artifacts.
- `check.log`: R CMD check output. Full tests pass; the sole NOTE is unavailable remote system-time verification.

## Reproduce

Run commands from the glyparse package root. Keep the same glyrepr installation for both revisions.

```sh
baseline=$(mktemp -d /tmp/glyparse-baseline.XXXXXX)
git archive 06ca81b | tar -x -C "$baseline"
Rscript benchmarks/array-parsers/capture-inputs.R "$baseline" benchmarks/array-parsers/inputs.rds
Rscript benchmarks/array-parsers/run.R "$baseline" benchmarks/array-parsers/reference.rds
Rscript benchmarks/array-parsers/run.R . benchmarks/array-parsers/current.rds
Rscript benchmarks/array-parsers/verify.R
Rscript benchmarks/array-parsers/benchmark.R "$baseline" benchmarks/array-parsers/timing-before.rds
Rscript benchmarks/array-parsers/benchmark.R . benchmarks/array-parsers/timing-after.rds
Rscript benchmarks/array-parsers/backend.R
Rscript benchmarks/array-parsers/summarize.R
```

Retain `workloads.rds` to rerun without the sibling glyrepr benchmark corpus. Delete it only to deliberately regenerate the workload selection.
