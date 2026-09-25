# Native parser migration

Format-specific string processing uses native C++17 where it improves performance. Linear Code retains the baseline R implementation, which is faster on its frozen workload. The R layer validates arguments, deduplicates inputs, groups auto-detected formats, restores names and positions, and reports failures/warnings. The native parsers have no R fallback or per-residue callback into R.

- IUPAC-short, extended, compact, GlyCAM IUPAC and GlycoWorkbench are converted to condensed IUPAC in `src/converters.cpp` / `src/gwb.cpp`, then passed to glyrepr's existing native IUPAC backend. Condensed IUPAC itself continues to use that backend.
- pGlyco, StrucGP, KCF, LINUCS, WURCS and GlycoCT produce arrays in C++, then call `glyrepr::structure_from_arrays()` in one batch. Topology, substituents, ring/configuration, alditols and floating attachments retain their existing semantics. Arrays remain validated regardless of the legacy `validate` flag.
- `auto_parse()` uses native format detection with the same precedence.
- Dictionaries are compiled into `native-tables.h`; `export-tables.R` and `generate-tables.py` reproduce them from the frozen R implementation. The live glyrepr vocabulary supplies supported names and anomer positions. Regexes are cached; WURCS descriptors are cached within each native batch. Progress mode processes chunks of 128 unique inputs.
- Invalid elements are isolated inside each native batch. User interrupts are checked between groups of 128 inputs. The recursive StrucGP, LINUCS and GWB readers cap nesting at 1,024 levels to protect the native stack.
- Compatibility includes the old permissive token recovery and ring-bound comparisons. This migration does not redefine accepted syntax. Private R helpers for migrated parsers were removed; old helper-level tests now probe the corresponding native primitives.

## Validation

Candidate: `18edb58` (native parsing with factored vocabulary patterns; Linear Code retains R). Baseline: `1cc34cd`, the completed array-backend migration with R format parsers. Both builds use the same installed glyrepr and R libraries.

`reference.rds` / `current.rds` contain canonical keys, names, NA positions and complete graph signatures for 1,931 positions across 14 public entrypoints, using `../array-parsers/inputs.rds`. All agree (see `parity.csv`). `mutations.rds` contains another 959 deterministic test-derived perturbations; `mutations-before.rds` and `mutations-after.rds` agree exactly. These are regression corpora, not samples of biological glycans.

All 1,688 package expectations pass. The package tests also cover every mapped residue, furanose and unusual configuration, floating components/substituents, alditols, cyclic/disconnected input, missing/duplicated/named inputs, strict failure mode, and progress chunks. `check.log` records the final package check: 0 errors, 0 warnings, and 2 environment notes (remote clock verification and an `xcrun_db` temporary file).

## Performance methodology

`workloads.rds` is frozen from the baseline: valid test-derived inputs for all 13 parser families, synthetic pGlyco/StrucGP chains, their mixed-format union, and the earlier 500-IUPAC cohort. Builds are installed into separate temporary libraries. The candidate is compiled with normal `-O2` flags, not pkgload's debug `-O0` build. Both use the same dependency libraries. Source hashes are recorded in `source-hashes.csv`.

`compare.R` starts a separate R process per build and round. Each workload has one warm-up, followed by three timed complete public-parser calls (including format processing and glyrepr materialization); garbage collection is outside the timed region. Five rounds alternate build order. Timing is sequential with no package tests or compilation running concurrently. Canonical outputs from every round must agree before `summarize.R` writes ranges, medians and speedups. These are warm, workload-specific end-to-end measurements, not universal speedup claims; the unchanged condensed-IUPAC and Linear Code paths are controls.

## Reproduce

Run from the package root; use the same glyrepr installation for both builds.

```sh
mkdir -p /tmp/glyparse-r-baseline /tmp/glyparse-r-lib /tmp/glyparse-native-lib
git archive 1cc34cd | tar -x -C /tmp/glyparse-r-baseline
R CMD INSTALL -l /tmp/glyparse-r-lib /tmp/glyparse-r-baseline
R CMD INSTALL --preclean --clean -l /tmp/glyparse-native-lib .
Rscript benchmarks/array-parsers/run.R /tmp/glyparse-r-baseline benchmarks/native-parsers/reference.rds
Rscript benchmarks/array-parsers/run.R . benchmarks/native-parsers/current.rds
Rscript benchmarks/native-parsers/mutations.R /tmp/glyparse-r-baseline benchmarks/native-parsers/mutations-before.rds
Rscript benchmarks/native-parsers/mutations.R . benchmarks/native-parsers/mutations-after.rds
Rscript benchmarks/native-parsers/verify.R
Rscript benchmarks/native-parsers/compare.R /tmp/glyparse-r-lib /tmp/glyparse-native-lib
Rscript benchmarks/native-parsers/summarize.R
Rscript benchmarks/native-parsers/report.R
```

Do not run `pkgload::load_all()` against the same source directory while building or installing it. For an entirely separate optimized build, copy the final source to a temporary directory before installation. The retained timings used this isolation.
