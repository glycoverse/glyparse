# Parser performance: 2026-10-21

The GlycoCT regression is resolved on this workload. The original goal of every parser below 0.01 ms per record is **not fully achieved**. The user accepted the measured improvements and requested closeout. With the default two decoding workers, 8 of 13 format parsers have a warm arithmetic mean below 0.01 ms/record; with eight workers, 10 of 13 do. `auto_parse()` remains above the target.

The complete [before/after comparison table](comparison.md) reports all 13 format parsers and automatic detection.

## Revisions and workload

- Candidate: glyparse `a3be5ad` with glyrepr `c99d5e9`, optimized installed builds. Subsequent namespace qualification (`stats::na.omit`), packaging and documentation changes leave the measured algorithm unchanged.
- Previous all-parser pair: glyparse `c45c0ad` with glyrepr `22f5efa`. Earlier WURCS/GlycoCT pair: glyparse `1830759` with glyrepr `ed3a98f`.
- R 4.6.1, aarch64 macOS 26.5.2, igraph 2.3.4; complete session information is included beside this report.
- Seven official formats are pinned to [GlyGen 2.12.1](https://data.glygen.org/ln2data/releases/data/v-2.12.1/reviewed/); URLs and published-file SHA256 values are in [sources.json](../sources.json). Six formats are derived from real, connected, unmodified WURCS glycans, with up to 3,000 distinct encodings per format. They are not official downloads for those six formats.
- The user-supplied WURCS and GlycoCT files have the same SHA256 values as the pinned official copies. The 13 corpora contain 261,382 rows. Mixed-format automatic detection adds 6,500 rows, sampled with seed 20261021. Input hashes, rows and unique counts are in [input-manifest.json](input-manifest.json).

## Complete public parser timings

Each warm value uses three full-batch calls and divides elapsed time by **all input rows**, including failed records. The mean is the arithmetic mean, not the median. CSV I/O, package loading, explicit pre-call GC and parity checks are outside timing; automatic GC and complete validation/canonical graph construction are inside. No parsed input/result cache persists between calls. Only decoding is threaded; all R access and graph assembly stay on the calling thread. First-call timing is from a separate R process for each parser and includes lazy initialization. These measurements describe batch throughput, not individual scalar-call latency or a maximum-latency guarantee.

All values below are **ms per input record**.

| Parser | Rows | Success / failure | Mean, 2 workers | Median | Maximum | Mean, 8 workers | Fresh process, 2 workers |
|---|---:|---:|---:|---:|---:|---:|---:|
| parse_iupac_short | 3000 | 3000 / 0 | 0.00678 | 0.00667 | 0.00700 | 0.00644 | 0.07700 |
| parse_linear_code | 3000 | 3000 / 0 | 0.00644 | 0.00633 | 0.00667 | 0.00622 | 0.06700 |
| parse_linucs | 3000 | 3000 / 0 | 0.00833 | 0.00833 | 0.00833 | 0.00789 | 0.07333 |
| parse_pglyco_struc | 2841 | 2841 / 0 | 0.00939 | 0.00950 | 0.00950 | 0.00927 | 0.07673 |
| parse_strucgp_struc | 2841 | 2841 / 0 | 0.00950 | 0.00950 | 0.00950 | 0.00950 | 0.07709 |
| parse_glycam_iupac | 7481 | 7481 / 0 | 0.00628 | 0.00628 | 0.00642 | 0.00575 | 0.03195 |
| parse_glycoct | 59847 | 51426 / 8421 | 0.01954 | 0.01940 | 0.01992 | 0.01502 | 0.02755 |
| parse_gwb | 51507 | 50220 / 1287 | 0.01405 | 0.01365 | 0.01514 | 0.01302 | 0.01967 |
| parse_iupac_compact | 24531 | 24531 / 0 | 0.00795 | 0.00791 | 0.00807 | 0.00697 | 0.02030 |
| parse_iupac_condensed | 19933 | 19933 / 0 | 0.00579 | 0.00577 | 0.00582 | 0.00587 | 0.01716 |
| parse_iupac_extended | 20407 | 20406 / 1 | 0.01691 | 0.01681 | 0.01715 | 0.00889 | 0.03019 |
| parse_wurcs | 59994 | 49728 / 10266 | 0.01813 | 0.01789 | 0.01912 | 0.01570 | 0.02947 |
| parse_kcf | 3000 | 3000 / 0 | 0.01033 | 0.01033 | 0.01033 | 0.00878 | 0.09433 |
| auto_parse | 6500 | 6339 / 161 | 0.02985 | 0.02985 | 0.03046 | 0.02523 | 0.06738 |

The accompanying [summary.csv](summary.csv) also reports eight-worker medians/maxima and successful-input-only means. Raw timings, success-only repetitions, fresh-process timings and metadata are retained in this directory. Success-only runs exclude unsupported inputs instead of treating rapid failures as successful throughput.

## GlycoCT regression comparison

The preceding report showed 0.05183 ms/record versus 0.04692 ms/record in the earlier pair. A controlled five-run remeasurement at the start of this pass gave the preceding pair a median of 2.655 s (0.04436 ms/record), so that apparent regression was not stable under the same conditions. The final default-worker mean is 1.16933 s (0.01954 ms/record), about 2.27 times faster than that controlled pre-change median. All 59,847 inputs retain the same 51,426 successes and 8,421 failures. Eight-worker mean is 0.01502 ms/record. The 0.01 ms target remains unmet for GlycoCT.

## Correctness and verification

- All 267,882 public-parser outputs match the frozen baseline at both worker limits: canonical strings, missing positions, graph-key order, vertex/edge/graph attributes and edge topology.
- Native GlycoCT/WURCS records, failure reasons, warning flags and descriptor counts match for all 119,841 inputs. Another 23,121 converter/direct-parser mutations and 8,737 pGlyco/StrucGP variants match the reference.
- Installed-package regression suites pass, including forced garbage collection, graph serialization, mutation isolation, serial/parallel parity, invalid-topology validation and older-constructor fallbacks.
- Full local `R CMD check --no-manual` (including examples and vignettes) passed for both packages with 0 errors, 0 warnings and 0 notes.
- Package checks and CI are recorded in [glyparse #55](https://github.com/glycoverse/glyparse/pull/55) and [glyrepr #104](https://github.com/glycoverse/glyrepr/pull/104); this benchmark is not a claim that every parser met the requested target.

## Reproduction

Follow [the benchmark instructions](../README.md), installing each package pair into its own library. Use the exact CSV hashes and the same corpus for both revisions. Set `GLYPARSE_THREADS=8` only for the explicit eight-worker comparison; the default is two. Run builds/tests separately from timed calls. Native records are checked before graph assembly, and unsupported syntax continues through the public reference fallback.
