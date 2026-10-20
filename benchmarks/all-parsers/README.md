# All structure parsers

This workload covers the 13 exported format parsers and a mixed-format `auto_parse()` batch. Seven CSVs are pinned to GlyGen release 2.12.1 with download URLs and SHA256 hashes in `sources.json`. The other six formats use up to 3,000 distinct encodings generated from real, connected, unmodified WURCS structures: IUPAC Short, Linear Code, LINUCS, KCF, pGlyco, and StrucGP. These are derived workloads, not official GlyGen downloads for those formats. Generic formats retain only the residue and topology information they can express.

Use separate optimized installed libraries for baseline and candidate. Both glyparse and glyrepr must come from the intended revision; do not time `devtools::load_all()` debug builds. CSV reading, package loading, graph comparisons, and explicit GC are outside the warm timing. Automatic GC during parsing is included. No parsed-result cache is retained between calls.

From the glyparse repository root:

```sh
python3 benchmarks/all-parsers/download.py /tmp/parser-benchmark/data
Rscript benchmarks/all-parsers/generate-corpora.R /path/glycan_sequences_wurcs.csv /tmp/parser-benchmark/data /path/baseline-lib
Rscript benchmarks/all-parsers/run.R baseline /tmp/parser-benchmark /path/baseline-lib
Rscript benchmarks/all-parsers/run.R candidate /tmp/parser-benchmark /path/candidate-lib
```

The same generated CSVs must be used for both revisions. The runner records rows, distinct inputs, successes, failures, source type, three full-batch elapsed times, and session information. Candidate runs require exact agreement with the saved baseline for canonical strings, missing positions, graph keys, vertex/edge/graph attributes, and edge topology. `ONLY_PARSERS=parse_kcf,auto_parse` can restrict a rerun while retaining other results. Warm average time per record means elapsed batch time divided by every input row; it is not scalar-call latency. Always inspect failure counts alongside throughput.

The 2026-10-21 comparison starts from glyparse `1830759` and glyrepr `ed3a98f`, the preceding WURCS/GlycoCT optimization. Results and a machine/environment-specific report are in `/tmp/glyparse-all/report.md` on the development machine. The original user-supplied WURCS file was used to create the derived corpus. Baseline snapshots, downloaded data, and generated data are not committed.

Use `Rscript benchmarks/all-parsers/fresh.R parse_kcf /tmp/parser-benchmark /path/candidate-lib` in a new R process for each parser to append the first-call measurement to `fresh.csv` and three successful-input-only measurements to `successful.csv`. First-call timing includes lazy parser/vocabulary initialization but excludes `library()` and CSV I/O. Do not run other tests or builds concurrently with measured calls.
