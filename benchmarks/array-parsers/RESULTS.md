# Array parser migration results

Production commit: `952d989`; baseline: `06ca81b`. Both use glyrepr 1.0.0.9000. These measurements apply to the frozen test-derived workloads in `workloads.rds`.

## Verification

- 1,676 test expectations passed; no failures, warnings, or skips.
- R CMD check: 0 errors, 0 warnings, 1 NOTE (remote system-time verification unavailable).
- All 1,931 regression input positions across 14 entrypoints match: canonical strings, missing/failure positions, names, full graph signatures, and floating metadata.
- Every benchmark workload's output keys match the baseline, including the 500-string IUPAC and 867-string mixed automatic-parser workloads.

## End-to-end timings

Seconds; median and min–max from five runs after one warm-up. Garbage collection is outside the measured region. The retained baseline and new measurements ran sequentially after checks finished.

| Entry point / workload | Inputs | Before median (range) | After median (range) | Ratio |
|---|---:|---:|---:|---:|
| parse_pglyco_struc | 47 | 0.144 (0.143–0.147) | 0.036 (0.035–0.073) | 4.00x |
| parse_strucgp_struc | 27 | 0.063 (0.062–0.063) | 0.016 (0.016–0.017) | 3.94x |
| parse_kcf | 283 | 0.670 (0.665–0.701) | 0.523 (0.516–0.536) | 1.28x |
| parse_linucs | 153 | 2.185 (2.173–2.234) | 2.047 (1.990–2.085) | 1.07x |
| parse_glycoct | 314 | 0.530 (0.521–0.558) | 0.324 (0.323–0.400) | 1.64x |
| parse_wurcs | 43 | 0.226 (0.223–0.229) | 0.110 (0.110–0.112) | 2.05x |
| auto_parse | 867 | 5.787 (3.988–7.299) | 3.165 (3.158–3.198) | 1.83x |
| auto_parse: IUPAC only | 500 | 11.110 (11.020–11.715) | 0.200 (0.196–0.203) | 55.55x |

The large IUPAC gain applies to `auto_parse()`: it now passes the whole format group to the already-native IUPAC constructor instead of extracting and recanonicalizing one graph at a time. Direct IUPAC parsing keeps its existing entrypoint.

LINUCS improves only modestly because its R syntax/residue processing still dominates. No format-specific parser was rewritten in C++. The old mixed automatic-parser timings vary substantially (3.988–7.299 s), so its median ratio is descriptive rather than a universal throughput claim.

## Construction only

On 267 already-parsed records, the old graph pipeline takes 0.448 s (0.438–0.466); structure_from_arrays() takes 0.102 s (0.099–0.103), a 4.39x median ratio over five alternating paired runs. Both routes include their required graph materialization; format parsing is excluded.

## Compatibility

Requires the new glyrepr development API (>= 1.0.0.9000). `validate = FALSE` remains accepted but no longer bypasses mandatory array validation. IUPAC-normalizing formats retain the existing native IUPAC entrypoint. Parser-specific warnings and default failure recovery remain covered by the existing snapshots and differential checks.

See README.md for reproduction commands and artifact definitions. Raw timings, graph signatures, sessions, and checksums are retained alongside this report.
