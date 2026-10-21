## Complete before/after parser benchmark

Optimized installed builds, identical inputs, three warm full-batch repetitions. Values are arithmetic mean elapsed milliseconds per input row, including failed inputs. The candidate uses the default two decoding workers. CSV I/O, package loading and explicit GC are excluded; automatic GC, validation and canonical graph construction are included.

**Baseline:** glyparse `1830759` + glyrepr `ed3a98f`, after the first WURCS/GlycoCT optimization and before the all-parser optimization. **Candidate:** glyparse `a3be5ad` + glyrepr `c99d5e9` (subsequent packaging/namespace qualification/NEWS changes leave the algorithm unchanged). Thus the table measures the additional all-parser optimization; it does not attribute the earlier WURCS/GlycoCT speedup to this comparison.

| Parser | Input rows | Successful / failed | Before (ms/row) | After (ms/row) | Speedup | After < 0.01 ms |
|---|---:|---:|---:|---:|---:|:---:|
| `parse_iupac_short()` | 3,000 | 3,000 / 0 | 0.36689 | 0.00678 | 54.13× | Yes |
| `parse_linear_code()` | 3,000 | 3,000 / 0 | 0.29533 | 0.00644 | 45.83× | Yes |
| `parse_linucs()` | 3,000 | 3,000 / 0 | 0.06578 | 0.00833 | 7.89× | Yes |
| `parse_pglyco_struc()` | 2,841 | 2,841 / 0 | 0.02124 | 0.00939 | 2.26× | Yes |
| `parse_strucgp_struc()` | 2,841 | 2,841 / 0 | 0.02100 | 0.00950 | 2.21× | Yes |
| `parse_glycam_iupac()` | 7,481 | 7,481 / 0 | 0.28993 | 0.00628 | 46.15× | Yes |
| `parse_glycoct()` | 59,847 | 51,426 / 8,421 | 0.04800 | 0.01954 | 2.46× | No |
| `parse_gwb()` | 51,507 | 50,220 / 1,287 | 0.34998 | 0.01405 | 24.91× | No |
| `parse_iupac_compact()` | 24,531 | 24,531 / 0 | 0.53222 | 0.00795 | 66.95× | Yes |
| `parse_iupac_condensed()` | 19,933 | 19,933 / 0 | 0.28360 | 0.00579 | 49.01× | Yes |
| `parse_iupac_extended()` | 20,407 | 20,406 / 1 | 0.38575 | 0.01691 | 22.82× | No |
| `parse_wurcs()` | 59,994 | 49,728 / 10,266 | 0.04603 | 0.01813 | 2.54× | No |
| `parse_kcf()` | 3,000 | 3,000 / 0 | 0.09222 | 0.01033 | 8.92× | No |
| `auto_parse()` | 6,500 | 6,339 / 161 | 0.25518 | 0.02985 | 8.55× | No |

Seven official GlyGen 2.12.1 formats use the pinned full CSVs, including the supplied WURCS and GlycoCT files. Six formats use up to 3,000 distinct encodings derived from real WURCS structures. `auto_parse()` uses 6,500 mixed-format inputs. All canonical outputs, missing positions, graph attributes and topology match the baseline. The requested 0.01 ms target is not fully achieved; 8/13 format parsers meet it at the default worker limit.

[Full report, including eight-worker, median, maximum and fresh-process measurements](report.md). Raw before/after timings and input hashes are committed alongside that report.
