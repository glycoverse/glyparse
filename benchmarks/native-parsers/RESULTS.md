# Native parsing results

Warm end-to-end measurements against the R-parser revision `1cc34cd`, using identical glyrepr and dependency installations. Five alternating rounds, three calls per measured workload; values below are seconds per complete vector call. The ranges reflect the five retained rounds, not confidence intervals.

| Parser | Inputs | R median (range) | Native median (range) | Speedup |
|---|---:|---:|---:|---:|
| `auto_parse` | 1878 | 5.6453 (4.8070–8.6883) | 0.9510 (0.8960–1.1723) | 5.94× |
| `parse_iupac_condensed` | 222 | 0.0503 (0.0460–0.0560) | 0.0527 (0.0507–0.0603) | 0.96× |
| `parse_iupac_short` | 164 | 0.0630 (0.0510–0.0757) | 0.0433 (0.0383–0.0553) | 1.45× |
| `parse_iupac_extended` | 182 | 0.1557 (0.1213–0.2027) | 0.0463 (0.0440–0.0813) | 3.36× |
| `parse_pglyco_struc` | 47 | 0.0640 (0.0543–0.0843) | 0.0243 (0.0237–0.0937) | 2.63× |
| `parse_strucgp_struc` | 27 | 0.0273 (0.0210–0.0347) | 0.0140 (0.0117–0.0260) | 1.95× |
| `parse_linear_code` | 32 | 0.0183 (0.0167–0.0243) | 0.0183 (0.0170–0.0297) | 1.00× |
| `parse_linucs` | 153 | 3.2107 (2.6413–3.5823) | 0.0697 (0.0613–0.0940) | 46.09× |
| `parse_glycam_iupac` | 239 | 0.1330 (0.1060–0.1340) | 0.0613 (0.0563–0.0930) | 2.17× |
| `parse_iupac_compact` | 159 | 0.0510 (0.0453–0.0557) | 0.0403 (0.0397–0.0577) | 1.26× |
| `parse_kcf` | 283 | 0.8120 (0.6903–1.6057) | 0.1203 (0.1127–0.1620) | 6.75× |
| `parse_wurcs` | 43 | 0.1690 (0.1527–0.2463) | 0.0493 (0.0463–0.0633) | 3.43× |
| `parse_glycoct` | 314 | 0.5033 (0.4617–0.5463) | 0.2053 (0.1850–0.2857) | 2.45× |
| `parse_gwb` | 18 | 0.0347 (0.0340–0.0357) | 0.0067 (0.0063–0.0087) | 5.20× |
| `auto_iupac` | 500 | 0.2803 (0.2707–0.3350) | 0.1940 (0.1773–0.2390) | 1.45× |

All timed canonical outputs agree across builds and rounds. Separately, 1,931 named positions and complete graph signatures agree across 14 public entrypoints, as do 959 additional perturbed inputs. Condensed IUPAC keeps glyrepr's native parser; Linear Code retains the faster baseline R implementation. These unchanged paths serve as controls. See README.md for corpus selection, build isolation, limitations and reproduction commands.
