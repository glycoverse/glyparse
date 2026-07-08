## R CMD check results

0 errors | 0 warnings | 0 note

## Rationality

This is an urgent patch release fixing a regression introduced in 0.7.0.
Parser outputs were constructed with an incompatible vctrs representation,
causing some operations on parsed glycan structures to fail with glyrepr 0.13.0.
No API changes are included. Thanks.
