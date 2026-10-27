## Preparation checks (0.8.1.9000, 2026-10-27)

R 4.6.1 on macOS arm64, using released glyrepr 1.2.0:
`devtools::check(remote = TRUE, manual = TRUE)` completed with
0 errors, 0 warnings, and 1 NOTE for the development version number.
The final 1.0.0 candidate will be checked separately before submission.

All tests also passed with the minimum required released glyrepr 1.1.0.
All 15 URLs passed urlchecker; README and PDF/HTML manuals were rebuilt.

## Reverse dependency checks

CRAN: checked glydraw 0.9.0 and glymotif 1.0.1 against CRAN and development
glyparse; 0 new problems, 0 failed checks.

Glycoverse: checked 9 direct reverse dependencies against glyparse 0.8.1
and preparation commit 797cac7; 0 new problems after isolated rechecks.
Initial glyread/glyrex checks had installation-directory artifacts; fresh
checks in /tmp passed with 0 errors, warnings, or notes on both sides.

## Windows-devel

Preparation version 0.8.1.9000 uploaded once. Results email pending.
No CRAN submission has been made for this release.
