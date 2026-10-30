## Preparation checks

R 4.6.1 on macOS arm64, with the minimum required glyrepr 1.2.0.
The repaired development candidate (0.8.1.9000) completed
`devtools::check(remote = TRUE, manual = TRUE)` with 0 errors, 0 warnings,
and 1 NOTE solely for the development version number.
All 15 URLs passed urlchecker; README and PDF/HTML manuals were rebuilt.
The final release archive will be checked before submission.

## CRAN reverse dependencies

On 2026-10-30, used the CRAN-recommended
`tools::check_packages_in_dir()` with CRAN-only reverse dependencies and
`tools::check_packages_in_dir_changes()` to compare glyparse 0.8.1 with
the repaired candidate. Coverage was determined with
`tools::package_dependencies(reverse = TRUE, which = "most", recursive = "strong")`.

Both CRAN reverse dependencies, glydraw 0.9.0 and glymotif 1.0.1, passed
with status OK against both versions. No check-status regressions were found.

## Windows-devel

The current repaired candidate was accepted for Windows-devel checking on
2026-10-28. No result email was received; the maintainer explicitly waived
waiting for this result on 2026-10-30. No Windows-devel pass is claimed.
