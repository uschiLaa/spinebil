## R CMD check results

── R CMD check results ──────────────── spinebil 1.0.5 ────
Duration: 1m 11.2s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔


## Resubmission
This update fixes reverse dependency check issues:
- Replaced `detectCores()` with the available cores
- Made `future::plan()` calls temporary
- Added BugReports field
No functional changes.