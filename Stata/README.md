# Stata companion scripts

These `.do` files are Stata companions to the R pipeline in `R/00_setup.R` through `R/06_tables_figures.R`.

## Goal
Provide a clear, step-by-step Stata version so coauthors can track the logic and replicate core outputs.

## Files
- `00_setup.do`: project paths, folders, and package checks.
- `01_simulate_data.do`: simulate firm-year panel, waiver requests, and supplier outcomes.
- `02_build_metrics.do`: create lag-based churn/Jaccard-style metrics and exposure controls.
- `03_models.do`: TWFE, static DiD, and event-study style regressions.
- `04_network_spillovers.do`: spillover-control regressions and community-cluster variant.
- `05_robustness.do`: alternative outcomes/treatments and placebo logic.
- `06_tables_figures.do`: summary statistics and figures.
- `run_all.do`: execute scripts in order.

## Notes
1. Some R-specific methods (e.g., `sunab`, Callaway-Sant'Anna `did`, MRQAP) are mapped to close Stata analogs (`eventstudyinteract`/relative-time FE, `csdid`, and placeholder diagnostics).
2. Install user packages as needed: `reghdfe`, `ftools`, `boottest`, `csdid`, `coefplot`, `estout`.
3. Scripts save Stata outputs in `data/` and `output/` using `.dta`, `.csv`, `.tex`, and `.png` where appropriate.
