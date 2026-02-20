# Tariff Waivers × Supplier Networks — R Skeleton (Simulation + Analysis)

This folder contains an **end-to-end, runnable** R scaffold you can adapt once your FactSet + USTR waiver data are fully assembled.

## What you get
- A **simulation** that mimics a firm–supplier bipartite panel with:
  - staggered waiver decisions,
  - partial waiver "coverage" (intensity),
  - comment-based "political sensitivity",
  - **network spillovers** via shared suppliers.
- Functions to compute:
  - Jaccard similarity (t vs t-1),
  - retention / add / drop rates,
  - baseline similarity (t vs pre-policy baseline),
  - degree controls.
- Multiple estimation approaches:
  - TWFE DiD,
  - event-study for staggered adoption (fixest::sunab),
  - Callaway–Sant'Anna DID (did package),
  - DDD extension with sensitivity,
  - spillover / exposure mapping controls.
- Robustness suite + tables/figures:
  - texreg regression tables,
  - summary statistics,
  - network plots + heatmaps.

## Folder structure
- `R/00_setup.R` — installs/loads packages + global options
- `R/01_simulate_data.R` — generates simulated edges + firm-year panel
- `R/02_build_metrics.R` — computes Jaccard + churn metrics + exposure
- `R/03_models.R` — main model specifications
- `R/04_network_spillovers.R` — spillover strategies (controls + MRQAP example)
- `R/05_robustness.R` — robustness checks (placebos, alt outcomes, trimming, weights)
- `R/06_tables_figures.R` — summary stats, texreg tables, network/heatmap plots
- `tikz_did_diagram.tex` — simple LaTeX TikZ figure for expected DV patterns
- `output/` — tables + figures written here
- `data/` — place your real data here when ready

## Quick start (simulation)
From the project root:

```r
source("R/00_setup.R")
source("R/01_simulate_data.R")   # writes simulated data to data/sim_*.rds
source("R/02_build_metrics.R")   # builds panel metrics + exposure variables
source("R/03_models.R")          # estimates main models
source("R/05_robustness.R")      # runs robustness suite
source("R/06_tables_figures.R")  # exports tables/figures
```

## Adapting to your real data
Once you have real FactSet edges + USTR decisions, replace the simulation objects with:
- a **long edge list**: `firm_id, supplier_id, year` (+ optional supplier_country, firm_industry)
- a **firm-year panel**: `firm_id, year, waiver_* variables, sensitivity, controls`

Then re-run `02_build_metrics.R` onward.
