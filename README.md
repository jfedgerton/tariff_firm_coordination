# Tariff Exemptions × Supplier Networks — Real Data Pipeline

Adapted from the simulation scaffold in `tariff_firm_coordination`. Uses real FactSet supplier relationship data + USTR Section 301 exemption decisions.

## Data (NOT tracked in git)
- `data/factset.xlsx` — FactSet supply chain relationships (firm_gvkey, supplier_gvkey, fyear)
- `data/exemptions.*` — USTR exemption request decisions (TBD: format and columns)

## Pipeline
```r
source("R/00_setup.R")
source("R/01_load_data.R")       # loads factset + exemption data → panel + edges + W_norm
source("R/02_build_metrics.R")   # Jaccard, churn, baseline divergence, exposure
source("R/03_models.R")          # TWFE, event-study (sunab), C&S DID
source("R/04_network_spillovers.R")  # exposure controls, community clustering, MRQAP
source("R/05_robustness.R")      # alt outcomes, alt treatments, balancing, placebo
source("R/06_tables_figures.R")  # summary stats, trend plots, network viz
```

## Status
- [x] FactSet data loading (01_load_data.R, Section A)
- [ ] Exemption data loading (01_load_data.R, Sections C + E) — placeholder until data inspected
- [x] Metrics pipeline (02–06) adapted for real column names

## Key differences from simulation scaffold
- `01_simulate_data.R` → `01_load_data.R` (no simulation, loads real data)
- Column names: `firm_gvkey` → `firm_id`, `supplier_gvkey` → `supplier_id`, `fyear` → `year`
- Controls: `log_assets` and `leverage` removed (not in FactSet); `log_degree` used as default
- Clustering: `firm_id + industry` → `firm_id + firm_hq` (HQ country)
- TODO markers throughout for exemption-data-dependent sections
