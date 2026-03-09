# CLAUDE.md

## Project Overview

Research project studying how tariff waivers affect firm-supplier network dynamics (supplier churn). Uses a simulated bipartite firm-supplier panel with staggered difference-in-differences estimation and network spillover effects.

## Languages & Tools

- **R** (primary): simulation, econometrics, visualization
- **Stata**: companion replication scripts (requires Stata 17+)
- **LaTeX/TikZ**: tables and diagrams

## Project Structure

```
R/                  # Main R pipeline (00-06, run sequentially)
Stata/              # Stata companion scripts + run_all.do
data/               # Generated data files (RDS, CSV)
output/figures/     # PNG visualizations
output/tables/      # LaTeX and CSV tables
```

## Running the R Pipeline

From the project root, source scripts in order:

```r
source("R/00_setup.R")             # installs packages, sets paths
source("R/01_simulate_data.R")     # generates firm-supplier panel
source("R/02_build_metrics.R")     # computes Jaccard similarity, churn, exposure
source("R/03_models.R")            # TWFE, event-study, Callaway-Sant'Anna
source("R/04_network_spillovers.R")# network spillover models
source("R/05_robustness.R")        # robustness checks
source("R/06_tables_figures.R")    # exports tables and figures
```

## Key R Dependencies

Core: `data.table`, `dplyr`, `tidyr`, `purrr`
Econometrics: `fixest`, `did`, `WeightIt`, `cobalt`
Network: `igraph`, `sna`, `ggraph`
Output: `ggplot2`, `texreg`, `modelsummary`, `knitr`, `kableExtra`

All packages are installed automatically by `R/00_setup.R`.

## Key Conventions

- Data files go in `data/`; figures in `output/figures/`; tables in `output/tables/`
- R scripts are numbered 00-06 and must run sequentially (each depends on prior outputs)
- RDS format is used for intermediate R objects; CSV is exported for cross-tool use
- The main outcome variable is `churn_t_t1` (1 - Jaccard similarity of supplier sets)
- Treatment variables: `waiver_any`, `grant_share`, `deny_any`
