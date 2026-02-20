*******************************************************
* run_all.do
* Execute full Stata companion pipeline
*******************************************************

do "Stata/00_setup.do"
do "Stata/01_simulate_data.do"
do "Stata/02_build_metrics.do"
do "Stata/03_models.do"
do "Stata/04_network_spillovers.do"
do "Stata/05_robustness.do"
do "Stata/06_tables_figures.do"

display as result "Stata pipeline complete."
log close
