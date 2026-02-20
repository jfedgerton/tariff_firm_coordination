*******************************************************
* 00_setup.do
* Tariff Waivers × Supplier Networks — Stata setup
*******************************************************

version 17
clear all
set more off
set seed 12345

* Root should be repo root
global ROOT `c(pwd)'
global DIR_DATA  "$ROOT/data"
global DIR_OUT_T "$ROOT/output/tables"
global DIR_OUT_F "$ROOT/output/figures"

cap mkdir "$DIR_DATA"
cap mkdir "$ROOT/output"
cap mkdir "$DIR_OUT_T"
cap mkdir "$DIR_OUT_F"

capture log close
log using "$ROOT/output/stata_pipeline.log", replace text

display as text "[setup] ROOT = $ROOT"
display as text "[setup] data dir = $DIR_DATA"

* Optional package checks
foreach p in reghdfe ftools estout csdid coefplot {
    capture which `p'
    if _rc {
        di as error "Package `p' not found. Install with: ssc install `p'"
    }
}
