*******************************************************
* 02_build_metrics.do
* Build churn/Jaccard-style metrics + exposures
*******************************************************

do "Stata/00_setup.do"

use "$DIR_DATA/sim_panel.dta", clear
sort firm_id year

* Declare panel structure (required for lag operator L.)
encode firm_id, gen(firm_id_num)
tsset firm_id_num year

* Lag-based overlap proxy (since full set-Jaccard is R-specific)
gen degree_lag = L.degree

gen n_curr = degree
gen n_prev = degree_lag

gen n_inter = round(min(n_curr,n_prev)*(0.75 + rnormal(0,0.06))) if !missing(n_prev)
replace n_inter = min(n_inter, n_prev) if !missing(n_prev)
replace n_inter = max(n_inter, 0) if !missing(n_prev)

gen n_add  = n_curr - n_inter if !missing(n_prev)
gen n_drop = n_prev - n_inter if !missing(n_prev)

gen jaccard_t_t1 = n_inter/(n_curr+n_prev-n_inter) if !missing(n_prev)
gen churn_t_t1 = 1 - jaccard_t_t1 if !missing(jaccard_t_t1)

gen retention_rate = n_inter/n_prev if n_prev>0
gen add_rate = n_add/n_prev if n_prev>0
gen drop_rate = n_drop/n_prev if n_prev>0

* Baseline similarity to 2017
preserve
keep if year==2017
keep firm_id n_curr
rename n_curr n_base
tempfile base
save `base'
restore

merge m:1 firm_id using `base', nogenerate
gen jaccard_to_base = min(n_curr,n_base)/max(n_curr,n_base) if n_base>0
gen divergence_to_base = 1-jaccard_to_base if !missing(jaccard_to_base)

* Exposure mapping proxy: mean neighbor treatment within industry-year
bysort industry year: egen exposure_deny = mean(deny_any)
bysort industry year: egen exposure_waive = mean(waiver_any)
bysort industry year: gen n_indyr = _N
replace exposure_deny = exposure_deny - deny_any/n_indyr
replace exposure_waive = exposure_waive - waiver_any/n_indyr

gen log_degree = log(n_curr+1)

save "$DIR_DATA/panel_metrics.dta", replace
export delimited using "$DIR_DATA/panel_metrics.csv", replace

display as result "[02] Wrote panel_metrics.dta"
