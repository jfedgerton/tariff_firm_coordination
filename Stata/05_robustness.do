*******************************************************
* 05_robustness.do
* Robustness checks suite (Stata analog)
*******************************************************

do "Stata/00_setup.do"

use "$DIR_DATA/panel_metrics.dta", clear
keep if !missing(churn_t_t1)

gen rel_time = year - g_year if g_year>0
forvalues k = 0/4 {
    gen evt_p`k' = (rel_time==`k') if g_year>0
}
forvalues k = 2/4 {
    gen evt_m`k' = (rel_time==-`k') if g_year>0
}

* 1) Alternative outcomes
foreach y in churn_t_t1 divergence_to_base add_rate drop_rate retention_rate {
    reghdfe `y' evt_p* evt_m* log_assets leverage if !missing(`y'), absorb(firm_id year) vce(cluster firm_id)
    estimates store m_`y'
}

* 2) Alternative treatment definitions
reghdfe churn_t_t1 waiver_any exposure_deny log_assets leverage, absorb(firm_id year) vce(cluster firm_id)
est store m_alt_t1

reghdfe churn_t_t1 grant_share exposure_deny log_assets leverage, absorb(firm_id year) vce(cluster firm_id)
est store m_alt_t2

gen treated_clean = ever_grant==1 if ever_grant!=ever_deny
reghdfe churn_t_t1 i.post_tariff##i.treated_clean log_assets leverage if !missing(treated_clean), ///
        absorb(firm_id year) vce(cluster firm_id)
est store m_alt_t3

* 3) Balanced panel
bysort firm_id: gen nT = _N
summ year, meanonly
local nYears = r(max)-r(min)+1
reghdfe churn_t_t1 evt_p* evt_m* log_assets leverage if nT==`nYears', ///
        absorb(firm_id year) vce(cluster firm_id)
est store m_bal

* 4) Degree trimming
summ n_curr, detail
local p99 = r(p99)
reghdfe churn_t_t1 evt_p* evt_m* log_assets leverage if n_curr<=`p99', ///
        absorb(firm_id year) vce(cluster firm_id)
est store m_trim

capture which esttab
if !_rc {
    esttab m_alt_t1 m_alt_t2 m_alt_t3 m_bal m_trim using "$DIR_OUT_T/robust_misc_stata.tex", replace se
}

display as result "[05] Robustness checks complete"
