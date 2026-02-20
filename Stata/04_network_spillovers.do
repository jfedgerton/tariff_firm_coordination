*******************************************************
* 04_network_spillovers.do
* Spillover strategies in Stata analog
*******************************************************

do "Stata/00_setup.do"

use "$DIR_DATA/panel_metrics.dta", clear
keep if !missing(churn_t_t1)

* Strategy 1: exposure controls
reghdfe churn_t_t1 waiver_any exposure_deny exposure_waive log_assets leverage, ///
        absorb(firm_id year) vce(cluster firm_id)
est store m_spill_ctrl

* Strategy 2: community proxy using industry clusters (placeholder for overlap-network communities)
egen comm_id = group(industry)
reghdfe churn_t_t1 waiver_any exposure_deny exposure_waive log_assets leverage, ///
        absorb(firm_id year) vce(cluster comm_id)
est store m_spill_comm_cluster

capture which esttab
if !_rc {
    esttab m_spill_ctrl m_spill_comm_cluster using "$DIR_OUT_T/reg_spillover_strategies_stata.tex", replace se
}

display as result "[04] Spillover models complete"
