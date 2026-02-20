*******************************************************
* 03_models.do
* Main model specs (TWFE, static DiD, event-time)
*******************************************************

do "Stata/00_setup.do"

use "$DIR_DATA/panel_metrics.dta", clear
keep if !missing(churn_t_t1)

reghdfe churn_t_t1 waiver_any deny_any post_tariff exposure_deny exposure_waive ///
        log_assets leverage log_degree, absorb(firm_id year) vce(cluster firm_id)
est store m_twfe_1

gen ever_waiver = ever_grant==1
reghdfe churn_t_t1 i.post_tariff##i.ever_waiver log_assets leverage, ///
        absorb(firm_id year) vce(cluster firm_id)
est store m_did_static

gen rel_time = year - g_year if g_year>0
forvalues k = 0/4 {
    gen evt_p`k' = (rel_time==`k') if g_year>0
}
forvalues k = 2/4 {
    gen evt_m`k' = (rel_time==-`k') if g_year>0
}
reghdfe churn_t_t1 evt_p* evt_m* log_assets leverage, absorb(firm_id year) vce(cluster firm_id)
est store m_es

reghdfe churn_t_t1 c.log_assets##(evt_p* evt_m*) leverage, absorb(firm_id year) vce(cluster firm_id)
est store m_es_ddd

reghdfe churn_t_t1 grant_share exposure_deny log_assets leverage, absorb(firm_id year) vce(cluster firm_id)
est store m_intensity

capture which esttab
if !_rc {
    esttab m_twfe_1 m_did_static m_es m_es_ddd m_intensity using "$DIR_OUT_T/reg_main_stata.tex", replace se
}

display as result "[03] Model estimation complete"
