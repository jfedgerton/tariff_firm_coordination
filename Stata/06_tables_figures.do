*******************************************************
* 06_tables_figures.do
* Summary tables and figures (Stata analog)
*******************************************************

do "Stata/00_setup.do"

use "$DIR_DATA/panel_metrics.dta", clear
keep if !missing(churn_t_t1)

* 1) Summary stats
estpost summarize churn_t_t1 jaccard_t_t1 divergence_to_base n_curr log_degree ///
    waiver_any deny_any grant_share exposure_deny exposure_waive log_assets leverage share_china share_usa
capture which esttab
if !_rc {
    esttab using "$DIR_OUT_T/summary_stats_stata.tex", replace cells("mean sd min max count")
}

* 2) Trend plot: mean churn by waiver status
collapse (mean) churn_t_t1, by(year waiver_any)
twoway (line churn_t_t1 year if waiver_any==0, sort) ///
       (line churn_t_t1 year if waiver_any==1, sort), ///
       legend(order(1 "No waiver" 2 "Waiver")) ///
       ytitle("Mean churn (1-Jaccard)") xtitle("Year")
graph export "$DIR_OUT_F/trend_churn_by_waiver_stata.png", replace

display as result "[06] Summary tables and figure complete"
