*******************************************************
* 01_simulate_data.do
* Simulate firm-year panel + waiver process (Stata)
*******************************************************

do "Stata/00_setup.do"

tempfile firms years panel requests fy_agg

* -------------------------
* 1) Firm base file
* -------------------------
clear
set obs 300
gen firm_n = _n
gen str6 firm_id = "F" + string(firm_n, "%04.0f")
gen industry_num = ceil(runiform()*10)
gen str5 industry = "IND" + string(industry_num)
gen log_assets0 = rnormal(10,1)
gen leverage0 = min(max(rbeta(2,5),0.01),0.99)
gen size_z = (log_assets0-10)/1
gen degree0 = max(6, rpoisson(22)+4)
save `firms', replace

* -------------------------
* 2) Year panel
* -------------------------
clear
set obs 9
gen year = 2013 + _n
keep if inrange(year,2014,2022)
save `years', replace

use `firms', clear
cross using `years'
sort firm_id year
by firm_id: gen t = _n
by firm_id: gen log_assets = log_assets0 + sum(rnormal(0,0.05))
gen assets = exp(log_assets)
by firm_id: gen leverage = leverage0 + sum(rnormal(0,0.02))
replace leverage = min(max(leverage,0.01),0.99)
gen post_tariff = year>=2018
save `panel', replace

* -------------------------
* 3) Request-level waiver decisions
* -------------------------
use `firms', clear
gen n_req = rpoisson(3)+1
expand n_req
bysort firm_id: gen req_num = _n
gen str12 request_id = firm_id + "_R" + string(req_num, "%02.0f")

gen p_grant = invlogit(-0.3 + 0.6*size_z + cond(inlist(industry,"IND9","IND10"),-0.4,0))
gen decision_year = 2018 + floor(runiform()*3)
gen grant = (runiform()<p_grant)
gen comment_count = rpoisson(exp(-1.6 + 0.30*size_z + 0.15*(industry=="IND3")))
gen support_share = rbeta(2.2,2.2)
gen support_count = round(comment_count*support_share)
gen oppose_count  = max(0, comment_count-support_count)
save "$DIR_DATA/sim_requests.dta", replace

* firm-level aggregates for merge
preserve
keep if grant==1
collapse (min) first_grant_year=decision_year, by(firm_id)
tempfile first_grant
save `first_grant', replace
restore

collapse (count) total_requests=request_id ///
         (sum) total_comments=comment_count, by(firm_id)
merge 1:1 firm_id using `first_grant', nogenerate
save `fy_agg', replace

* -------------------------
* 4) Merge core treatment variables to panel
* -------------------------
use `panel', clear
merge m:1 firm_id using `fy_agg', nogenerate
replace total_requests = 0 if missing(total_requests)
replace total_comments = 0 if missing(total_comments)
gen g_year = first_grant_year
gen g_year0 = cond(missing(g_year),0,g_year)
gen post_grant = (year>=g_year) if g_year>.
replace post_grant = 0 if missing(post_grant)

* Approximate cumulative treatment intensity tracker
bysort firm_id (year): gen n_decided = cond(year<2018,0,year-2017)
replace n_decided = max(n_decided,0)
by firm_id: gen ever_grant = !missing(g_year)
by firm_id: gen ever_deny  = 1

gen n_grant = round(n_decided*runiform())
gen n_deny  = n_decided - n_grant
gen waiver_any = n_grant>0
gen deny_any   = n_deny>0
gen grant_share = cond(n_decided==0,0,n_grant/n_decided)

* Approximate supplier outcomes to keep downstream scripts aligned
gen degree = degree0 + round(rnormal(0,3))
replace degree = max(degree,1)
gen share_china = min(max(0.18 + 0.05*waiver_any - 0.08*deny_any + rnormal(0,0.05),0),1)
gen share_usa   = min(max(0.35 + rnormal(0,0.05),0),1)

save "$DIR_DATA/sim_panel.dta", replace
export delimited using "$DIR_DATA/sim_panel.csv", replace

display as result "[01] Wrote sim_panel.dta and sim_requests.dta"
