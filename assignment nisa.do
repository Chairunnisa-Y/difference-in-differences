use "D:\IMPORTANCE ABOUT ME !\Lanjut Sekolah\Universitas Indonesia\Semester 2\CSPD\After UTS\assignment.dta", clear
est clear
set more off
gen post75 = year >= 1976
gen treat_post = pc1975 * post75


eststo clear
eststo: regress tout treat_post, robust
esttab using results.rtf, replace b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) title("OLS Regression") label


* Create variable for treatment group
gen treated = pc1975

* Generate dummy full year
tabulate year, generate(year_)

* Estimasi Two-Way Fixed Effects DID: yearly effect
eststo: areg tout c.treated#i.year, absorb(fips) robust

esttab using results.rtf, replace b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) title("Two-Ways FE") label


* Estimate with controls variables
eststo: regress tout treat_post avg_income pct_white pct_college pct_adult pct_unemployed bilingual polltax hballot, robust

esttab using results.rtf, replace b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) title("Regression with Controls") label


* Preparation
use assignment.dta, clear
xtset fips year

* Create post-treatment variables
gen post75 = year >= 1976

* Prepare the temporary file to keep the output
tempname memhold
tempfile placebo_results
postfile `memhold' coef using `placebo_results', replace

* Loop 500 times
set seed 12345

forvalues i = 1/500 {
    
    preserve
    
    * Create random treatment per fips
    gen rand = runiform()
    egen rand_treat = mean(rand), by(fips)
    gen placebo_treat = rand_treat > 0.5
    
    * Create interaction placebo_treat_post
    gen placebo_treat_post = placebo_treat * post75
    
    * Run placebo regression
    regress tout placebo_treat_post, robust
    
    * Keep coeff. placebo_treat_post
    scalar b = _b[placebo_treat_post]
    post `memhold' (b)
    
    restore
}

postclose `memhold'

* Load the result dan create histogram
use `placebo_results', clear
histogram coef, width(0.005) color(gs14) ///
    xtitle("Koefisien Placebo Treatment (Random Assignment)") ///
    ytitle("Frekuensi") ///
    title("Distribusi Koefisien Placebo (500 Simulasi)")


gen bilingual_post = bilingual * post75
gen polltax_post = polltax * post75

regress tout treat_post bilingual_post polltax_post, robust
