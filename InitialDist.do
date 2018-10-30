/*

clear

set obs 10000

gen random = runiform(0,1)
gen M_dist = 1 if random<0.01
replace M_dist = 0 if random>=0.01
drop random

*gen A_dist = rnormal(1000, 250)
gen A_health = rnormal(40000, 5000)
gen A_unhealth = rnormal(25000, 5000)

gen A_dist = A_health if M_dist==0
replace A_dist = A_unhealth if M_dist==1

gen AIME_health = rnormal(3000, 500)
gen AIME_unhealth = rnormal(2000, 500)

gen AIME_dist = AIME_health if M_dist==0
replace AIME_dist = AIME_unhealth if M_dist==1

preserve

keep A_dist
*export delimited using "C:\Users\Masato\Dropbox\RA_workplace\French_rep\fortran_codes\optimization_with_simulation\sim_dist.csv", novar replace
export delimited using "C:\Users\Masato\Dropbox\RA_workplace\French_rep\fortran_codes\With_social_security\sim_A_dist.csv", novar replace

restore

preserve

keep AIME_dist
*export delimited using "C:\Users\Masato\Dropbox\RA_workplace\French_rep\fortran_codes\optimization_with_simulation\sim_dist.csv", novar replace
export delimited using "C:\Users\Masato\Dropbox\RA_workplace\French_rep\fortran_codes\With_social_security\sim_AIME_dist.csv", novar replace

restore


keep M_dist
export delimited using "C:\Users\Masato\Dropbox\RA_workplace\French_rep\fortran_codes\With_social_security\sim_M_dist.csv", novar replace

clear

set obs 10000

gen W_dist = rnormal(7,3)

export delimited using "C:\Users\Masato\Dropbox\RA_workplace\French_rep\fortran_codes\With_social_security\sim_W_dist.csv", novar replace

clear

set obs 660000

gen W_shock = rnormal(0,1)

export delimited using "C:\Users\Masato\Dropbox\RA_workplace\French_rep\fortran_codes\With_social_security\wage_shock.csv", novar replace
*/
clear

local workdir "C:\Users\Masato\Dropbox\RA_workplace\French_rep\fortran_codes\With_social_security"

*import delimited using "`workdir'\simulated_prof_ind.csv"
import delimited using "`workdir'\simulated_prof.csv"
*import delimited using "`workdir'\valuesopt.csv"

*collapse (mean) aopt copt hopt, by(age m)
*twoway (line copt age if m==1) (line copt age if m==0)

twoway (line a_good age) (line a_bad age)

gen Wnum = 10
gen Hnum = 10
gen Anum = 10
gen AIMEnum = 10
gen Cnum = 100
gen pen_start = 65
gen last = "analytical"
gen integral="French"

save `workdir'\simulated_prof_fixed_ver1.dta, replace
