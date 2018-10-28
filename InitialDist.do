/*
clear

set obs 1000

gen A_dist = rnormal(2000, 250)
*export delimited using "C:\Users\Masato\Dropbox\RA_workplace\French_rep\fortran_codes\optimization_with_simulation\sim_dist.csv", novar replace
export delimited using "C:\Users\Masato\Dropbox\RA_workplace\French_rep\fortran_codes\With_health_transition\sim_A_dist.csv", novar replace

clear

set obs 1000

gen M_dist = runiform(0,1)
replace M_dist = 1 if M_dist<0.01
replace M_dist = 0 if M_dist>=0.01

export delimited using "C:\Users\Masato\Dropbox\RA_workplace\French_rep\fortran_codes\With_health_transition\sim_M_dist.csv", novar replace

*/
clear

local workdir "C:\Users\Masato\Dropbox\RA_workplace\French_rep\fortran_codes\With_health_transition"

import delimited using "`workdir'\simulated_prof_ind.csv"
*import delimited using "`workdir'\decision_prof.csv"

*twoway line c age
