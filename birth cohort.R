# Simulate Malian birth cohort and calculate cases
load("PostDistr.Rdata")
source("intervention efficacy.R") # load intervention efficacy data
source("AR estimate.R")  # load attack rate data

# replace 0s with 1s for every month infant is alive through 6 months of life
mat_cohort <- empty_cohort
for (i in 1:sim_mo) {
  for(h in 1:12){
  if( h-i<=0){
  mat_cohort[h,i]<-1
  if(i-h >=6){
    mat_cohort[h,i] <-0
  }
  }}}

# number of people born in Mali 
pop_mali <- 18540000 # 2017 population in Mali (World Bank)
br_crude <- 42 # crude birth rate per 1000 people in Mali, 2017 (World Bank)
pop_bc <- pop_mali * (br_crude/1000) # total number of infants born in one year in Mali

# create a matrix for the number of infants alive in birth cohort through first sixth months of life
days_per_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
days_per_year <- days_per_month/365
num_infants <- days_per_year* pop_bc
mat_infants <- (matrix(num_infants, 12, ncol = sim_mo)) * mat_cohort

# only administer Palivizumab to infants during RSV season  
# assume RSV season lasts 4 months: July, August, September, October
mAb_admin <- mat_cohort 
# create matrix for Palivizumab administration (regimen applicability)
for (i in 1:sim_mo) {
  for(h in 1:12){
    if( i<7 | i>10){
      mAb_admin[h,i]<-0
    }
  }}

# create matrix for Palivizumab efficacy
mAb_eff<-mAb_admin
for (i in 1:sim_mo) {
  for(h in 1:12){
      mAb_eff[h,i]<- mAb_eff[h,i]*mAb_eff_bc
  }}

# create matrix for mVax admin to mothers
mVax_admin <- empty_cohort
for(i in 1:12) {
  mVax_admin[i, i] <-1
}

# create matrix for mVax duration of effect
# assume mVax is given to all preg. women and is effective only 1st 3 months of infant life
mVax_dur <- empty_cohort
mVax_months <- 3 # mVax has 3 months duration
for (i in 1:12) {
  mVax_dur[i, i:(i+(mVax_months-1))] <- 1
}

# create a matrix for efficacy of mVax bc 
mVax_eff <- mVax_dur* mVax_eff_bc

# create matrix for llAb administration (regimen applicability)
# birth dose only for the onset of RSV season (WHO PPC, 2020)
llAb_admin <- empty_cohort
for (i in 1:12) {
  if(i >2 & i < 11) 
  llAb_admin[i, i] <- 1
}

# create matrix for when llAb provides protection
# 50 mg dose of llAb has 70.1% efficacy through first five months
mat_eff_llAb <- mat_cohort
for (i in 1:sim_mo) {
  for(h in 1:12){
    if(i > (4+h) | (h<3) | (h>10)) {
      mat_eff_llAb[h,i] <- 0
    }}}
llAb_eff <- mat_eff_llAb * llAb_eff_bc


# functions for calculating the probability of disease and the number of cases while removing the babies who get sick in each cohort
pd_calc <- function (Ve, cov, AR, ad) {
  mat_out <- AR* (1-ad) + AR* ad* cov* (1-Ve) + AR* ad* (1-cov)
}

RSVcases <- function (pd, babies) {
  lim <- dim(pd)[2]
  cases <- matrix(0, nrow = dim(pd)[1], ncol = lim)
  bb <- babies
  for (m in 1:lim) {
    cases[,m] <- pd[,m]*bb
    bb <- bb - cases[,m]
  }
  sum(cases)
}

# calculate probability of disease under no intervention across uncertainty range for the attack rate
cases_no_u <- apply(AR_u, 3, RSVcases, babies = num_infants)

# calculate number of cases under mAb intervention
p_mAb_bc <- 0.77 # DTP3 coverage in Mali (WHO & UNICEF estimate 2018)
pd_mAb_mat_bc <- pd_calc(mAb_eff, p_mAb_bc, AR_bc, mAb_admin)
cases_mAb_bc <- RSVcases(pd_mAb_mat_bc, num_infants)

# conduct sensitivity analysis for cases under mAb intervention by coverage (from 0 to 100%)
SA_cov_vec <- seq(0, 1, by = 0.2)
SA_mAbcov_cases_ar <- array(NA, dim = c(dim(AR_bc)[1], dim(AR_bc)[2], length(SA_cov_vec)))
for (cV in 1:length(SA_cov_vec)) {
  SA_mAbcov_cases_ar[,,cV] <- pd_calc(mAb_eff, SA_cov_vec[cV], AR_bc, mAb_admin)
} 
SA_mAbcov_cases <- apply(SA_mAbcov_cases_ar, 3, RSVcases, babies = num_infants)

# calculate number of cases across uncertainty range for Palivizumab efficacy and the attack rates
pd_mAb_array <- array(NA, dim = c(dim(AR_bc)[1], dim(AR_bc)[2], trials))
for (eV in 1:length(eff_mAb)) {
  pd_mAb_array[,,eV] <- pd_calc(eff_mAb[eV], p_mAb_bc, AR_u[,,eV], mAb_admin)
} 

cases_mAb_u <- apply(pd_mAb_array, 3, RSVcases, babies = num_infants)

# calculate number of cases under mVax intervention
# p_mVax_bc = ANC4+ Mali (UNICEF, 2018) * DTP1 Mali (WHO, 2018), metric from Debellut et al. 2018
p_mVax_bc <- 0.433* 0.82 
pd_mVax_mat_bc <- pd_calc(mVax_eff, p_mVax_bc, AR_bc, mVax_dur)
cases_mVax_bc <-  RSVcases(pd_mVax_mat_bc, num_infants)

# conduct sensitivity analysis for mVax cases by intervention coverage (from 0 to 100%)
SA_mVcov_cases_ar <- array(NA, dim = c(dim(AR_bc)[1], dim(AR_bc)[2], length(SA_cov_vec)))
for (cV in 1:length(SA_cov_vec)) {
  SA_mVcov_cases_ar[,,cV] <- pd_calc(mVax_eff, SA_cov_vec[cV], AR_bc, mVax_dur)
} 
SA_mVcov_cases <- apply(SA_mVcov_cases_ar, 3, RSVcases, babies = num_infants)

# calculate number of cases across uncertainty range for maternal vaccine efficacy and the attack rates
pd_mVax_array <- array(NA, dim = c(dim(AR_bc)[1], dim(AR_bc)[2], trials))
for (eV in 1:length(eff_mVax)) {
  pd_mVax_array[,,eV] <- pd_calc(eff_mVax[eV], p_mVax_bc, AR_u[,,eV], mVax_dur)
} 
cases_mVax_u <- apply(pd_mVax_array, 3, RSVcases, babies = num_infants)

# calculate number of cases under llAb intervention
p_llAb_bc <- 0.83 # probability of receiving llAb, assumption based on 2018 BCG coverage in Mali (WHO, 2018 estimate)

pd_llAb_mat_bc <- pd_calc(llAb_eff_bc, p_llAb_bc, AR_bc, mat_eff_llAb)
cases_llAb_bc <- RSVcases(pd_llAb_mat_bc, num_infants)

# conduct sensitivity analysis for mVax cases by intervention coverage (from 0 to 100%)
SA_llcov_cases_ar <- array(NA, dim = c(dim(AR_bc)[1], dim(AR_bc)[2], length(SA_cov_vec)))
for (cV in 1:length(SA_cov_vec)) {
  SA_llcov_cases_ar[,,cV] <- pd_calc(llAb_eff, SA_cov_vec[cV], AR_bc, mat_eff_llAb)
} 
SA_llcov_cases <- apply(SA_llcov_cases_ar, 3, RSVcases, babies = num_infants)

# calculate number of cases across uncertainty range for long-acting mAb efficacy and the attack rates
pd_llAb_array <- array(NA, dim = c(dim(AR_bc)[1], dim(AR_bc)[2], trials))
for (eV in 1:length(eff_llAb)) {
  pd_llAb_array[,,eV] <- pd_calc(eff_llAb[eV], p_llAb_bc, AR_u[,,eV], mat_eff_llAb)
} 
cases_llAb_u <- apply(pd_llAb_array, 3, RSVcases, babies = num_infants)
