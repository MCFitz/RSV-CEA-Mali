# Sensitivity Analysis

source("confidence intervals.R")

# Calculate ICERs for each intervention when each parameter is set to its lower and upper CI, all others held at base case

CER_func <- function(cases, p_LRTI, p_inpat, CFR, dw_s, dw_m, duration_ill, p_int, num_eligible, cost_inpat, cost_out) {
  cases_no <- SA_mAbcov_cases[1]
  YLL <- YLL_pc - 7/12
  int_cost <- 3 + adcost_llAb
  med_cost <- (cost_inpat * p_LRTI * p_inpat * p_seek_care * cases) + (cost_out * cases * p_LRTI * (1- p_inpat))
  medcost_no <- (cost_inpat * p_LRTI * p_inpat * p_seek_care * cases_no) + (cost_out * cases_no * p_LRTI * (1- p_inpat))
  mort_no <- (cases_no * p_LRTI * p_inpat * p_seek_care * CFR) + (cases_no * p_LRTI * p_inpat * (1-p_seek_care) * CFR/0.51*0.49)
  YLL_calc_no <- mort_no * YLL
  YLD_calc_no <- (((cases_no * p_LRTI) - mort_no) * p_inpat * duration_ill * dw_s) + (((cases_no * p_LRTI) - mort_no) * ((1 - p_inpat) * duration_ill * dw_m))
  DALYs_no <- YLL_calc_no + YLD_calc_no 
  mort <- (cases * p_LRTI * p_inpat * p_seek_care * CFR) + (cases * p_LRTI * p_inpat * (1-p_seek_care) * CFR/0.51*0.49)
  YLL_calc <- mort * YLL
  YLD_calc <- (((cases * p_LRTI) - mort_no) * p_inpat * duration_ill * dw_s) + (((cases * p_LRTI) - mort_no) * ((1 - p_inpat) * duration_ill * dw_m))
  DALYs_lost <- YLL_calc + YLD_calc
  CER <- (med_cost + (num_eligible * p_int * int_cost) - medcost_no) / (DALYs_no - DALYs_lost)
}


CER_AR <- function(cases_no, cases, p_LRTI, p_inpat, CFR, dw_s, dw_m, duration_ill, p_int, num_eligible, cost_inpat, cost_out) {
  YLL <- YLL_pc - 7/12
  int_cost <- 3 + adcost_llAb
  med_cost <- (cost_inpat * p_LRTI * p_inpat * p_seek_care * cases) + (cost_out * cases * p_LRTI * (1- p_inpat))
  medcost_no <- (cost_inpat * p_LRTI * p_inpat * p_seek_care * cases_no) + (cost_out * cases_no * p_LRTI * (1- p_inpat))
  mort_no <- (cases_no * p_LRTI * p_inpat * p_seek_care * CFR) + (cases_no * p_LRTI * p_inpat * (1-p_seek_care) * CFR/0.51*0.49)
  YLL_calc_no <- mort_no * YLL
  YLD_calc_no <- (((cases_no * p_LRTI) - mort_no) * p_inpat * duration_ill * dw_s) + (((cases_no * p_LRTI) - mort_no) * ((1 - p_inpat) * duration_ill * dw_m))
  DALYs_no <- YLL_calc_no + YLD_calc_no 
  mort <- (cases * p_LRTI * p_inpat * p_seek_care * CFR) + (cases * p_LRTI * p_inpat * (1-p_seek_care) * CFR/0.51*0.49)
  YLL_calc <- mort * YLL
  YLD_calc <- (((cases * p_LRTI) - mort_no) * p_inpat * duration_ill * dw_s) + (((cases * p_LRTI) - mort_no) * ((1 - p_inpat) * duration_ill * dw_m))
  DALYs_lost <- YLL_calc + YLD_calc
  CER <- (med_cost + (num_eligible * p_int * int_cost) - medcost_no) / (DALYs_no - DALYs_lost)
}


### point estimates for number of cases when you set the AR matrix to the base case and modify only the intervention efficacy at its lower and upper 95% CIs
pd_mAb_LC_eff <- pd_calc(CI_mAb_eff[1], p_mAb_bc, AR_bc, mAb_admin)
cases_mAb_LC_eff <- RSVcases(pd_mAb_LC_eff, num_infants)
pd_mAb_UC_eff <- pd_calc(CI_mAb_eff[2], p_mAb_bc, AR_bc, mAb_admin)
cases_mAb_UC_eff <- RSVcases(pd_mAb_UC_eff, num_infants)

pd_mVax_LC_eff <- pd_calc(CI_mVax_eff[1], p_mVax_bc, AR_bc, mVax_dur)
cases_mVax_LC_eff <- RSVcases(pd_mVax_LC_eff, num_infants)
pd_mVax_UC_eff <- pd_calc(CI_mVax_eff[2], p_mVax_bc, AR_bc, mVax_dur)
cases_mVax_UC_eff <- RSVcases(pd_mVax_UC_eff, num_infants)

pd_llAb_LC_eff <- pd_calc(CI_llAb_eff[1], p_llAb_bc, AR_bc, mat_eff_llAb)
cases_llAb_LC_eff <- RSVcases(pd_llAb_LC_eff, num_infants)
pd_llAb_UC_eff <- pd_calc(CI_llAb_eff[2], p_llAb_bc, AR_bc, mat_eff_llAb)
cases_llAb_UC_eff <- RSVcases(pd_llAb_UC_eff, num_infants)

### point estimates for the number of cases when you set intervention efficacy to the base case and modify the AR matrix only across lower and upper 95% CI ranges for the <3mo. and 3-<6 mo.age based attack rates

# create vectors for age-based ARs at lower 95% CI for <3mo. as well as >3mo. & <6mo
age_AR3_LC <- c(CI_AR_age[1,1:3], age_AR[4:6])
age_AR3_UC <- c(CI_AR_age[2,1:3], age_AR[4:6])
age_AR6_LC <- c(age_AR[1:3], CI_AR_age[1, 4:6])
age_AR6_UC <-c(age_AR[1:3], CI_AR_age[2, 4:6])

# construct AR matrices for each
temp5 <- empty_cohort
for (a in 1:12) {
  temp5[a,] <- c(rep(0, times = a-1), age_AR3_LC, rep(0, times = 12-a))
}
AR3_LC <- temp2/(mali_inc/1000)*temp5

temp6 <- empty_cohort
for (a in 1:12) {
  temp6[a,] <- c(rep(0, times = a-1), age_AR3_UC, rep(0, times = 12-a))
}
AR3_UC <- temp2/(mali_inc/1000)*temp6

temp7 <- empty_cohort
for (a in 1:12) {
  temp7[a,] <- c(rep(0, times = a-1), age_AR6_LC, rep(0, times = 12-a))
}
AR6_LC <- temp2/(mali_inc/1000)*temp7

temp8 <- empty_cohort
for (a in 1:12) {
  temp8[a,] <- c(rep(0, times = a-1), age_AR6_UC, rep(0, times = 12-a))
}
AR6_UC <- temp2/(mali_inc/1000)*temp8

cases_no_LC_AR3 <- RSVcases(AR3_LC, num_infants)
cases_no_UC_AR3 <- RSVcases(AR3_UC, num_infants)
cases_no_LC_AR6 <- RSVcases(AR6_LC, num_infants)
cases_no_UC_AR6 <- RSVcases(AR6_UC, num_infants)
cases_no_bc <- RSVcases(AR_bc, num_infants)

pd_mAb_LC_AR3 <- pd_calc(mAb_eff_bc, p_mAb_bc, AR3_LC, mAb_admin)
cases_mAb_LC_AR3 <- RSVcases(pd_mAb_LC_AR3, num_infants)
pd_mAb_UC_AR3 <- pd_calc(mAb_eff_bc, p_mAb_bc, AR3_UC, mAb_admin)
cases_mAb_UC_AR3 <- RSVcases(pd_mAb_UC_AR3, num_infants)

pd_mVax_LC_AR3 <- pd_calc(mVax_eff_bc, p_mVax_bc, AR3_LC, mVax_dur)
cases_mVax_LC_AR3 <- RSVcases(pd_mVax_LC_AR3, num_infants)
pd_mVax_UC_AR3 <- pd_calc(mVax_eff_bc, p_mVax_bc, AR3_UC, mVax_dur)
cases_mVax_UC_AR3 <- RSVcases(pd_mVax_UC_AR3, num_infants)

pd_llAb_LC_AR3 <- pd_calc(llAb_eff_bc, p_llAb_bc, AR3_LC, mat_eff_llAb)
cases_llAb_LC_AR3 <- RSVcases(pd_llAb_LC_AR3, num_infants)
pd_llAb_UC_AR3 <- pd_calc(llAb_eff_bc, p_llAb_bc, AR3_UC, mat_eff_llAb)
cases_llAb_UC_AR3 <- RSVcases(pd_llAb_UC_AR3, num_infants)

pd_mAb_LC_AR6 <- pd_calc(mAb_eff_bc, p_mAb_bc, AR6_LC, mAb_admin)
cases_mAb_LC_AR6 <- RSVcases(pd_mAb_LC_AR6, num_infants)
pd_mAb_UC_AR6 <- pd_calc(mAb_eff_bc, p_mAb_bc, AR6_UC, mAb_admin)
cases_mAb_UC_AR6 <- RSVcases(pd_mAb_UC_AR6, num_infants)

pd_mVax_LC_AR6 <- pd_calc(mVax_eff_bc, p_mVax_bc, AR6_LC, mVax_dur)
cases_mVax_LC_AR6 <- RSVcases(pd_mVax_LC_AR6, num_infants)
pd_mVax_UC_AR6 <- pd_calc(mVax_eff_bc, p_mVax_bc, AR6_UC, mVax_dur)
cases_mVax_UC_AR6 <- RSVcases(pd_mVax_UC_AR6, num_infants)

pd_llAb_LC_AR6 <- pd_calc(llAb_eff_bc, p_llAb_bc, AR6_LC, mat_eff_llAb)
cases_llAb_LC_AR6 <- RSVcases(pd_llAb_LC_AR6, num_infants)
pd_llAb_UC_AR6 <- pd_calc(llAb_eff_bc, p_llAb_bc, AR6_UC, mat_eff_llAb)
cases_llAb_UC_AR6 <- RSVcases(pd_llAb_UC_AR6, num_infants)

### checking
(pd_mVax_LC_AR6 - AR6_LC) - (pd_mVax_UC_AR6 - AR6_UC)
(cases_mVax_LC_AR6 - cases_no_LC_AR6) - (cases_mVax_UC_AR6 - cases_no_UC_AR6)
# are the 0-3 mo cases the same under mVax LC and UC when only the AR6 is changing? YES
# and under status quo for LC and UC when only the AR6 is changing? YES

# mAb ICER calculations

ct_mAb_bc <- CER_func(cases_mAb_bc, p_pneum, p_inpatient, CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod,
                di_yrs, p_mAb_bc, num_infants_eligible_mAb, cost_hosp, cost_outpatient)

ct_mAb_LC_AR3 <- CER_AR(cases_no_LC_AR3, cases_mAb_LC_AR3, p_pneum, p_inpatient, CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod,
                    di_yrs, p_mAb_bc, num_infants_eligible_mAb, cost_hosp, cost_outpatient)

ct_mAb_UC_AR3 <- CER_AR(cases_no_UC_AR3, cases_mAb_UC_AR3, p_pneum, p_inpatient, CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod,
                    di_yrs, p_mAb_bc, num_infants_eligible_mAb, cost_hosp, cost_outpatient)

ct_mAb_LC_AR6 <- CER_AR(cases_no_LC_AR6, cases_mAb_LC_AR6, p_pneum, p_inpatient, CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod,
                       di_yrs, p_mAb_bc, num_infants_eligible_mAb, cost_hosp, cost_outpatient)

ct_mAb_UC_AR6 <- CER_AR(cases_no_UC_AR6, cases_mAb_UC_AR6, p_pneum, p_inpatient, CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod,
                       di_yrs, p_mAb_bc, num_infants_eligible_mAb, cost_hosp, cost_outpatient)

ct_mAb_LC_eff <- CER_func(cases_mAb_LC_eff, p_pneum, p_inpatient, CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod,
                   di_yrs, p_mAb_bc, num_infants_eligible_mAb, cost_hosp, cost_outpatient)

ct_mAb_UC_eff <- CER_func(cases_mAb_UC_eff, p_pneum, p_inpatient, CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod,
                   di_yrs, p_mAb_bc, num_infants_eligible_mAb, cost_hosp, cost_outpatient)

ct_mAb_LC_pneum <- CER_func(cases_mAb_bc, CI_LRTI[1], p_inpatient, 
                      CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mAb_bc,
                      num_infants_eligible_mAb, cost_hosp, cost_outpatient)  

ct_mAb_UC_pneum <- CER_func(cases_mAb_bc, CI_LRTI[2], p_inpatient, 
                      CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mAb_bc,
                      num_infants_eligible_mAb, cost_hosp, cost_outpatient)  

ct_mAb_LC_inpat <-CER_func(cases_mAb_bc, p_pneum, CI_inpat[1], 
                     CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mAb_bc,
                     num_infants_eligible_mAb, cost_hosp, cost_outpatient)  

ct_mAb_UC_inpat <- CER_func(cases_mAb_bc, p_pneum, CI_inpat[2], 
                      CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mAb_bc,
                      num_infants_eligible_mAb, cost_hosp, cost_outpatient) 

ct_mAb_LC_CFR <- CER_func(cases_mAb_bc, p_pneum, p_inpatient, 
                    CI_CFR[1], dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mAb_bc,
                    num_infants_eligible_mAb, cost_hosp, cost_outpatient)  

ct_mAb_UC_CFR <- CER_func(cases_mAb_bc, p_pneum, p_inpatient, 
                    CI_CFR[2], dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mAb_bc,
                    num_infants_eligible_mAb, cost_hosp, cost_outpatient)

ct_mAb_LC_dw_LRTI_severe <- CER_func(cases_mAb_bc, p_pneum, p_inpatient, 
                          CFR_inpatient, CI_dw_LRTI_severe[1], dw_LRTI_mod, di_yrs, p_mAb_bc,
                          num_infants_eligible_mAb, cost_hosp, cost_outpatient)  

ct_mAb_UC_dw_LRTI_severe <- CER_func(cases_mAb_bc, p_pneum, p_inpatient, 
                          CFR_inpatient, CI_dw_LRTI_severe[2], dw_LRTI_mod, di_yrs, p_mAb_bc,
                          num_infants_eligible_mAb, cost_hosp, cost_outpatient)  

ct_mAb_LC_dw_LRTI_mod <- CER_func(cases_mAb_bc, p_pneum, p_inpatient, 
                            CFR_inpatient, dw_LRTI_severe, CI_dw_LRTI_mod[1], di_yrs, p_mAb_bc,
                            num_infants_eligible_mAb, cost_hosp, cost_outpatient)

ct_mAb_UC_dw_LRTI_mod <- CER_func(cases_mAb_bc, p_pneum, p_inpatient, 
                            CFR_inpatient, dw_LRTI_severe, CI_dw_LRTI_mod[2], di_yrs, p_mAb_bc,
                            num_infants_eligible_mAb, cost_hosp, cost_outpatient)

ct_mAb_LC_duration <- CER_func(cases_mAb_bc, p_pneum, p_inpatient, 
                         CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, CI_di_yrs[1], p_mAb_bc,
                         num_infants_eligible_mAb, cost_hosp, cost_outpatient)  

ct_mAb_UC_duration <- CER_func(cases_mAb_bc, p_pneum, p_inpatient, 
                         CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, CI_di_yrs[2], p_mAb_bc,
                         num_infants_eligible_mAb, cost_hosp, cost_outpatient)  

ct_mAb_LC_cost_hosp <- CER_func(cases_mAb_bc, p_pneum, p_inpatient, 
                          CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mAb_bc,
                          num_infants_eligible_mAb, CI_costs_inpatient[1], cost_outpatient)  

ct_mAb_UC_cost_hosp <- CER_func(cases_mAb_bc, p_pneum, p_inpatient, 
                          CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mAb_bc,
                          num_infants_eligible_mAb, CI_costs_inpatient[2], cost_outpatient)

ct_mAb_LC_cost_outpat <- CER_func(cases_mAb_bc, p_pneum, p_inpatient, 
                            CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mAb_bc,
                            num_infants_eligible_mAb, cost_hosp, CI_costs_outpatient[1])  

ct_mAb_UC_cost_outpat <- CER_func(cases_mAb_bc, p_pneum, p_inpatient, 
                            CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mAb_bc,
                            num_infants_eligible_mAb, cost_hosp, CI_costs_outpatient[2])  

# create tornado plot vector for mAb
ct_list_mAb <- c(ct_mAb_LC_cost_outpat, ct_mAb_UC_cost_outpat,
                 ct_mAb_LC_dw_LRTI_severe, ct_mAb_UC_dw_LRTI_severe, ct_mAb_LC_dw_LRTI_mod, ct_mAb_UC_dw_LRTI_mod,
                 ct_mAb_LC_duration, ct_mAb_UC_duration, ct_mAb_LC_cost_hosp, ct_mAb_UC_cost_hosp, ct_mAb_LC_AR3, ct_mAb_UC_AR3, ct_mAb_LC_eff, ct_mAb_UC_eff,
                 ct_mAb_LC_AR6, ct_mAb_UC_AR6, ct_mAb_LC_pneum, ct_mAb_UC_pneum, 
                 ct_mAb_LC_inpat, ct_mAb_UC_inpat, ct_mAb_LC_CFR, ct_mAb_UC_CFR)

ct_mat_mAb <- matrix(ct_list_mAb, 2, 11)


# llAb ICER calculations

ct_llAb_bc <- CER_func(cases_llAb_bc, p_pneum, p_inpatient,CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod,
                 di_yrs, p_llAb_bc, num_infants_eligible_llAb, cost_hosp, cost_outpatient)

ct_llAb_LC_eff <- CER_func(cases_llAb_LC_eff , p_pneum, p_inpatient, 
                 CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_llAb_bc,
                 num_infants_eligible_llAb, cost_hosp, cost_outpatient)

ct_llAb_UC_eff <- CER_func(cases_llAb_UC_eff, p_pneum, p_inpatient, 
                     CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_llAb_bc,
                     num_infants_eligible_llAb, cost_hosp, cost_outpatient)

ct_llAb_LC_AR3 <- CER_AR(cases_no_LC_AR3, cases_llAb_LC_AR3 , p_pneum, p_inpatient, 
                     CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_llAb_bc,
                     num_infants_eligible_llAb, cost_hosp, cost_outpatient)

ct_llAb_UC_AR3 <- CER_AR(cases_no_UC_AR3, cases_llAb_UC_AR3, p_pneum, p_inpatient, 
                     CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_llAb_bc,
                     num_infants_eligible_llAb, cost_hosp, cost_outpatient)

ct_llAb_LC_AR6 <- CER_AR(cases_no_LC_AR6, cases_llAb_LC_AR6 , p_pneum, p_inpatient, 
                        CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_llAb_bc,
                        num_infants_eligible_llAb, cost_hosp, cost_outpatient)

ct_llAb_UC_AR6 <- CER_AR(cases_no_UC_AR6, cases_llAb_UC_AR6, p_pneum, p_inpatient, 
                        CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_llAb_bc,
                        num_infants_eligible_llAb, cost_hosp, cost_outpatient)

ct_llAb_LC_pneum <-CER_func(cases_llAb_bc, CI_LRTI[1], p_inpatient, 
                       CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_llAb_bc,
                       num_infants_eligible_llAb, cost_hosp, cost_outpatient)

ct_llAb_UC_pneum <- CER_func(cases_llAb_bc, CI_LRTI[2], p_inpatient, 
                       CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_llAb_bc,
                       num_infants_eligible_llAb, cost_hosp, cost_outpatient)

ct_llAb_LC_inpatient <-CER_func(cases_llAb_bc, p_pneum, CI_inpat[1], 
                          CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_llAb_bc,
                          num_infants_eligible_llAb, cost_hosp, cost_outpatient)

ct_llAb_UC_inpatient <- CER_func(cases_llAb_bc, p_pneum, CI_inpat[2], 
                           CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_llAb_bc,
                           num_infants_eligible_llAb, cost_hosp, cost_outpatient)

ct_llAb_LC_CFR <- CER_func(cases_llAb_bc, p_pneum, p_inpatient, 
                           CI_CFR[1], dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_llAb_bc,
                           num_infants_eligible_llAb, cost_hosp, cost_outpatient)

ct_llAb_UC_CFR <- CER_func(cases_llAb_bc, p_pneum, p_inpatient, 
                     CI_CFR[2], dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_llAb_bc,
                     num_infants_eligible_llAb, cost_hosp, cost_outpatient)

ct_llAb_LC_dw_LRTI_severe <- CER_func(cases_llAb_bc, p_pneum, p_inpatient, 
                           CFR_inpatient, CI_dw_LRTI_severe[1], dw_LRTI_mod, di_yrs, p_llAb_bc,
                           num_infants_eligible_llAb, cost_hosp, cost_outpatient)

ct_llAb_UC_dw_LRTI_severe <- CER_func(cases_llAb_bc, p_pneum, p_inpatient,
                           CFR_inpatient, CI_dw_LRTI_severe[2], dw_LRTI_mod, di_yrs, p_llAb_bc,
                           num_infants_eligible_llAb, cost_hosp, cost_outpatient)

ct_llAb_LC_dw_LRTI_mod <- CER_func(cases_llAb_bc, p_pneum, p_inpatient, 
                             CFR_inpatient, dw_LRTI_severe, CI_dw_LRTI_mod[1], di_yrs, p_llAb_bc,
                             num_infants_eligible_llAb, cost_hosp, cost_outpatient)

ct_llAb_UC_dw_LRTI_mod <- CER_func(cases_llAb_bc, p_pneum, p_inpatient,
                             CFR_inpatient, dw_LRTI_severe, CI_dw_LRTI_mod[2], di_yrs, p_llAb_bc,
                             num_infants_eligible_llAb, cost_hosp, cost_outpatient)

ct_llAb_LC_di_yrs <- CER_func(cases_llAb_bc, p_pneum, p_inpatient,
                        CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, CI_di_yrs[1], p_llAb_bc,
                        num_infants_eligible_llAb, cost_hosp, cost_outpatient)

ct_llAb_UC_di_yrs <- CER_func(cases_llAb_bc, p_pneum, p_inpatient, 
                        CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, CI_di_yrs[2], p_llAb_bc,
                        num_infants_eligible_llAb, cost_hosp, cost_outpatient)

ct_llAb_LC_cost_hosp <- CER_func(cases_llAb_bc, p_pneum, p_inpatient,
                           CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_llAb_bc,
                           num_infants_eligible_llAb, CI_costs_inpatient[1], cost_outpatient)

ct_llAb_UC_cost_hosp <- CER_func(cases_llAb_bc, p_pneum, p_inpatient,
                           CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_llAb_bc,
                           num_infants_eligible_llAb, CI_costs_outpatient[2], cost_outpatient)

ct_llAb_LC_cost_out <- CER_func(cases_llAb_bc, p_pneum, p_inpatient, 
                          CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_llAb_bc,
                          num_infants_eligible_llAb, cost_hosp, CI_costs_outpatient[1])

ct_llAb_UC_cost_out <- CER_func(cases_llAb_bc, p_pneum, p_inpatient,
                          CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_llAb_bc,
                          num_infants_eligible_llAb, cost_hosp, CI_costs_outpatient[2])

# create tornado plot vector for llAb
ct_list_llAb <- c(ct_llAb_LC_cost_out, ct_llAb_UC_cost_out,
                 ct_llAb_LC_dw_LRTI_severe, ct_llAb_UC_dw_LRTI_severe,ct_llAb_LC_dw_LRTI_mod, ct_llAb_UC_dw_LRTI_mod,
                 ct_llAb_LC_di_yrs, ct_llAb_UC_di_yrs, ct_llAb_LC_cost_hosp, ct_llAb_UC_cost_hosp, ct_llAb_LC_AR3, ct_llAb_UC_AR3, ct_llAb_LC_eff, ct_llAb_UC_eff,
                 ct_llAb_LC_AR6, ct_llAb_UC_AR6, ct_llAb_LC_pneum, ct_llAb_UC_pneum, 
                 ct_llAb_LC_inpatient, ct_llAb_UC_inpatient, ct_llAb_LC_CFR, ct_llAb_UC_CFR)

ct_mat_llAb <- matrix(ct_list_llAb, 2, 11)


# mVax ICER calculations

ct_mVax_bc <- CER_func(cases_mVax_bc, p_pneum, p_inpatient, CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod,
                 di_yrs, p_mVax_bc, sum(num_infants), cost_hosp, cost_outpatient)
  
ct_mVax_LC_eff <- CER_func(cases_mVax_LC_eff, p_pneum, p_inpatient, 
                 CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mVax_bc,
                 sum(num_infants), cost_hosp, cost_outpatient)

ct_mVax_UC_eff <- CER_func(cases_mVax_UC_eff, p_pneum, p_inpatient, 
                 CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mVax_bc,
                 sum(num_infants), cost_hosp, cost_outpatient)

ct_mVax_LC_AR3 <- CER_AR(cases_no_LC_AR3, cases_mVax_LC_AR3, p_pneum, p_inpatient, 
                     CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mVax_bc,
                     sum(num_infants), cost_hosp, cost_outpatient)

ct_mVax_UC_AR3 <- CER_AR(cases_no_UC_AR3, cases_mVax_UC_AR3, p_pneum, p_inpatient, 
                     CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mVax_bc,
                     sum(num_infants), cost_hosp, cost_outpatient)

ct_mVax_LC_AR6 <- CER_AR(cases_no_LC_AR6, cases_mVax_LC_AR6, p_pneum, p_inpatient, 
                        CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mVax_bc,
                        sum(num_infants), cost_hosp, cost_outpatient)

ct_mVax_UC_AR6 <- CER_AR(cases_no_UC_AR6, cases_mVax_UC_AR6, p_pneum, p_inpatient, 
                        CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mVax_bc,
                        sum(num_infants), cost_hosp, cost_outpatient)

ct_mVax_LC_pneum <- CER_func(cases_mVax_bc, CI_LRTI[1], p_inpatient, 
                      CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mVax_bc,
                      sum(num_infants), cost_hosp, cost_outpatient)

ct_mVax_UC_pneum <- CER_func(cases_mVax_bc, CI_LRTI[2], p_inpatient, 
                       CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mVax_bc,
                       sum(num_infants), cost_hosp, cost_outpatient)

ct_mVax_LC_inpatient <- CER_func(cases_mVax_bc, p_pneum, CI_inpat[1], 
                          CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mVax_bc,
                          sum(num_infants), cost_hosp, cost_outpatient)

ct_mVax_UC_inpatient <- CER_func(cases_mVax_bc, p_pneum, CI_inpat[2], 
                           CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mVax_bc,
                           sum(num_infants), cost_hosp, cost_outpatient)

ct_mVax_LC_CFR <- CER_func(cases_mVax_bc, p_pneum, p_inpatient, 
                     CI_CFR[1], dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mVax_bc,
                     sum(num_infants), cost_hosp, cost_outpatient)

ct_mVax_UC_CFR <- CER_func(cases_mVax_bc, p_pneum, p_inpatient, 
                     CI_CFR[2], dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mVax_bc,
                     sum(num_infants), cost_hosp, cost_outpatient)

ct_mVax_LC_dw_LRTI_severe <- CER_func(cases_mVax_bc, p_pneum, p_inpatient,
                           CFR_inpatient, CI_dw_LRTI_severe[1], dw_LRTI_mod, di_yrs, p_mVax_bc,
                           sum(num_infants), cost_hosp, cost_outpatient)

ct_mVax_UC_dw_LRTI_severe <- CER_func(cases_mVax_bc, p_pneum, p_inpatient, 
                           CFR_inpatient, CI_dw_LRTI_severe[2], dw_LRTI_mod, di_yrs, p_mVax_bc,
                           sum(num_infants), cost_hosp, cost_outpatient)

ct_mVax_LC_dw_LRTI_mod <- CER_func(cases_mVax_bc, p_pneum, p_inpatient, 
                             CFR_inpatient, dw_LRTI_severe, CI_dw_LRTI_mod[1], di_yrs, p_mVax_bc,
                             sum(num_infants), cost_hosp, cost_outpatient)

ct_mVax_UC_dw_LRTI_mod <- CER_func(cases_mVax_bc, p_pneum, p_inpatient, 
                             CFR_inpatient, dw_LRTI_severe, CI_dw_LRTI_mod[2], di_yrs, p_mVax_bc,
                             sum(num_infants), cost_hosp, cost_outpatient)

ct_mVax_LC_di_yrs <- CER_func(cases_mVax_bc, p_pneum, p_inpatient, 
                        CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, CI_di_yrs[1], p_mVax_bc,
                        sum(num_infants), cost_hosp, cost_outpatient)

ct_mVax_UC_di_yrs <- CER_func(cases_mVax_bc, p_pneum, p_inpatient, 
                        CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, CI_di_yrs[2], p_mVax_bc,
                        sum(num_infants), cost_hosp, cost_outpatient)

ct_mVax_LC_cost_hosp <- CER_func(cases_mVax_bc, p_pneum, p_inpatient, 
                           CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mVax_bc,
                           sum(num_infants), CI_costs_inpatient[1], cost_outpatient)

ct_mVax_UC_cost_hosp <- CER_func(cases_mVax_bc, p_pneum, p_inpatient,
                           CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mVax_bc,
                           sum(num_infants), CI_costs_inpatient[2], cost_outpatient)

ct_mVax_LC_cost_out <- CER_func(cases_mVax_bc, p_pneum, p_inpatient, 
                          CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mVax_bc,
                          sum(num_infants), cost_hosp, CI_costs_outpatient[1])

ct_mVax_UC_cost_out <- CER_func(cases_mVax_bc, p_pneum, p_inpatient, 
                          CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mVax_bc,
                          sum(num_infants), cost_hosp, CI_costs_outpatient[2])

# create tornado plot vector for mVax
ct_list_mVax <- c(ct_mVax_LC_cost_out, ct_mVax_UC_cost_out,
                  ct_mVax_LC_dw_LRTI_severe, ct_mVax_UC_dw_LRTI_severe,ct_mVax_LC_dw_LRTI_mod, ct_mVax_UC_dw_LRTI_mod,
                  ct_mVax_LC_di_yrs, ct_mVax_UC_di_yrs, ct_mVax_LC_cost_hosp, ct_mVax_UC_cost_hosp, ct_mVax_LC_AR3, ct_mVax_UC_AR3,
                  ct_mVax_LC_eff, ct_mVax_UC_eff, ct_mVax_LC_AR6, ct_mVax_UC_AR6, ct_mVax_LC_pneum, ct_mVax_UC_pneum, 
                  ct_mVax_LC_inpatient, ct_mVax_UC_inpatient, ct_mVax_LC_CFR, ct_mVax_UC_CFR)

ct_mat_mVax <- matrix(ct_list_mVax, 2, 11)


################################################################################################################

## ICER for PPC mVax
ct_PPC_mVax <- CER_func(cases_PPC_mVax_bc, p_pneum, p_inpatient,
                  CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mVax_bc,
                  num_infants_eligible_mVax, cost_hosp, cost_outpatient)
####

## ICER for ResVax™ using complete dataset from Novavax Prepare Trial
ct_CmVax <- CER_func(cases_CmVax_bc, p_pneum, p_inpatient,
                  CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mVax_bc,
                  num_infants_eligible_mVax, cost_hosp, cost_outpatient)

###

## ICER for ResVax™ when given as pre-seasonal administration as opposed to year-round
ct_ps_mVax_bc <- CER_func(cases_ps_mVax_bc, p_pneum, p_inpatient,
                    CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod, di_yrs, p_mVax_bc,
                    num_eligible_ps_mVax, cost_hosp, cost_outpatient)

####

## ICER for scenario analysis where we include RSV URTI clinical pathway
# 

CER_URTI <- function(cases, p_LRTI, p_inpat, p_out_URTI, CFR, dw_s, dw_m, duration_ill, p_int, num_eligible, cost_inpat, cost_out) {
  cases_no <- SA_mAbcov_cases[1]
  YLL <- YLL_pc - 7/12
  int_cost <- 3 + adcost_llAb
  med_cost <- (cost_inpat * p_LRTI * p_inpat * cases) + (cost_out * cases * p_LRTI * (1- p_inpat) + (cost_out * cases * (1- p_LRTI) * p_out_URTI))
  medcost_no <- (cost_inpat * p_LRTI * p_inpat * cases_no) + (cost_out * cases_no * p_LRTI * (1- p_inpat) + (cost_out * cases_no * (1- p_LRTI) *p_out_URTI)) 
  mort_no <- (cases_no * p_LRTI * p_inpat * p_seek_care * CFR) + (cases_no * p_LRTI * p_inpat * (1-p_seek_care) * CFR/0.51*0.49)
  YLL_calc_no <- mort_no * YLL
  YLD_calc_no <- (((cases_no * p_LRTI) - mort_no) * p_inpat * duration_ill * dw_s) + (((cases_no * p_LRTI) - mort_no) * ((1 - p_inpat) * duration_ill * dw_m)) +
    (cases_no * (1 - p_LRTI) * p_out_URTI * duration_ill * dw_m)
  DALYs_no <- YLL_calc_no + YLD_calc_no
  mort <- (cases * p_LRTI * p_inpat * p_seek_care * CFR) + (cases * p_LRTI * p_inpat * (1-p_seek_care) * CFR/0.51*0.49)
  YLL_calc <- mort * YLL
  YLD_calc <- (((cases * p_LRTI) - mort_no) * p_inpat * duration_ill * dw_s) + (((cases * p_LRTI) - mort_no) * ((1 - p_inpat) * duration_ill * dw_m)) +
    (cases * (1 - p_LRTI) * p_out_URTI * duration_ill * dw_m)
  DALYs_lost <- YLL_calc + YLD_calc
  CER <- (med_cost + (num_eligible * p_int * int_cost) - medcost_no) / (DALYs_no - DALYs_lost)
}

ct_URTI_mAb_bc <- CER_URTI(cases_mAb_bc, p_pneum, p_inpatient, p_outpat_febARI, CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod,
                            di_yrs, p_mAb_bc, num_infants_eligible_mAb, cost_hosp, cost_outpatient)
ct_URTI_llAb_bc <- CER_URTI(cases_llAb_bc, p_pneum, p_inpatient, p_outpat_febARI, CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod,
                            di_yrs, p_llAb_bc, num_infants_eligible_llAb, cost_hosp, cost_outpatient)
ct_URTI_mVax_bc <- CER_URTI(cases_mVax_bc, p_pneum, p_inpatient, p_outpat_febARI, CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod,
                            di_yrs, p_mVax_bc, num_infants_eligible_mVax, cost_hosp, cost_outpatient)
###

## ICER for scenario analysis where are children who require care receive it

CER_cSA <- function(cases, p_LRTI, p_inpat, CFR, dw_s, dw_m, duration_ill, p_int, num_eligible, cost_inpat, cost_out) {
  cases_no <- SA_mAbcov_cases[1]
  YLL <- YLL_pc - 7/12
  int_cost <- 3 + adcost_llAb
  med_cost <- (cost_inpat * p_LRTI * p_inpat * cases) + (cost_out * cases * p_LRTI * (1- p_inpat))
  medcost_no <- (cost_inpat * p_LRTI * p_inpat * cases_no) + (cost_out * cases_no * p_LRTI * (1- p_inpat))
  mort_no <- (cases_no * p_LRTI * p_inpat * CFR)
  YLL_calc_no <- mort_no * YLL
  YLD_calc_no <- (((cases_no * p_LRTI) - mort_no) * p_inpat * duration_ill * dw_s) + (((cases_no * p_LRTI) - mort_no) * ((1 - p_inpat) * duration_ill * dw_m))
  DALYs_no <- YLL_calc_no + YLD_calc_no 
  mort <- (cases * p_LRTI * p_inpat * CFR)
  YLL_calc <- mort * YLL
  YLD_calc <- (((cases * p_LRTI) - mort_no) * p_inpat * duration_ill * dw_s) + (((cases * p_LRTI) - mort_no) * ((1 - p_inpat) * duration_ill * dw_m))
  DALYs_lost <- YLL_calc + YLD_calc
  CER <- (med_cost + (num_eligible * p_int * int_cost) - medcost_no) / (DALYs_no - DALYs_lost)
}

ct_cSA_mAb_bc <- CER_cSA(cases_mAb_bc, p_pneum, p_inpatient, CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod,
                        di_yrs, p_mAb_bc, num_infants_eligible_mAb, cost_hosp, cost_outpatient)

ct_cSA_llAb_bc <- CER_cSA(cases_llAb_bc, p_pneum, p_inpatient, CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod,
                         di_yrs, p_llAb_bc, num_infants_eligible_llAb, cost_hosp, cost_outpatient)

ct_cSA_mVax_bc <- CER_cSA(cases_mVax_bc, p_pneum, p_inpatient, CFR_inpatient, dw_LRTI_severe, dw_LRTI_mod,
                         di_yrs, p_mVax_bc, num_infants_eligible_mVax, cost_hosp, cost_outpatient)

## ICER for alternate CFR analysis
PERCH_trad_mAb_bc <- CER_func(cases_mAb_bc, p_pneum, p_inpatient, CFR_inpatient_PERCH_trad, dw_LRTI_severe, dw_LRTI_mod,
                      di_yrs, p_mAb_bc, num_infants_eligible_mAb, cost_hosp, cost_outpatient)

buchwald_mAb_bc <- CER_func(cases_mAb_bc, p_pneum, p_inpatient, CFR_inpatient_buchwald, dw_LRTI_severe, dw_LRTI_mod,
                                di_yrs, p_mAb_bc, num_infants_eligible_mAb, cost_hosp, cost_outpatient)


  