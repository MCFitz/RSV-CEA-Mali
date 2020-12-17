## 
source("economic_outcomes.R")

# Scenario analysis for WHO preferred product characteristics (PPCs) of mVax
# Efficacy
PPC_mVax <- 0.70

# cohort matrix for PPC mVax efficacy
PPC_mVax_dur <- empty_cohort
PPC_mVax_month <- 4 # PPC: at least 4 months duration
for (i in 1:12) {
  PPC_mVax_dur[i, i:(i+(PPC_mVax_month-1))] <- 1
}
PPC_mVax_eff <- PPC_mVax_dur* PPC_mVax

# PPC mVax cases calculation
pd_PPC_mVax_mat_bc <- pd_calc(PPC_mVax_eff, p_mVax_bc, AR_bc, PPC_mVax_dur)
cases_PPC_mVax_bc <- RSVcases(pd_PPC_mVax_mat_bc, num_infants)

PPC_pd_mVax_array <- array(NA, dim = c(dim(AR_bc)[1], dim(AR_bc)[2], trials))
for (m in 1:trials) {
  PPC_pd_mVax_array[,,m] <- pd_calc(PPC_mVax_eff, p_mVax_bc, AR_u[,,m], PPC_mVax_dur)
} 

PPC_cases_mVax_u <- apply(PPC_pd_mVax_array, 3, RSVcases, babies = num_infants)

# PPC prob pneumonia
PPC_pneum_mVax_bc <- pneum_func(p_pneum, cases_PPC_mVax_bc)
PPC_pneum_mVax_u <- pneum_func(p_pneum_u, PPC_cases_mVax_u)
# PPC inpatient care
PPC_inpatient_mVax_bc <- inpat_func(p_inpatient, PPC_pneum_mVax_bc)
PPC_inpatient_mVax_u <- inpat_func(p_inpatient_u, PPC_pneum_mVax_u)
# PPC appropriate level of care not received
PPC_nr_care_mVax_bc <- nr_care_func(p_inpatient, PPC_pneum_mVax_bc)
PPC_nr_care_mVax_u <- nr_care_func(p_inpatient_u, PPC_pneum_mVax_u)
# PPC outpatient care
PPC_outpat_mVax_bc <- outpat_func(p_inpatient, PPC_pneum_mVax_bc)
PPC_outpat_mVax_u <- outpat_func(p_inpatient_u, PPC_pneum_mVax_u)
# PPC mortality
PPC_mortality_mVax_bc <- mort_inpat_func(CFR_inpatient, PPC_inpatient_mVax_bc, CFR_nr_care, PPC_nr_care_mVax_bc)
PPC_mortality_mVax_u <- mort_inpat_func(CFR_inpatient_u, PPC_inpatient_mVax_u, CFR_nr_care_u, PPC_nr_care_mVax_u)
# PPC med costs
PPC_medcost_mVax_bc <- medcost_func(cost_hosp, PPC_inpatient_mVax_bc, cost_outpatient, PPC_outpat_mVax_bc)
PPC_medcost_mVax_u <- medcost_func(cost_hosp_u, PPC_inpatient_mVax_u, cost_outpatient_u, PPC_outpat_mVax_u)
# PPC_total costs for bc
PPC_totalcost_mVax_bc <- progcost_mVax_bc + PPC_medcost_mVax_bc
# PPC YLL
PPC_YLL_mVax_bc <- YLL_func(PPC_mortality_mVax_bc)
PPC_YLL_mVax_u <- YLL_func(PPC_mortality_mVax_u)
# PPC YLD
PPC_YLD_mVax_bc <- YLD_func(PPC_inpatient_mVax_bc, PPC_mortality_mVax_bc, di_yrs, dw_LRTI_severe, PPC_pneum_mVax_bc, dw_LRTI_mod)
PPC_YLD_mVax_u <- YLD_func(PPC_inpatient_mVax_u, PPC_mortality_mVax_u, di_yrs_u, dw_LRTI_severe_u, PPC_pneum_mVax_u, dw_LRTI_mod_u)
# PPC DALYs
PPC_DALY_mVax_bc <- PPC_YLL_mVax_bc + PPC_YLD_mVax_bc
PPC_DALY_mVax_u <- PPC_YLL_mVax_u + PPC_YLD_mVax_u
# PPC DALYs saved
PPC_DALY_saved_mVax_bc <- D_saved_func(DALY_mVax[1], PPC_DALY_mVax_bc)
PPC_DALY_saved_mVax_u <- D_saved_func(DALY_no_u, PPC_DALY_mVax_u)

# calculations for each intervention under alternate care-seeking/access scenario, where every child who requires care receives it and deaths only occur in hospitalized patients
# cSA mAb
inpatient_cSA_mAb_u <- inpat_cSA_func(p_inpatient_u, pneum_mAb_u)
mortality_cSA_mAb_u <- mort_cSA_func(CFR_inpatient_u, inpatient_cSA_mAb_u)
medcost_cSA_mAb_u <- medcost_func(cost_hosp_u, inpatient_mAb_u, cost_outpatient_u, outpat_mAb_u)
YLL_cSA_mAb_u <- YLL_func(mortality_cSA_mAb_u)
YLD_cSA_mAb_u <- YLD_func(inpatient_cSA_mAb_u, mortality_cSA_mAb_u, di_yrs_u, dw_LRTI_severe_u, pneum_mAb_u, dw_LRTI_mod_u)
DALY_cSA_mAb_u <- YLL_cSA_mAb_u + YLD_cSA_mAb_u
DALY_saved_cSA_mAb_u <- D_saved_func(DALY_cSA_no_u, DALY_cSA_mAb_u)

# cSA llAb
inpatient_cSA_llAb_u <- inpat_cSA_func(p_inpatient_u, pneum_llAb_u)
mortality_cSA_llAb_u <- mort_cSA_func(CFR_inpatient_u, inpatient_cSA_llAb_u)
medcost_cSA_llAb_u <- medcost_func(cost_hosp_u, inpatient_llAb_u, cost_outpatient_u, outpat_llAb_u)
YLL_cSA_llAb_u <- YLL_func(mortality_cSA_llAb_u)
YLD_cSA_llAb_u <- YLD_func(inpatient_cSA_llAb_u, mortality_cSA_llAb_u, di_yrs_u, dw_LRTI_severe_u, pneum_llAb_u, dw_LRTI_mod_u)
DALY_cSA_llAb_u <- YLL_cSA_llAb_u + YLD_cSA_llAb_u
DALY_saved_cSA_llAb_u <- D_saved_func(DALY_cSA_no_u, DALY_cSA_llAb_u)

# cSA mVax
inpatient_cSA_mVax_u <- inpat_cSA_func(p_inpatient_u, pneum_mVax_u)
mortality_cSA_mVax_u <- mort_cSA_func(CFR_inpatient_u, inpatient_cSA_mVax_u)
medcost_cSA_mVax_u <- medcost_func(cost_hosp_u, inpatient_mVax_u, cost_outpatient_u, outpat_mVax_u)
YLL_cSA_mVax_u <- YLL_func(mortality_cSA_mVax_u)
YLD_cSA_mVax_u <- YLD_func(inpatient_cSA_mVax_u, mortality_cSA_mVax_u, di_yrs_u, dw_LRTI_severe_u, pneum_mVax_u, dw_LRTI_mod_u)
DALY_cSA_mVax_u <- YLL_cSA_mVax_u + YLD_cSA_mVax_u
DALY_saved_cSA_mVax_u <- D_saved_func(DALY_cSA_no_u, DALY_cSA_mVax_u)

# calculations for each intervention when RSV-URTI is included in the outcome tree

# URTI mAb
pneum_URTI_mAb_u <- pneum_func(p_pneum_u, cases_mAb_u)
inpatient_URTI_mAb_u <- inpat_func(p_inpatient_u, pneum_URTI_mAb_u)
nr_care_URTI_mAb_u <- nr_care_func(p_inpatient_u, pneum_URTI_mAb_u)
febARI_mAb_u <- fARI_func(p_pneum_u, cases_mAb_u)
outpat_URTI_mAb_u <- outpat_URTI_func (p_inpatient_u, pneum_URTI_mAb_u, febARI_mAb_u)
mortality_URTI_mAb_u <- mort_inpat_func(CFR_inpatient_u, inpatient_URTI_mAb_u, CFR_nr_care_u, nr_care_URTI_mAb_u)
medcost_URTI_mAb_u <-  medcost_func(cost_hosp_u, inpatient_URTI_mAb_u, cost_outpatient_u, outpat_URTI_mAb_u)
YLL_URTI_mAb_u <- YLL_func(mortality_URTI_mAb_u)
YLD_URTI_mAb_u <- YLD_URTI_func(inpatient_URTI_mAb_u, mortality_URTI_mAb_u, di_yrs_u, dw_LRTI_severe_u, pneum_URTI_mAb_u, dw_LRTI_mod_u, febARI_mAb_u, dw_URTI_mod_u, dw_URTI_mild_u)
DALY_URTI_mAb_u <- YLL_URTI_mAb_u + YLD_URTI_mAb_u
DALY_saved_URTI_mAb_u <- D_saved_func(DALY_URTI_no_u, DALY_URTI_mAb_u)

# URTI llAb
pneum_URTI_llAb_u <- pneum_func(p_pneum_u, cases_llAb_u)
inpatient_URTI_llAb_u <- inpat_func(p_inpatient_u, pneum_URTI_llAb_u)
nr_care_URTI_llAb_u <- nr_care_func(p_inpatient_u, pneum_URTI_llAb_u)
febARI_llAb_u <- fARI_func(p_pneum_u, cases_llAb_u)
outpat_URTI_llAb_u <- outpat_URTI_func (p_inpatient_u, pneum_URTI_llAb_u, febARI_llAb_u)
mortality_URTI_llAb_u <- mort_inpat_func(CFR_inpatient_u, inpatient_URTI_llAb_u, CFR_nr_care_u, nr_care_URTI_llAb_u)
medcost_URTI_llAb_u <-  medcost_func(cost_hosp_u, inpatient_URTI_llAb_u, cost_outpatient_u, outpat_URTI_llAb_u)
YLL_URTI_llAb_u <- YLL_func(mortality_URTI_llAb_u)
YLD_URTI_llAb_u <- YLD_URTI_func(inpatient_URTI_llAb_u, mortality_URTI_llAb_u, di_yrs_u, dw_LRTI_severe_u, pneum_URTI_llAb_u, dw_LRTI_mod_u, febARI_llAb_u, dw_URTI_mod_u, dw_URTI_mild_u)
DALY_URTI_llAb_u <- YLL_URTI_llAb_u + YLD_URTI_llAb_u
DALY_saved_URTI_llAb_u <- D_saved_func(DALY_URTI_no_u, DALY_URTI_llAb_u)

# URTI mVax
pneum_URTI_mVax_u <- pneum_func(p_pneum_u, cases_mVax_u)
inpatient_URTI_mVax_u <- inpat_func(p_inpatient_u, pneum_URTI_mVax_u)
nr_care_URTI_mVax_u <- nr_care_func(p_inpatient_u, pneum_URTI_mVax_u)
febARI_mVax_u <- fARI_func(p_pneum_u, cases_mVax_u)
outpat_URTI_mVax_u <- outpat_URTI_func (p_inpatient_u, pneum_URTI_mVax_u, febARI_mVax_u)
mortality_URTI_mVax_u <- mort_inpat_func(CFR_inpatient_u, inpatient_URTI_mVax_u, CFR_nr_care_u, nr_care_URTI_mVax_u)
medcost_URTI_mVax_u <-  medcost_func(cost_hosp_u, inpatient_URTI_mVax_u, cost_outpatient_u, outpat_URTI_mVax_u)
YLL_URTI_mVax_u <- YLL_func(mortality_URTI_mVax_u)
YLD_URTI_mVax_u <- YLD_URTI_func(inpatient_URTI_mVax_u, mortality_URTI_mVax_u, di_yrs_u, dw_LRTI_severe_u, pneum_URTI_mVax_u, dw_LRTI_mod_u, febARI_mVax_u, dw_URTI_mod_u, dw_URTI_mild_u)
DALY_URTI_mVax_u <- YLL_URTI_mVax_u + YLD_URTI_mVax_u
DALY_saved_URTI_mVax_u <- D_saved_func(DALY_URTI_no_u, DALY_URTI_mVax_u)


# ResVax pre-seasonal campaign scenario analysis

ps_mVax_admin <- mVax_admin
for (i in 1:sim_mo) {
  for(h in 1:12){
    if(i <5 | i > 10){
      ps_mVax_admin[h,i] <-0
    }}}

ps_mVax_mat <- empty_cohort
for (i in 1:sim_mo) {
  if (i > 4 & i < 11){
    ps_mVax_mat[i, i:(i+(mVax_months-1))] <- 1
  }}

ps_mVax_eff_mat <- ps_mVax_mat * mVax_eff_bc
num_eligible_ps_mVax <- sum(ps_mVax_admin * num_infants)
pd_ps_mVax_bc <- pd_calc(ps_mVax_eff_mat, p_mVax_bc, AR_bc, ps_mVax_mat)
cases_ps_mVax_bc <- RSVcases(pd_ps_mVax_bc, num_infants)

ps_pd_mVax_array <- array(NA, dim = c(dim(AR_bc)[1], dim(AR_bc)[2], trials))
for (pm in 1:trials) {
  ps_pd_mVax_array[,,pm] <- pd_calc(eff_mVax[pm], p_mVax_bc, AR_u[,,pm], ps_mVax_mat)
} 
ps_cases_mVax_u <- apply(ps_pd_mVax_array, 3, RSVcases, babies = num_infants)

pneum_ps_mVax_u <- pneum_func(p_pneum_u, ps_cases_mVax_u)
inpatient_ps_mVax_u <- inpat_func(p_inpatient_u, pneum_ps_mVax_u)
nr_care_ps_mVax_u <- nr_care_func(p_inpatient_u, pneum_ps_mVax_u)
outpat_ps_mVax_u <- outpat_func(p_inpatient_u, pneum_ps_mVax_u)
mortality_ps_mVax_u <- mort_inpat_func(CFR_inpatient_u, inpatient_ps_mVax_u, CFR_nr_care_u, nr_care_ps_mVax_u)
medcost_ps_mVax_u <- medcost_func(cost_hosp_u, inpatient_ps_mVax_u, cost_outpatient_u, outpat_ps_mVax_u)
YLL_ps_mVax_u <- YLL_func(mortality_ps_mVax_u)
YLD_ps_mVax_u <- YLD_func(inpatient_ps_mVax_u, mortality_ps_mVax_u, di_yrs_u, dw_LRTI_severe_u, pneum_ps_mVax_u, dw_LRTI_mod_u)
DALY_ps_mVax_u <- YLL_ps_mVax_u + YLD_ps_mVax_u
DALY_saved_ps_mVax_u <- D_saved_func(DALY_no_u, DALY_ps_mVax_u)

# ResVax complete Prepare™ trial dataset

CmVax_eff_mat <- mVax_dur * CmVax_eff_bc                               # efficacy matrix
pd_CmVax_bc <- pd_calc(CmVax_eff_mat, p_mVax_bc, AR_bc, mVax_dur)     # probability of disease calculation point estimate
cases_CmVax_bc <- RSVcases(pd_CmVax_bc, num_infants)                       # calculate number of cases point estimate

cd_pd_mVax_array <- array(NA, dim = c(dim(AR_bc)[1], dim(AR_bc)[2], trials))
for (cd in 1:trials) {
  cd_pd_mVax_array[,,cd] <- pd_calc(eff_CmVax[cd], p_mVax_bc, AR_u[,,cd], mVax_dur)
} 
cd_cases_mVax_u <- apply(cd_pd_mVax_array, 3, RSVcases, babies = num_infants)

pneum_CmVax_u <- pneum_func(p_pneum_u, cd_cases_mVax_u)
inpatient_CmVax_u <- inpat_func(p_inpatient_u, pneum_CmVax_u)
nr_care_CmVax_u <- nr_care_func(p_inpatient_u, pneum_CmVax_u)
outpat_CmVax_u <- outpat_func(p_inpatient_u, pneum_CmVax_u)
mortality_CmVax_u <- mort_inpat_func(CFR_inpatient_u, inpatient_CmVax_u, CFR_nr_care_u, nr_care_CmVax_u)
medcost_CmVax_u <- medcost_func(cost_hosp_u, inpatient_CmVax_u, cost_outpatient_u, outpat_CmVax_u)
YLL_CmVax_u <- YLL_func(mortality_CmVax_u)
YLD_CmVax_u <- YLD_func(inpatient_CmVax_u, mortality_CmVax_u, di_yrs_u, dw_LRTI_severe_u, pneum_CmVax_u, dw_LRTI_mod_u)
DALY_CmVax_u <- YLL_CmVax_u + YLD_CmVax_u
DALY_saved_CmVax_u <- D_saved_func(DALY_no_u, DALY_CmVax_u)

#############################################

## build 1 year birth cohorts for llAb catch-up campaign scenario analysis

mat_year_cohort <- empty_year_cohort
for (i in 1:ft) {
  for(h in 1:12){
    if( h-i<=0){
      mat_year_cohort[h,i]<-1
      if(i-h >=12){
        mat_year_cohort[h,i] <-0
      }
    }}}

mat_year_infants <- (matrix(num_infants, 12, ncol = ft)) * mat_year_cohort

# matrix for when llAb is administered in alternate scenario analysis
# birth dose + catch-up campaign at 4wk EPI visit

# birth dose
llAb_SA_admin_pt1 <- empty_year_cohort
for (i in 1:ft) {
  if(i >2 & i < 11)
    llAb_SA_admin_pt1[i, i] <- 1
}

# catch-up campaign
llAb_SA_admin_pt2 <- empty_year_cohort
for (i in 1:ft){
  for (h in 1:12){
    if ((h<3 & i-h == 3) | h == 12 & i == 20 | h == 11 & i == 19) {
      llAb_SA_admin_pt2[h,i] <- 1
    }}}

# birth dose and catch-up campaign combined
llAb_SA_admin <- llAb_SA_admin_pt1 + llAb_SA_admin_pt2

# calculate number eligible to receive llAb in this scenario
num_eligible_llAb_SA <- sum(llAb_SA_admin * mat_year_infants)

# create matrix for when protection is provided in this scenario
mat_eff_SA_llAb_pt1 <- empty_year_cohort
for (i in 1:ft) {
  if(i >2 & i <11)
    mat_eff_SA_llAb_pt1[i, i:(i+4)] <- 1
}

mat_eff_SA_llAb_pt2 <- empty_year_cohort
for (i in 1:ft) {
  for(h in 1:12){
    if (h<3 & i-h == 3) {
      mat_eff_SA_llAb_pt2[h,i:(i+4)] <- 1}
    if (h == 12 & i == 20 | h == 11 & i == 19) {
      mat_eff_SA_llAb_pt2[h,i:(i+3)] <- 1
    }}}

mat_eff_SA_llAb <- mat_eff_SA_llAb_pt1 + mat_eff_SA_llAb_pt2
llAb_SA_eff <- mat_eff_SA_llAb * llAb_eff_bc

#### 

# calculate cases for llAb scenario analysis
pd_SA_llAb_mat_bc <- pd_calc(llAb_eff_bc, p_llAb_bc, AR_y_bc, mat_eff_SA_llAb)
cases_SA_llAb_bc <- RSVcases(pd_SA_llAb_mat_bc, num_infants)

pd_SA_llAb_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (eV in 1:length(eff_llAb)) {
  pd_SA_llAb_array[,,eV] <- pd_calc(eff_llAb[eV], p_llAb_bc, AR_y_u[,,eV], mat_eff_SA_llAb)
} 
cases_SA_llAb_u <- apply(pd_SA_llAb_array, 3, RSVcases, babies = num_infants)

# calculate cases for no intervention with 1 year follow-up time
cases_no_year_bc <- RSVcases(AR_y_bc, num_infants)
cases_no_year_u <- apply(AR_y_u, 3, RSVcases, babies = num_infants)

# calculate number who develop RSV-LRTI
pneum_no_year_bc <- pneum_func(p_pneum, cases_no_year_bc)
pneum_no_year_u <- pneum_func(p_pneum_u, cases_no_year_u)
pneum_SA_llAb_bc <- pneum_func(p_pneum, cases_SA_llAb_bc)
pneum_SA_llAb_u <- pneum_func(p_pneum_u, cases_SA_llAb_u)

# calculate number receiving inpatient care
inpatient_no_year_bc <- inpat_func(p_inpatient, pneum_no_year_bc)
inpatient_no_year_u <- inpat_func(p_inpatient_u, pneum_no_year_u)
inpatient_SA_llAb_bc <- inpat_func(p_inpatient, pneum_SA_llAb_bc)
inpatient_SA_llAb_u <- inpat_func(p_inpatient_u, pneum_SA_llAb_u)

# calculate number of infants not receiving appropriate level of care
nr_care_no_year_bc <- nr_care_func(p_inpatient, pneum_no_year_bc)
nr_care_no_year_u <-nr_care_func(p_inpatient_u, pneum_no_year_u)
nr_care_SA_llAb_bc <- nr_care_func(p_inpatient, pneum_SA_llAb_bc)
nr_care_SA_llAb_u <- nr_care_func(p_inpatient_u, pneum_SA_llAb_u)

# calculate number of infants receiving outpatient care
outpat_no_year_bc <- outpat_func(p_inpatient, pneum_no_year_bc)
outpat_no_year_u <- outpat_func(p_inpatient_u, pneum_no_year_u)
outpat_SA_llAb_bc <- outpat_func(p_inpatient, pneum_SA_llAb_bc)
outpat_SA_llAb_u <- outpat_func(p_inpatient_u, pneum_SA_llAb_u)

# calculate number of deaths
CFR_inpat_year <- 0.015
CFR_inpat_year_u <- rbeta(trials, 0.032*78, 0.429*380 - 0.032*78)

CFR_nr_year <- CFR_inpat_year/0.51 * 0.49  # 49% of infants in LMIC with RSV-LRTI die outside of inpatient care setting
CFR_nr_year_u <- CFR_inpat_year_u/ 0.51 * 0.49

mortality_no_year_bc <- mort_inpat_func(CFR_inpat_year, inpatient_no_year_bc, CFR_nr_year, nr_care_no_year_bc)
mortality_no_year_u <- mort_inpat_func(CFR_inpat_year_u, inpatient_no_year_u, CFR_nr_year_u, nr_care_no_year_u)
mortality_SA_llAb_bc <- mort_inpat_func(CFR_inpat_year, inpatient_SA_llAb_bc, CFR_nr_year, nr_care_SA_llAb_bc)
mortality_SA_llAb_u <- mort_inpat_func(CFR_inpat_year_u, inpatient_SA_llAb_u, CFR_nr_year_u, nr_care_SA_llAb_u)

# calculate costs of medical care
medcost_no_year_bc <- medcost_func(cost_hosp, inpatient_no_year_bc, cost_outpatient, outpat_no_year_bc)
medcost_no_year_u <- medcost_func(cost_hosp_u, inpatient_no_year_u, cost_outpatient_u, outpat_no_year_u)
medcost_SA_llAb_bc <- medcost_func(cost_hosp, inpatient_SA_llAb_bc, cost_outpatient, outpat_SA_llAb_bc) 
medcost_SA_llAb_u <- medcost_func(cost_hosp_u, inpatient_SA_llAb_u, cost_outpatient_u, outpat_SA_llAb_u)

# calculate YLL
YLL_no_year_bc <- YLL_func(mortality_no_year_bc)
YLL_no_year_u <- YLL_func(mortality_no_year_u)
YLL_SA_llAb_bc <- YLL_func(mortality_SA_llAb_bc)
YLL_SA_llAb_u <- YLL_func(mortality_SA_llAb_u)

# calculate YLD
YLD_no_year_bc <- YLD_func(inpatient_no_year_bc, mortality_no_year_bc, di_yrs, dw_LRTI_severe, pneum_no_year_bc, dw_LRTI_mod)
YLD_no_year_u <- YLD_func(inpatient_no_year_u, mortality_no_year_u, di_yrs_u, dw_LRTI_severe_u, pneum_no_year_u, dw_LRTI_mod_u)
YLD_SA_llAb_bc <- YLD_func(inpatient_SA_llAb_bc, mortality_SA_llAb_bc, di_yrs, dw_LRTI_severe, pneum_SA_llAb_bc, dw_LRTI_mod)
YLD_SA_llAb_u <- YLD_func(inpatient_SA_llAb_u, mortality_SA_llAb_u, di_yrs_u, dw_LRTI_severe_u, pneum_SA_llAb_u, dw_LRTI_mod_u)

# calculate DALYs lost
DALY_no_year_bc <- YLL_no_year_bc + YLD_no_year_bc
DALY_no_year_u <- YLL_no_year_u + YLD_no_year_u
DALY_SA_llAb_bc <- YLL_SA_llAb_bc + YLD_SA_llAb_bc
DALY_SA_llAb_u <- YLL_SA_llAb_u + YLD_SA_llAb_u

# calculate DALYs averted
DALY_saved_SA_llAb_bc <- D_saved_func(DALY_no_year_bc, DALY_SA_llAb_bc)
DALY_saved_SA_llAb_u <- D_saved_func(DALY_no_year_u, DALY_SA_llAb_u)

# Find max. acceptable cost per dose
# int_cost <- c(0, 500)

ICER_func <- function(med_cost_no, med_cost, num_eligible, p_int, int_cost, DALYs_saved){
  (med_cost + (num_eligible * p_int * int_cost) - med_cost_no) / (DALYs_saved)
}

ICER_SA_llAb_bc <- ICER_func(medcost_no_year_bc, medcost_SA_llAb_bc, num_eligible_llAb_SA, p_llAb_bc, 3 + adcost_llAb, DALY_saved_SA_llAb_bc)

ct_SA_llAb_bc <- ICER_SA_llAb_bc

##########
## Calculate cost threshold uncertainty distributions for all intervention scenarios

ctu_func <- function(medcost_no, med_cost, num_eligible, p_int, DALYs_averted){
  CER <- (med_cost + (num_eligible * p_int * (3 + adcost_llAb) - medcost_no)) / DALYs_averted
}

# for base case interventions
ct_mAb_u <- ctu_func(medcost_no_u, medcost_mAb_u, num_infants_eligible_mAb, p_mAb_bc, DALY_saved_mAb_u)
ct_llAb_u <- ctu_func(medcost_no_u, medcost_llAb_u, num_infants_eligible_llAb, p_llAb_bc, DALY_saved_llAb_u)
ct_mVax_u <- ctu_func(medcost_no_u, medcost_mVax_u, num_infants_eligible_mVax, p_mVax_bc, DALY_saved_mVax_u)

#for scenario analyses
# mVax PPC
ct_PPC_mVax_u <- ctu_func(medcost_no_u, PPC_medcost_mVax_u, num_infants_eligible_mVax, p_mVax_bc, PPC_DALY_saved_mVax_u)

# ResVax complete dataset
ct_CmVax_u <- ctu_func(medcost_no_u, medcost_CmVax_u, num_infants_eligible_mVax, p_mVax_bc, DALY_saved_CmVax_u)

# mVax pre-seasonal campaign
ct_ps_mVax_u <- ctu_func(medcost_no_u, medcost_ps_mVax_u ,num_eligible_ps_mVax, p_mVax_bc, DALY_saved_ps_mVax_u)

# llAb birth-dose plus catch-up campaign
ct_SA_llAb_u <- ctu_func(medcost_no_year_u, medcost_SA_llAb_u, num_eligible_llAb_SA, p_llAb_bc, DALY_saved_SA_llAb_u)

# RSV-URTI included
ct_URTI_mAb_u <- ctu_func(medcost_URTI_no_u, medcost_URTI_mAb_u, num_infants_eligible_mAb, p_mAb_bc, DALY_saved_URTI_mAb_u)
ct_URTI_llAb_u <- ctu_func(medcost_URTI_no_u, medcost_URTI_llAb_u, num_infants_eligible_llAb, p_llAb_bc, DALY_saved_URTI_llAb_u)
ct_URTI_mVax_u <- ctu_func(medcost_URTI_no_u, medcost_URTI_mVax_u, num_infants_eligible_mVax, p_mVax_bc, DALY_saved_URTI_mVax_u)

# alternate care seeking scenario / adequate care provided
ct_cSA_mAb_u <- ctu_func(medcost_cSA_no_u, medcost_cSA_mAb_u, num_infants_eligible_mAb, p_mAb_bc, DALY_saved_cSA_mAb_u)
ct_cSA_llAb_u <- ctu_func(medcost_cSA_no_u, medcost_cSA_llAb_u, num_infants_eligible_llAb, p_llAb_bc, DALY_saved_cSA_llAb_u)
ct_cSA_mVax_u <- ctu_func(medcost_cSA_no_u, medcost_cSA_mVax_u, num_infants_eligible_mVax, p_mVax_bc, DALY_saved_cSA_mVax_u)

