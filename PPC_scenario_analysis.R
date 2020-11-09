# Scenario analysis for WHO preferred product characteristics (PPCs) of mVax
source("economic_outcomes.R")

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

# conduct sensitivity analysis for PPC_mVax cases by intervention coverage (from 0 to 100%)
SA_PPC_mVcov_cases_ar <- array(NA, dim = c(dim(AR_bc)[1], dim(AR_bc)[2], length(SA_cov_vec)))
for (cV in 1:length(SA_cov_vec)) {
  SA_PPC_mVcov_cases_ar[,,cV] <- pd_calc(PPC_mVax, SA_cov_vec[cV], AR_bc, PPC_mVax_dur)
}
SA_PPC_mVcov_cases <- apply(SA_PPC_mVcov_cases_ar, 3, RSVcases, babies = num_infants)

# PPC prob pneumonia
PPC_pneum_mVax <- pneum_func(p_pneum, SA_PPC_mVcov_cases)
PPC_pneum_mVax_bc <- pneum_func(p_pneum, cases_PPC_mVax_bc)
PPC_pneum_mVax_u <- pneum_func(p_pneum_u, PPC_cases_mVax_u)

# PPC inpatient care
PPC_inpatient_mVax <- inpat_func(p_inpatient, PPC_pneum_mVax) 
PPC_inpatient_mVax_bc <- inpat_func(p_inpatient, PPC_pneum_mVax_bc)
PPC_inpatient_mVax_u <- inpat_func(p_inpatient_u, PPC_pneum_mVax_u)

# PPC appropriate level of care not received
PPC_nr_care_mVax <- nr_care_func(p_inpatient, PPC_pneum_mVax)
PPC_nr_care_mVax_bc <- nr_care_func(p_inpatient, PPC_pneum_mVax_bc)
PPC_nr_care_mVax_u <- nr_care_func(p_inpatient_u, PPC_pneum_mVax_u)

# PPC outpatient care
PPC_outpat_mVax <- outpat_func(p_inpatient, PPC_pneum_mVax)
PPC_outpat_mVax_bc <- outpat_func(p_inpatient, PPC_pneum_mVax_bc)
PPC_outpat_mVax_u <- outpat_func(p_inpatient_u, PPC_pneum_mVax_u)

# PPC mortality
PPC_mortality_mVax <- mort_inpat_func(CFR_inpatient, PPC_inpatient_mVax, CFR_nr_care, PPC_nr_care_mVax)
PPC_mortality_mVax_bc <- mort_inpat_func(CFR_inpatient, PPC_inpatient_mVax_bc, CFR_nr_care, PPC_nr_care_mVax_bc)
PPC_mortality_mVax_u <- mort_inpat_func(CFR_inpatient_u, PPC_inpatient_mVax_u, CFR_nr_care_u, PPC_nr_care_mVax_u)

# PPC med costs
PPC_medcost_mVax <- medcost_func(cost_hosp, PPC_inpatient_mVax, cost_outpatient, PPC_outpat_mVax)
PPC_medcost_mVax_bc <- medcost_func(cost_hosp, PPC_inpatient_mVax_bc, cost_outpatient, PPC_outpat_mVax_bc)
PPC_medcost_mVax_u <- medcost_func(cost_hosp_u, PPC_inpatient_mVax_u, cost_outpatient_u, PPC_outpat_mVax_u)

# PPC_total costs for bc
PPC_totalcost_mVax_bc <- progcost_mVax_bc + PPC_medcost_mVax_bc

# PPC YLL
PPC_YLL_mVax <- YLL_func(PPC_mortality_mVax)
PPC_YLL_mVax_bc <- YLL_func(PPC_mortality_mVax_bc)
PPC_YLL_mVax_u <- YLL_func(PPC_mortality_mVax_u)

# PPC YLD
PPC_YLD_mVax <- YLD_func(PPC_inpatient_mVax, PPC_mortality_mVax, di_yrs, dw_LRTI_severe, PPC_pneum_mVax, dw_LRTI_mod)
PPC_YLD_mVax_bc <- YLD_func(PPC_inpatient_mVax_bc, PPC_mortality_mVax_bc, di_yrs, dw_LRTI_severe, PPC_pneum_mVax_bc, dw_LRTI_mod)
PPC_YLD_mVax_u <- YLD_func(PPC_inpatient_mVax_u, PPC_mortality_mVax_u, di_yrs_u, dw_LRTI_severe_u, PPC_pneum_mVax_u, dw_LRTI_mod_u)

# PPC DALYs
PPC_DALY_mVax <- PPC_YLL_mVax + PPC_YLD_mVax
PPC_DALY_mVax_bc <- PPC_YLL_mVax_bc + PPC_YLD_mVax_bc
PPC_DALY_mVax_u <- PPC_YLL_mVax_u + PPC_YLD_mVax_u

# PPC DALYs saved
PPC_DALY_saved_mVax <- D_saved_func(PPC_DALY_mVax[1], PPC_DALY_mVax)
PPC_DALY_saved_mVax_bc <- D_saved_func(PPC_DALY_mVax[1], PPC_DALY_mVax_bc)
PPC_DALY_saved_mVax_u <- D_saved_func(DALY_no_u, PPC_DALY_mVax_u)

# PPC ICER by cost per dose for the base case
PPC_totalcost_mVax_range <- PPC_medcost_mVax_bc + mVax_cost_range

# PPC prob CE mVax
input_prep_PPC_m <- cbind(PPC_medcost_mVax_u, PPC_DALY_saved_mVax_u)

prob_CE_PPC_m <- rep(0, length(mVax_cost_range))
for (v in 1:length(mVax_cost_range)) {
  NHBpm <- NHB3(input_prep_PPC_m, mVax_cost_range[v])
  prob_CE_PPC_m[v] <- sum(NHBpm >= 0)/trials
}

