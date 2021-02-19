
CI_func <- function(param) {
  c(quantile(param, probs = 0.05), quantile(param, probs = 0.95))
}

## Confidence intervals for age-based attack rates
CI_AR_age <- apply(FUN = CI_func, X = AR_age_u, MARGIN =2)

## Confidence intervals for product efficacy
CI_mAb_eff <- CI_func(eff_mAb)
CI_llAb_eff <- CI_func(eff_llAb)
CI_mVax_eff <- CI_func(eff_mVax)

## Confidence intervals for probability of pneumonia (LRTI)
# CI_LRTI <- CI_func_beta(45, 108)
CI_LRTI <- CI_func(p_pneum_u)
  
## Confidence intervals for probability of inpatient care given pneumonia (LRTI)
# CI_inpat <- CI_func_beta(13, 32)
CI_inpat <- CI_func(p_inpatient_u)
  
## Confidence intervals on probability of death among LRTI cases receiving inpatient care
# CI_CFR <- CI_func_beta(7, 122)
CI_CFR <- CI_func(CFR_inpatient_u)
  
## Confidence intervals for costs of medical care
CI_costs_inpatient <- CI_func(cost_hosp_u)
CI_costs_outpatient <- CI_func(cost_outpatient_u)

## Confidence intervals for duration of illness
CI_di_yrs <- CI_func(di_yrs_u)

## Confidence intervals for disability weights
CI_dw_LRTI_severe <- CI_func(dw_LRTI_severe_u)
CI_dw_LRTI_mod <- CI_func(dw_LRTI_mod_u)

## Confidence intervals for DALYs averted in the base case
CI_DALY_saved_mAb <- CI_func(DALY_saved_mAb_u)
CI_DALY_saved_llAb <- CI_func(DALY_saved_llAb_u)
CI_DALY_saved_mVax <- CI_func(DALY_saved_mVax_u)

## Confidence intervals for cases in the base case
CI_cases_mAb <- CI_func(cases_mAb_u)
CI_cases_mVax <- CI_func(cases_mVax_u)
CI_cases_llAb <- CI_func(cases_llAb_u)

## Confidence intervals for medical costs averted
CI_MCA_mAb <- CI_func(MCA_mAb_u)
CI_MCA_llAb <- CI_func(MCA_llAb_u)
CI_MCA_mVax <- CI_func(MCA_mVax_u)

## Confidence intervals for ICERs
CI_ct_mAb <- CI_func(ct_mAb_u)            # for each base case intervention
CI_ct_llAb <- CI_func(ct_llAb_u)
CI_ct_mVax <- CI_func(ct_mVax_u)
                                          # for each scenario analysis
CI_ct_PPC_mVax <- CI_func(ct_PPC_mVax_u)  # PPC
CI_ct_CmVax <- CI_func(ct_CmVax_u )       # complete dataset
CI_ps_mVax <- CI_func(ct_ps_mVax_u )      # pre-seasonal admin
CI_ct_SA_llAb <- CI_func(ct_SA_llAb_u)    # llAb catch-up campaign included

CI_ct_URTI_mAb <- CI_func(ct_URTI_mAb_u)  # URTI included
CI_ct_URTI_llAb <- CI_func(ct_URTI_llAb_u)
CI_ct_URTI_mVax <- CI_func(ct_URTI_mVax_u)

CI_ct_cSA_mAb <- CI_func(ct_cSA_mAb_u)    # appropriate care achieved
CI_ct_cSA_llAb <- CI_func(ct_cSA_llAb_u)
CI_ct_cSA_mVax <- CI_func(ct_cSA_mVax_u)

# Confidence intervals for alternate CFRs
CI_ct_mAb_Ptrad <- CI_func(ct_mAb_Ptrad_u)
CI_ct_mAb_buchwald <- CI_func(ct_mAb_buchwald_u)



