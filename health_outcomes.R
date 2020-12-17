#####################################################################################################
# Epi and Health Outcomes

source("birth cohort.R") # load cohort structure and cases calculations
source("health_functions.R")

# calculate number of infants who develop RSV-LRTI (pneumonia)

ILI_positivity <- rbeta(trials, 110, 379-110)
p_pneum_u <- rbeta(trials, 43, 110 + ILI_positivity*839)
p_pneum <- 43/(110+839*(110/379))

pneum_no_u <- pneum_func(p_pneum_u, cases_no_u)   # number of infants with pneumonia under no intervention, uncertainty
pneum_mAb <- pneum_func(p_pneum, SA_mAbcov_cases) # SA for int. coverage
pneum_mAb_bc <- pneum_func(p_pneum, cases_mAb_bc) # point estimate / base case
pneum_mAb_u <- pneum_func(p_pneum_u, cases_mAb_u) # uncertainty
pneum_mVax <- pneum_func(p_pneum, SA_mVcov_cases)
pneum_mVax_bc <- pneum_func(p_pneum, cases_mVax_bc)
pneum_mVax_u <- pneum_func(p_pneum_u, cases_mVax_u)
pneum_llAb <- pneum_func(p_pneum, SA_llcov_cases)
pneum_llAb_bc <- pneum_func(p_pneum, cases_llAb_bc)
pneum_llAb_u <- pneum_func(p_pneum_u, cases_llAb_u)

# calculate number of infants who develop URTI (febrile ARI)

febARI_no_u <- fARI_func(p_pneum_u, cases_no_u)
febARI_mAb <- fARI_func(p_pneum, SA_mAbcov_cases)
febARI_mAb_bc <- fARI_func(p_pneum, cases_mAb_bc)
febARI_mAb_u <- fARI_func(p_pneum_u, cases_mAb_u)
febARI_mVax <- fARI_func(p_pneum, SA_mVcov_cases)
febARI_mVax_bc <- fARI_func(p_pneum, cases_mVax_bc)
febARI_mVax_u <- fARI_func(p_pneum_u, cases_mVax_u)
febARI_llAb <- fARI_func(p_pneum, SA_llcov_cases)
febARI_llAb_bc <- fARI_func(p_pneum, cases_llAb_bc)
febARI_llAb_u <- fARI_func(p_pneum_u, cases_llAb_u)

# calculate number of infants receiving inpatient care

p_seek_care <- 0.47
p_inpatient_u <- rbeta(trials, 13, 30)
p_inpatient <- 13/43

inpatient_no_u <- inpat_func(p_inpatient_u, pneum_no_u)
inpatient_cSA_no_u <- inpat_cSA_func(p_inpatient_u, pneum_no_u)   # analysis for adequate care scenario
inpatient_mAb <- inpat_func(p_inpatient, pneum_mAb)
inpatient_mAb_bc <- inpat_func(p_inpatient, pneum_mAb_bc)
inpatient_mAb_u <- inpat_func(p_inpatient_u, pneum_mAb_u)
inpatient_mVax <- inpat_func(p_inpatient, pneum_mVax)
inpatient_mVax_bc <- inpat_func(p_inpatient, pneum_mVax_bc)
inpatient_mVax_u <- inpat_func(p_inpatient_u, pneum_mVax_u)
inpatient_llAb <- inpat_func(p_inpatient, pneum_llAb)
inpatient_llAb_bc <- inpat_func(p_inpatient, pneum_llAb_bc)
inpatient_llAb_u <- inpat_func(p_inpatient_u, pneum_llAb_u)

<<<<<<< HEAD
# # calculate number of infants not receiving appropriate level of care
# # 53% of infants in LMIC with RSV-LRTI do not receive appropriate level of care
=======
# calculate number of infants not receiving appropriate level of care
# 53% of infants in LMIC with RSV-LRTI do not receive appropriate level of care
>>>>>>> NewPriors

nr_care_no_u <- nr_care_func(p_inpatient_u, pneum_no_u)
nr_care_mAb <- nr_care_func(p_inpatient, pneum_mAb)
nr_care_mAb_bc <- nr_care_func(p_inpatient, pneum_mAb_bc)
nr_care_mAb_u <- nr_care_func(p_inpatient_u, pneum_mAb_u)
nr_care_mVax <- nr_care_func(p_inpatient, pneum_mVax)
nr_care_mVax_bc <- nr_care_func(p_inpatient, pneum_mVax_bc)
nr_care_mVax_u <- nr_care_func(p_inpatient_u, pneum_mVax_u)
nr_care_llAb <- nr_care_func(p_inpatient, pneum_llAb)
nr_care_llAb_bc <- nr_care_func(p_inpatient, pneum_llAb_bc)
nr_care_llAb_u <- nr_care_func(p_inpatient_u, pneum_llAb_u)

# calculate number of infants receiving outpatient care by int. coverage

p_outpat_febARI <- 0.936 # probability of outpatient care given URTI, from Orenstein cost data (no care was assumed for episodes with $0 medical costs)
p_outpat_febARI_u <- rbeta(trials, 132, 9)
  
outpat_no_u <- outpat_func(p_inpatient_u, pneum_no_u)
outpat_URTI_no_u <- outpat_URTI_func(p_inpatient_u, pneum_no_u, febARI_no_u)    # scenario for RSV-URTI inclusion
outpat_mAb <- outpat_func(p_inpatient, pneum_mAb)
outpat_mAb_bc <- outpat_func(p_inpatient, pneum_mAb_bc)
outpat_mAb_u <- outpat_func(p_inpatient_u, pneum_mAb_u)
outpat_mVax <- outpat_func(p_inpatient, pneum_mVax)
outpat_mVax_bc <- outpat_func(p_inpatient, pneum_mVax_bc)
outpat_mVax_u <- outpat_func(p_inpatient_u, pneum_mVax_u)
outpat_llAb <- outpat_func(p_inpatient, pneum_llAb)
outpat_llAb_bc <- outpat_func(p_inpatient, pneum_llAb_bc)
outpat_llAb_u <- outpat_func(p_inpatient_u, pneum_llAb_u)

# CFRs
<<<<<<< HEAD
# 7 out of 117 died from PERCH Mali from age 28 days to 6 months
# 1 out of 13 for Buchwald et al. study

# CFR_inpatient <- 1/13     # MALI INCIDENCE STUDY, BUCHWALD
# CFR_inpatient_u <- rbeta(trials, 1, 12)

CFR_inpatient <- 0.016  # PERCH PIA
CFR_inpatient_u <- rbeta(trials, 0.05*48, 0.552*259 - 0.05*48)

# CFR_inpatient <- 0.0598 # PERCH TRAD CALC
# CFR_inpatient_u <- rbeta(trials, 7, 110)

# library(readr)
# RSV_CFRByIteration_03MAL_LT6M <- read_csv("RSV_CFRByIteration_03MAL_LT6M.csv")
# CFR_inpatient_u <- sample(RSV_CFRByIteration_03MAL_LT6M$cfr.19..., trials, replace = TRUE)
=======
# from PERCH Mali, PIA calculation

CFR_inpatient <- 0.016
CFR_inpatient_u <- rbeta(trials, 0.05*48, 0.552*259 - 0.05*48)

>>>>>>> NewPriors
CFR_nr_care <- CFR_inpatient/0.51 * 0.49  # 49% of infants in LMIC with RSV-LRTI die outside of inpatient care setting
CFR_nr_care_u <- CFR_inpatient_u/ 0.51 * 0.49

mortality_no_u <- mort_inpat_func(CFR_inpatient_u, inpatient_no_u, CFR_nr_care_u, nr_care_no_u) # no intervention
mortality_cSA_no_u <- mort_cSA_func(CFR_inpatient_u, inpatient_cSA_no_u)  # no intervention, scenario for adequate care provided
mortality_mAb <- mort_inpat_func(CFR_inpatient, inpatient_mAb, CFR_nr_care, nr_care_mAb)
mortality_mAb_bc <- mort_inpat_func(CFR_inpatient, inpatient_mAb_bc, CFR_nr_care, nr_care_mAb_bc)
mortality_mAb_u <- mort_inpat_func(CFR_inpatient_u, inpatient_mAb_u, CFR_nr_care_u, nr_care_mAb_u)
mortality_mVax <- mort_inpat_func(CFR_inpatient, inpatient_mVax, CFR_nr_care, nr_care_mVax)
mortality_mVax_bc <- mort_inpat_func(CFR_inpatient, inpatient_mVax_bc, CFR_nr_care, nr_care_mVax_bc)
mortality_mVax_u <- mort_inpat_func(CFR_inpatient_u, inpatient_mVax_u, CFR_nr_care_u, nr_care_mVax_u)
mortality_llAb <- mort_inpat_func(CFR_inpatient, inpatient_llAb, CFR_nr_care, nr_care_llAb)
mortality_llAb_bc <- mort_inpat_func(CFR_inpatient, inpatient_llAb_bc, CFR_nr_care, nr_care_llAb_bc)
mortality_llAb_u <- mort_inpat_func(CFR_inpatient_u, inpatient_llAb_u, CFR_nr_care_u, nr_care_llAb_u)

##