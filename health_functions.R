# Functions for calculating particular things we love

# calculate number of infants who develop RSV-LRTI (pneumonia)
pneum_func <- function(prob_pneum, num_cases) {
  prob_pneum * num_cases
}

# calculate number of infants who develop URTI (febrile ARI)
fARI_func <- function(prob_pneum, num_cases) {
  (1 - prob_pneum) * num_cases
}

# calculate number of infants receiving inpatient care
inpat_func <- function(p_inpat, num_pneum){
  p_inpat* num_pneum * p_seek_care
}

# alternate care scenario
inpat_cSA_func <- function(p_inpat, num_pneum){
  p_inpat* num_pneum
}

# calculate number of infants not receiving appropriate level of care
# 53% of infants in LMIC with RSV-LRTI do not receive appropriate level of care
nr_care_func <- function(p_inpat, num_pneum){
  p_inpat* num_pneum * (1-p_seek_care)
}

# number of deaths
mort_inpat_func <- function(CFR_inpat, num_inpat, CFR_nr, num_nr_care){
  (CFR_inpat * num_inpat) + (CFR_nr * num_nr_care)
}

# alternate care scenario
mort_cSA_func <- function(CFR_inpat, num_inpat){
  (CFR_inpat * num_inpat)
}