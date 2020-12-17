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

inpat_cSA_func <- function(p_inpat, num_pneum){
  p_inpat* num_pneum
}

# calculate number of infants receiving outpatient care
outpat_func <- function(p_inpat, num_pneum){
  ((1-p_inpat) * num_pneum)
}

outpat_URTI_func <- function(p_inpat, num_pneum, febARI){
  ((1-p_inpat) * num_pneum) + (febARI * p_outpat_febARI_u)
}

# calculate number of infants not receiving appropriate level of care
nr_care_func <- function(p_inpat, num_pneum){
  p_inpat* num_pneum * (1-p_seek_care)
}

# number of deaths
mort_inpat_func <- function(CFR_inpat, num_inpat, CFR_nr, num_nr_care){
  (CFR_inpat * num_inpat) + (CFR_nr * num_nr_care)
}

mort_cSA_func <- function(CFR_inpat, num_inpat){
  (CFR_inpat * num_inpat)
}