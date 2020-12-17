################################################################################
# economic outcomes

source("health_outcomes.R") # load health outcomes
source("CostData.R") # load cost data
source("EffectData.R") # load effect data

# create matrices for Palivizumab administration costs by intervention coverage
adcost_calc <- function (ad, cost_pd, cov){
  output <- ad* (cost_pd + adcost_llAb) * cov
}
SA_mAbcov_costs_ar <- array(NA, dim = c(dim(AR_bc)[1], dim(AR_bc)[2], length(SA_cov_vec)))
for (aC in 1:length(SA_cov_vec)) {
  SA_mAbcov_costs_ar[,,aC] <- adcost_calc(mAb_admin, cost_mAb_dose+ adcost_llAb, SA_cov_vec[aC])
} 
SA_mAbcov_costs <- apply(SA_mAbcov_costs_ar* num_infants, 3, "sum")

# calculate programmatic costs of Palivizumab administration for the base case
progcost_mAb_bc <- sum(adcost_calc(mAb_admin, cost_mAb_dose, p_mAb_bc) * num_infants)

# create matrix for mVax admin to mothers
mVax_admin <- empty_cohort
for(i in 1:12) {
  mVax_admin[i, i] <-1
}

# calculate cost of medical care for each intervention by int. coverage
medcost_func <- function(c_hosp, num_inpat, c_out, num_outpat){
  (c_hosp * num_inpat) + (c_out * num_outpat)
}

medcost_no_u <- medcost_func(cost_hosp_u, inpatient_no_u, cost_outpatient_u, outpat_no_u)
medcost_cSA_no_u <- medcost_func(cost_hosp_u, inpatient_cSA_no_u, cost_outpatient_u, outpat_no_u)
medcost_URTI_no_u <- medcost_func(cost_hosp_u, inpatient_no_u, cost_outpatient_u, outpat_URTI_no_u)
medcost_mAb <- medcost_func(cost_hosp, inpatient_mAb, cost_outpatient, outpat_mAb)
medcost_mAb_bc <- medcost_func(cost_hosp, inpatient_mAb_bc, cost_outpatient, outpat_mAb_bc)
medcost_mAb_u <- medcost_func(cost_hosp_u, inpatient_mAb_u, cost_outpatient_u, outpat_mAb_u)
medcost_mVax <- medcost_func(cost_hosp, inpatient_mVax, cost_outpatient, outpat_mVax)
medcost_mVax_bc <- medcost_func(cost_hosp, inpatient_mVax_bc, cost_outpatient, outpat_mVax_bc)
medcost_mVax_u <- medcost_func(cost_hosp_u, inpatient_mVax_u, cost_outpatient_u, outpat_mVax_u)
medcost_llAb <- medcost_func(cost_hosp, inpatient_llAb, cost_outpatient, outpat_llAb)
medcost_llAb_bc <- medcost_func(cost_hosp, inpatient_llAb_bc, cost_outpatient, outpat_llAb_bc) 
medcost_llAb_u <- medcost_func(cost_hosp_u, inpatient_llAb_u, cost_outpatient_u, outpat_llAb_u)

# calculate medical costs averted
MCA_func <- function(mc_no, mc_int){
  (mc_no - mc_int)
}

MCA_mAb_bc <- MCA_func(medcost_mAb[1], medcost_mAb_bc)
MCA_mAb_u <- MCA_func(medcost_no_u, medcost_mAb_u)
MCA_llAb_bc <- MCA_func(medcost_llAb[1], medcost_llAb_bc)
MCA_llAb_u <- MCA_func(medcost_no_u, medcost_llAb_u)
MCA_mVax_bc <-MCA_func(medcost_mVax[1], medcost_mVax_bc)
MCA_mVax_u <- MCA_func(medcost_no_u, medcost_mVax_u)

# create matrices for cost of llAb for birth cohort, by int. coverage
SA_llAbcov_costs_ar <- array(NA, dim = c(dim(AR_bc)[1], dim(AR_bc)[2], length(SA_cov_vec)))
for (aC in 1:length(SA_cov_vec)) {
  SA_llAbcov_costs_ar[,,aC] <- adcost_calc(llAb_admin, cost_mAb_dose+ adcost_llAb, SA_cov_vec[aC])
} 
SA_llAbcov_costs <- apply(SA_llAbcov_costs_ar* num_infants, 3, "sum")

# calculate llAb programmatic cost for the base case
progcost_llAb_bc <- sum(adcost_calc(llAb_admin, cost_mAb_dose, p_llAb_bc)* num_infants)

# calculate mVax programmatic costs by int. coverage
SA_mVaxcov_costs_ar <- array(NA, dim = c(dim(AR_bc)[1], dim(AR_bc)[2], length(SA_cov_vec)))
for (aC in 1:length(SA_cov_vec)) {
  SA_mVaxcov_costs_ar[,,aC] <- adcost_calc(mVax_admin, cost_mVax+ adcost_llAb, SA_cov_vec[aC])
} 
SA_mVaxcov_costs <- apply(SA_mVaxcov_costs_ar* num_infants, 3, "sum")

# calculate mVax programmatic cost for the base case
progcost_mVax_bc <- sum(adcost_calc(mVax_admin, cost_mVax, p_mVax_bc)* num_infants)

# calculate total costs of intervention for birth cohort by int. coverage
totalcost_mAb <- SA_mAbcov_costs + medcost_mAb
totalcost_mAb_bc <- progcost_mAb_bc + medcost_mAb_bc
totalcost_llAb <- SA_llAbcov_costs + medcost_llAb
totalcost_llAb_bc <- progcost_llAb_bc + medcost_llAb_bc
totalcost_mVax <- SA_mVaxcov_costs + medcost_mVax
totalcost_mVax_bc <- progcost_mVax_bc + medcost_mVax_bc

# discounting function
# net present value (NPV) is a function of time in years (yrs), the discount rate (dr), and the original outcome (oo)
# discount YLL at 3% per year
NPV <- function(yrs, dr, oo){
  oo/(1+ dr)^(yrs-1)
}

yrs_vec <- c(1:58)
YLL_pc <- sum(NPV(yrs_vec, 0.03, 1))


# calculate YLL for each intervention 
# use life expectancy at birth for Mali (World Bank, 2017)
# subtract 7 months from first year of life to account for average age illness and subsequent death
YLL_func <- function(num_mort) {
  num_mort * (YLL_pc - 7/12)
}

YLL_no_u <- YLL_func(mortality_no_u)
YLL_cSA_no_u <- YLL_func(mortality_cSA_no_u)
YLL_mAb <- YLL_func(mortality_mAb)  # by int. coverage
YLL_mAb_bc <- YLL_func(mortality_mAb_bc) # base case
YLL_mAb_u <- YLL_func(mortality_mAb_u) # uncertainty
YLL_mVax <- YLL_func(mortality_mVax)
YLL_mVax_bc <- YLL_func(mortality_mVax_bc)
YLL_mVax_u <- YLL_func(mortality_mVax_u)
YLL_llAb <- YLL_func(mortality_llAb)
YLL_llAb_bc <- YLL_func(mortality_llAb_bc)
YLL_llAb_u <- YLL_func(mortality_llAb_u)

# calculate YLD for each intervention by int. coverage

YLD_func <- function(num_inpat, num_mort, di, dw_LRTI_s, num_pneum, dw_LRTI_m) {
  ((num_inpat - num_mort) * di * dw_LRTI_s) +
    ((num_pneum - num_inpat) * di * dw_LRTI_m)
}

YLD_URTI_func <- function(num_inpat, num_mort, di, dw_LRTI_s, num_pneum, dw_LRTI_m, febARI, dw_URTI_mo, dw_URTI_mi){
  ((num_inpat - num_mort) * di * dw_LRTI_s) +
    ((num_pneum - num_inpat) * di * dw_LRTI_m) +
    (febARI * p_outpat_febARI * di * dw_URTI_mo) +
    (febARI * (1-p_outpat_febARI) * di * dw_URTI_mi)
}

YLD_no_u <- YLD_func(inpatient_no_u, mortality_no_u, di_yrs_u, dw_LRTI_severe_u, pneum_no_u, dw_LRTI_mod_u)
YLD_cSA_no_u <- YLD_func(inpatient_cSA_no_u, mortality_cSA_no_u, di_yrs_u, dw_LRTI_severe_u, pneum_no_u, dw_LRTI_mod_u)
YLD_URTI_no_u <- YLD_URTI_func(inpatient_no_u, mortality_no_u, di_yrs_u, dw_LRTI_severe_u, pneum_no_u, dw_LRTI_mod_u, febARI_no_u, dw_URTI_mod_u, dw_URTI_mild_u)
YLD_mAb <- YLD_func(inpatient_mAb, mortality_mAb, di_yrs, dw_LRTI_severe, pneum_mAb, dw_LRTI_mod)
YLD_mAb_bc <- YLD_func(inpatient_mAb_bc, mortality_mAb_bc, di_yrs, dw_LRTI_severe, pneum_mAb_bc, dw_LRTI_mod)
YLD_mAb_u <- YLD_func(inpatient_mAb_u, mortality_mAb_u, di_yrs_u, dw_LRTI_severe_u, pneum_mAb_u, dw_LRTI_mod_u)
YLD_mVax <- YLD_func(inpatient_mVax, mortality_mVax, di_yrs, dw_LRTI_severe, pneum_mVax, dw_LRTI_mod)
YLD_mVax_bc <- YLD_func(inpatient_mVax_bc, mortality_mVax_bc, di_yrs, dw_LRTI_severe, pneum_mVax_bc, dw_LRTI_mod)
YLD_mVax_u <- YLD_func(inpatient_mVax_u, mortality_mVax_u, di_yrs_u, dw_LRTI_severe_u, pneum_mVax_u, dw_LRTI_mod_u)
YLD_llAb <- YLD_func(inpatient_llAb, mortality_llAb, di_yrs, dw_LRTI_severe, pneum_llAb, dw_LRTI_mod)
YLD_llAb_bc <- YLD_func(inpatient_llAb_bc, mortality_llAb_bc, di_yrs, dw_LRTI_severe, pneum_llAb_bc, dw_LRTI_mod)
YLD_llAb_u <- YLD_func(inpatient_llAb_u, mortality_llAb_u, di_yrs_u, dw_LRTI_severe_u, pneum_llAb_u, dw_LRTI_mod_u)

# calculate DALYs lost for each intervention
# prior to discounting
DALY_no_u <- YLL_no_u + YLD_no_u
DALY_cSA_no_u <- YLL_cSA_no_u + YLD_cSA_no_u
DALY_URTI_no_u <- YLL_no_u + YLD_URTI_no_u
DALY_mAb <- YLL_mAb + YLD_mAb                # by intervention coverage (vector)
DALY_mAb_bc <- YLL_mAb_bc + YLD_mAb_bc       # base case
DALY_mAb_u <- YLL_mAb_u + YLD_mAb_u          # uncertainty
DALY_mVax <- YLL_mVax + YLD_mVax
DALY_mVax_bc <- YLL_mVax_bc + YLD_mVax_bc
DALY_mVax_u <- YLL_mVax_u + YLD_mVax_u
DALY_llAb <- YLL_llAb + YLD_llAb
DALY_llAb_bc <- YLL_llAb_bc + YLD_llAb_bc
DALY_llAb_u <- YLL_llAb_u + YLD_llAb_u

# calculate DALYS saved for each intervention
D_saved_func <- function(no_D, int_D){
  (no_D - int_D)
}

DALY_saved_mAb <- D_saved_func(DALY_mAb[1], DALY_mAb)           # by intervention coverage (vector)
DALY_saved_mAb_bc <- D_saved_func(DALY_mAb[1], DALY_mAb_bc)     # point estimate/ base case
DALY_saved_mAb_u <- D_saved_func(DALY_no_u, DALY_mAb_u)         # uncertainty
DALY_saved_mVax <- D_saved_func(DALY_mVax[1], DALY_mVax)
DALY_saved_mVax_bc <- D_saved_func(DALY_mVax[1], DALY_mVax_bc)
DALY_saved_mVax_u <- D_saved_func(DALY_no_u, DALY_mVax_u)
DALY_saved_llAb <- D_saved_func(DALY_llAb[1], DALY_llAb)
DALY_saved_llAb_bc <- D_saved_func(DALY_llAb[1], DALY_llAb_bc)
DALY_saved_llAb_u <- D_saved_func(DALY_no_u, DALY_llAb_u)

# Calculate ICER by cost per dose of intervention
cost_range <- seq(0, 500, by = 0.1)

# number of individuals eligible for a dose
num_infants_eligible_mAb <- sum(mat_infants * mAb_admin)
num_infants_eligible_llAb <- sum(mat_infants * llAb_admin)
num_infants_eligible_mVax <-sum(mat_infants * mVax_admin)

# intervention costs when value of dose varies between 0 to 500 USD  
mAb_cost_range <- num_infants_eligible_mAb * p_mAb_bc * cost_range
llAb_cost_range <- num_infants_eligible_llAb * p_llAb_bc * cost_range
mVax_cost_range <- num_infants_eligible_mVax * p_mVax_bc * cost_range

# total costs (medical care + intervention) when value of dose varies between 0 to 500 USD
totalcost_mAb_range <- medcost_mAb_bc + mAb_cost_range
totalcost_llAb_range <- medcost_llAb_bc + llAb_cost_range
totalcost_mVax_range <- medcost_mVax_bc + mVax_cost_range

# costs per cases averted when value of dose varies between 0 and 500 USD
CPC_mAb <- (totalcost_mAb_range - medcost_mAb[1]) / (SA_mAbcov_cases[1] - cases_mAb_bc)
CPC_llAb <- (totalcost_llAb_range - medcost_llAb[1]) / (SA_llcov_cases[1] - cases_llAb_bc)
CPC_mVax <- (totalcost_mVax_range - medcost_mVax[1]) / (SA_mVcov_cases[1] - cases_mVax_bc)

# costs per death averted when value of dose varies between 0 and 500 USD
CPD_mAb <- (totalcost_mAb_range - medcost_mAb[1]) / (mortality_mAb[1] - mortality_mAb_bc)
CPD_llAb <- (totalcost_llAb_range - medcost_llAb[1]) / (mortality_llAb[1] - mortality_llAb_bc)
CPD_mVax <- (totalcost_mVax_range - medcost_mVax[1]) / (mortality_mVax[1] - mortality_mVax_bc)

# Cost-effectiveness ratio when value of dose varies between 0 and 500 USD
# NOTE: ICER starts out as negative because the intervention is more effective and less costly at $0 per dose
ICER_range_mAb <- (totalcost_mAb_range - medcost_mAb[1]) / DALY_saved_mAb_bc
ICER_range_llAb <- (totalcost_llAb_range - medcost_llAb[1]) / DALY_saved_llAb_bc
ICER_range_mVax <-(totalcost_mVax_range - medcost_mVax[1]) / DALY_saved_mVax_bc

# WTP
CET_Mali_GDP <- 891

# NHBs function 
NHB3 <- function (inputs, prog_cost) {
  medcost <- inputs[,1]
  DALY_saved <- inputs[,2]
  WTP <- CET_Mali_GDP
  NHB <- DALY_saved - ((medcost + prog_cost)- medcost_no_u)/WTP
  NHB
}

# prob CE for mAb
input_prep_p <- cbind(medcost_mAb_u, DALY_saved_mAb_u)

prob_CE_p <- rep(0, length(mAb_cost_range))
for (p in 1:length(mAb_cost_range)) {
  NHBp <- NHB3(input_prep_p, mAb_cost_range[p])
  prob_CE_p[p] <- sum(NHBp >= 0)/trials
}

# prob CE for mVax
input_prep_m <- cbind(medcost_mVax_u, DALY_saved_mVax_u)

prob_CE_m <- rep(0, length(mVax_cost_range))
for (m in 1:length(mVax_cost_range)) {
  NHBm <- NHB3(input_prep_m, mVax_cost_range[m])
  prob_CE_m[m] <- sum(NHBm >= 0)/trials
}

# prob CE for llAb
input_prep_x <- cbind(medcost_llAb_u, DALY_saved_llAb_u)

prob_CE_x <- rep(0, length(llAb_cost_range))
for (x in 1:length(llAb_cost_range)) {
  NHBl <- NHB3(input_prep_x, llAb_cost_range[x])
  prob_CE_x[x] <- sum(NHBl >= 0)/trials
}

# # prob CE across span of WTP
NHB_func <- function (inputs, WTP) {
  DALY_saved <- inputs[,2]
  prog_cost <- inputs[,3]
  NHB <- DALY_saved - prog_cost / WTP
  NHB
}

WTP_sp <- c(0.01, seq(10, 20*CET_Mali_GDP, by = 5))

# donor perspective
progcost_mAb_3 <- num_infants_eligible_mAb * p_mAb_bc * (3-.20)
prep_mAb_3 <- cbind(medcost_mAb_u, DALY_saved_mAb_u, progcost_mAb_3)
progcost_mAb_9 <- num_infants_eligible_mAb * p_mAb_bc * (9- 0.20)
prep_mAb_9 <- cbind(medcost_mAb_u, DALY_saved_mAb_u, progcost_mAb_9)

progcost_llAb_3 <- num_infants_eligible_llAb * p_llAb_bc * (3-.20)
prep_llAb_3 <- cbind(medcost_llAb_u, DALY_saved_llAb_u, progcost_llAb_3)
progcost_llAb_9 <- num_infants_eligible_llAb * p_llAb_bc * (9- 0.20)
prep_llAb_9 <- cbind(medcost_llAb_u, DALY_saved_llAb_u, progcost_llAb_9)

progcost_mVax_3 <- num_infants_eligible_mVax * p_mVax_bc * (3- 0.20)
prep_mVax_3 <- cbind(medcost_mVax_u, DALY_saved_mVax_u, progcost_mVax_3)
progcost_mVax_9 <- num_infants_eligible_mVax * p_mVax_bc * (9 - 0.20)
prep_mVax_9 <- cbind(medcost_mVax_u, DALY_saved_mVax_u, progcost_mVax_9)

####
govcost_mAb <- num_infants_eligible_mAb * p_mAb_bc * (0.20 + adcost_llAb)
gprep_mAb <- cbind(medcost_mAb_u, DALY_saved_mAb_u, govcost_mAb)

govcost_llAb <- num_infants_eligible_llAb * p_llAb_bc * (0.20 + adcost_llAb)
gprep_llAb <- cbind(medcost_llAb_u, DALY_saved_llAb_u, govcost_llAb)

govcost_mVax <- num_infants_eligible_mVax * p_mVax_bc * (0.20 + adcost_mVax)
gprep_mVax <- cbind(medcost_mVax_u, DALY_saved_mVax_u, govcost_mVax)


pce_donor_mAb_3 <- rep(0, length(WTP_sp))
for (dmAb in 1: length(WTP_sp)){
  NHB_dmAb <- NHB_func(prep_mAb_3, WTP_sp[dmAb])
  pce_donor_mAb_3[dmAb] <- sum(NHB_dmAb >0)/trials
}

pce_donor_mAb_9 <- rep(0, length(WTP_sp))
for (dmAb in 1: length(WTP_sp)){
  NHB_dmAb <- NHB_func(prep_mAb_9, WTP_sp[dmAb])
  pce_donor_mAb_9[dmAb] <- sum(NHB_dmAb >0)/trials
}

pce_donor_llAb_3 <- rep(0, length(WTP_sp))
for (dl in 1: length(WTP_sp)){
  NHB_dl <- NHB_func(prep_llAb_3, WTP_sp[dl])
  pce_donor_llAb_3[dl] <- sum(NHB_dl >0)/trials
}

pce_donor_llAb_9 <- rep(0, length(WTP_sp))
for (dl in 1: length(WTP_sp)){
  NHB_dl <- NHB_func(prep_llAb_9, WTP_sp[dl])
  pce_donor_llAb_9[dl] <- sum(NHB_dl >0)/trials
}

pce_donor_mVax_3 <- rep(0, length(WTP_sp))
for (dm in 1: length(WTP_sp)){
  NHB_dm <- NHB_func(prep_mVax_3, WTP_sp[dm])
  pce_donor_mVax_3[dm] <- sum(NHB_dm >0)/trials
}

pce_donor_mVax_9 <- rep(0, length(WTP_sp))
for (dm in 1: length(WTP_sp)){
  NHB_dm <- NHB_func(prep_mVax_9, WTP_sp[dm])
  pce_donor_mVax_9[dm] <- sum(NHB_dm >0)/trials
}


###
pce_gov_mAb <- rep(0, length(WTP_sp))
for (gmAb in 1: length(WTP_sp)){
  NHB_gmAb <- NHB_func(gprep_mAb, WTP_sp[gmAb])
  pce_gov_mAb[gmAb] <- sum(NHB_gmAb >0)/trials
}

pce_gov_llAb <- rep(0, length(WTP_sp))
for (gl in 1: length(WTP_sp)){
  NHB_gl <- NHB_func(gprep_llAb, WTP_sp[gl])
  pce_gov_llAb[gl] <- sum(NHB_gl >0)/trials
}

pce_gov_mVax <- rep(0, length(WTP_sp))
for (gm in 1: length(WTP_sp)){
  NHB_gm <- NHB_func(gprep_mVax, WTP_sp[gm])
  pce_gov_mVax[gm] <- sum(NHB_gm >0)/trials
}

# ## mVax vs llAb, head to head cost per dose NHB comparison
# 
# mVax_h2h <- seq(1, 10, length.out = 50)
# llAb_h2h <- seq(1, 10, length.out = 40)
# 
# mVax_h2h_mat <- matrix (mVax_h2h, nrow = length(mVax_h2h), ncol = length(llAb_h2h), byrow = FALSE)
# llAb_h2h_mat <- matrix (llAb_h2h, nrow = length(mVax_h2h), ncol = length(llAb_h2h), byrow = TRUE)
# 
# mVax_data_frame <- cbind(medcost_mVax_u, DALY_saved_mVax_u, medcost_no_u)
# llAb_data_frame <- cbind(medcost_llAb_u, DALY_saved_llAb_u, medcost_no_u)
# 
# NHB_h2h <- function (USD_price, inputs, p_int, num_eligible){
#   WTP <- CET_Mali_GDP
#   medcost_u <- inputs[,1]
#   DALY_saved_u <- inputs[,2]
#   medcost_no_u <- inputs[,3]
#   NHB_1 <- DALY_saved_u - ((medcost_u + (USD_price * num_eligible * p_int)) - medcost_no_u)/WTP
# }
# 
# NHB_mVax <- mapply(NHB_h2h, mVax_h2h_mat, 
#                    MoreArgs = list(mVax_data_frame, p_mVax_bc, sum(num_infants)))
# NHB_llAb <- mapply(NHB_h2h, llAb_h2h_mat, 
#                    MoreArgs = list(llAb_data_frame, p_llAb_bc, num_infants_eligible_llAb))
# 
# mVax_winner <- NHB_mVax > NHB_llAb
# prob_mVax_wins <- colSums(mVax_winner)/trials
# prob_matrix <- matrix(prob_mVax_wins, nrow = length(mVax_h2h), ncol = length(llAb_h2h))

