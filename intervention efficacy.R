## intervention efficacy

# build intervention efficacy parameter sampling distribution for llAb
# based on llAb phase 2b clinical trial study results
# for medically attended RSV in healthy pre-term infants through 150 days (5 months)
# https://clinicaltrials.gov/ct2/show/results/NCT02878330?view=results
# RR = 1 - eff
llAb_eff_bc <- 0.701

llAb_RRm <- 1 - llAb_eff_bc
llAb_RRl <- 1 - 0.523
llAb_RRu <- 1 - 0.812

log(llAb_RRm) - log(llAb_RRl)
log(llAb_RRm) - log(llAb_RRu)
# distance roughly similar

llAb_sd <- (log(llAb_RRl) - log(llAb_RRu))/ 3.92

llAb_RRsample <- rnorm(trials, log(llAb_RRm), llAb_sd)

eff_llAb <- 1 - exp(llAb_RRsample)
# hist(eff_llAb)

# build intervention efficacy parameter sampling distribution for mVax
# based on mVax phase 3 clinical trial results in South African infants through day 90 (3 months)
# optimal timing of administration led to better antibody transfer in SA vs US and elsewhere
# RSVVW'19 mtg Novavax presentation slides

mVax_eff_bc <- 0.56

mVax_RRm <- 1 - mVax_eff_bc
mVax_RRl <- 1 - 0.33

mVax_sd <- (log(mVax_RRl) - log(mVax_RRm)) / 1.96

mVax_RRsample <- rnorm(trials, log(mVax_RRm), mVax_sd)

eff_mVax <- 1 - exp(mVax_RRsample)
# hist(eff_mVax)

# intervention efficacy parameter sampling distribution for mVax with complete global dataset
# (all clinical sites and hospital records from S.A., U.S. and elsewhere)
# from Griffin et al. 2020

CmVax_eff_bc <- 0.414
CmVax_RRm <- 1 - CmVax_eff_bc
CmVax_RRl <- 1 - 0.18
CmVax_RRu <- 1 - 0.581

CmVax_sd <- (log(CmVax_RRl) - log(CmVax_RRu)) / 3.92
CmVax_RRsample <- rnorm(trials, log(CmVax_RRm), CmVax_sd)

eff_CmVax <- 1 - exp(CmVax_RRsample)
# hist(eff_CmVax)

# build intervention efficacy parameter sampling distribution for Palivizumab (mAb)
# based on RSV IMpact trial (1998)
# reductions in RSV hospitalizations Palivizumab compared to placebo
# no BPD, premature

mAb_eff_bc <- 0.78
mAb_eff_l <- 0.66
mAb_eff_u <- 0.90

mAb_sd <- (mAb_eff_u - mAb_eff_bc)/ 1.96
eff_mAb <- rnorm(trials, mAb_eff_bc, mAb_sd)
# hist(eff_mAb)
