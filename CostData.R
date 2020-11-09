# RSV Cost-Effectiveness in Mali
# Cost Data

# Programmatic Costs in USD per infant dose
cost_mAb_dose <- 3       # cost Palivizumab, assumption
cost_mVax <- 3       # cost maternal vaccine, assumption

# Medical costs in 2019 USD (based on data collected from Orenstein et al.)
cost_hosp <- 118.57
cost_outpatient <- 6.56

# sampling distribution for medical costs
# using SE of costs
cost_hosp_u <- rnorm(trials, cost_hosp, 15.90)
cost_outpatient_u <- rnorm(trials, cost_outpatient, 0.67)

# Administrative costs brought to 2019 USD
# based on CPI average annual percent change from IMF
inflate_func <- function(ov, rate, t){
  pv <- ov* (1 + rate)^t
}

# administrative/ delivery costs for llAb and mAb
adcost_llAb_2015 <- 1.25
adcost_llAb <- inflate_func(adcost_llAb_2015, 0.019, 4)

# adcost_mVax_2018 <- 0.83
# inflate_func(adcost_mVax_2018, 0.018, 1)
# use same administrative/ delivery costs for mVax as llAb and mAb
adcost_mVax <- adcost_llAb