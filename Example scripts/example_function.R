# example functions for cost-effectiveness

# reframe cost-effectiveness as net health benefits (NHB) analysis
# such that the outcome is whether the intervention provides positive NHB


NHB_mvax <- function(int_cost) {
  med_costs <- 1000
  prog_cost <- int_cost*0.5
  DALY_saved_mvax <- 50
  WTP <- 800
  NHB <- DALY_saved_mvax - (med_costs * prog_cost)/WTP
  NHB
}

NHB_mvax(0)  # call the function for a single input

NHB_mvax(c(0, 20, 30))  # call the function for a few inputs

mvax_cost <- seq(0, 500, by = 5)
NHB_mvax_store <- NHB_mvax(mvax_cost)  # call the function for a LOT of inputs

plot(mvax_cost, NHB_mvax_store, type = "l")


#### MORE COMPLICATED EXAMPLE
### USING THE APPLY FUNCTION
### GODSPEED, RACHEL

draws <- 1000
med_costs_mvax <- rnorm(draws, 1000, 50)
DALYs_saved_mvax <- rnorm(draws, 100, 25)

input_prep <- cbind(med_costs_mvax, DALYs_saved_mvax)

# new NHB function
NHB2 <- function (inputs, int_cost) {
  med_costs <- inputs[1]
  DALY_saved_mvax <- inputs[2]
  prog_cost <- int_cost*0.5
  WTP <- 800
  NHB <- DALY_saved_mvax - (med_costs * prog_cost)/WTP
  NHB
}

NHB2(inputs = input_prep[1,], int_cost = 0)  # for the first set of inputs

prob_CE <- rep(0, length(mvax_cost))
for (m in 1:length(mvax_cost)) {
  NHBs <- apply(input_prep, 1, NHB2, int_cost = mvax_cost[m])
  prob_CE[m] <- sum((NHBs >= 0))/draws
}

plot(mvax_cost, prob_CE, type = "l")

