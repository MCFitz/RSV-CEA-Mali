################################################################################
# Bayesian melding
pretrial <- 1000000
trials <- 10000

source("health_functions.R")

ILI_positivity <- rbeta(trials, 110, 379-110)
p_pneum_u <- rbeta(trials, 43, 110 + ILI_positivity*839)
p_pneum <- 43/(110+839*(110/379))
p_inpatient_u <- rbeta(trials, 13, 30)
p_inpatient <- 13/43
# 
# p_pneum_u_pr <- rbeta(pretrial, 45, 108)/((803+839)/803)
# p_inpatient_u_pr <- rbeta(pretrial, 13, 30)
# # CFR_inpatient_u_pr <- rbeta(pretrial, 7, 122)
# 
# 
# # CFR_B <- rbeta(pretrial, 1, 12)
# # CFR_RSV_u_pr <- p_pneum_u_pr*p_inpatient_u_pr*CFR_inpatient_u_pr
# inp_RSV_u_pr <- p_pneum_u_pr*p_inpatient_u_pr
# 
# # pneum_no_u_pr <- pneum_func(p_pneum_u_pr, cases_no_u)
# # inpatient_cSA_no_u_pr <- inpat_cSA_func(p_inpatient_u_pr, pneum_no_u_pr)
# # mortality_cSA_no_u_pr <- mort_cSA_func(CFR_inpatient_u_pr, inpatient_cSA_no_u_pr)
# 
# save(p_pneum_u_pr, p_inpatient_u_pr, file = "PriorDistr.Rdata")
# 
# # mod_death_prob
# # RSVdeath
# # llCFR <- dbinom(RSVdeath, RSVbabies, mod_death_prob, log = TRUE)
# 
# RSVllik <- function (mod_hosp_prob, RSVhosp, RSVbabies) {
#   llinp <- dbinom(RSVhosp, RSVbabies, mod_hosp_prob, log = TRUE)
#   llinp
# }
# 
# data_RSVbabies <- round(153 + (839 * 108/(494 -45))) ## corrected for oversampling of pneumonia babies
# # data_RSVdeath <- 1
# data_RSV_hosp <- 13
# 
# # run liklihood
# # CFR_RSV_u_pr
# # data_RSVdeath
# llik <- RSVllik (inp_RSV_u_pr, data_RSV_hosp, data_RSVbabies)
# 
# # Bayesian melding
# likli_weights <- exp(llik)/sum(exp(llik)) # create vector of weights
# mset <- sample(1:pretrial, trials, replace = TRUE, prob = likli_weights) #chosen set
# llpost <- llik[mset]
# lpoint <- which.max(llpost) # new point estimate
# 
# # specify and save new parameter distributions
# p_pneum_u <- p_pneum_u_pr[mset]
# p_pneum <- p_pneum_u[lpoint] # probability of pneumonia given RSV
# 
# p_inpatient_u <- p_inpatient_u_pr[mset]
# p_inpatient <- p_inpatient_u[lpoint] # probability of inpatient care given pneumonia
# 
# ##### estimating alpha and beta parameters from new disributions
# estBetaParams <- function(mu, var) {
#   alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
#   beta <- alpha * (1 / mu - 1)
#   return(params = list(alpha = alpha, beta = beta))
# }
# 
# eBP_pneum <- estBetaParams(mean(p_pneum_u), var(p_pneum_u))
# eBP_inpat <- estBetaParams(mean(p_inpatient_u), var(p_inpatient_u))
# 
# 
# # CFR_inpatient_u <- CFR_inpatient_u_pr[mset]
# # CFR_inpatient <- CFR_inpatient_u[lpoint] # case-fatality ratio of infants receiving inpatient care given pneumonia
# 
# ### visualizing results of fit
# # plot(CFR_inpatient_u_pr, llik)
# # points(CFR_inpatient_u_pr[mset], llik[mset], col = "goldenrod")
# # points(CFR_inpatient_u[lpoint], llpost[lpoint], col = "red")
# 
# # plot(p_inpatient_u_pr, llik)
# # points(p_inpatient_u_pr[mset], llik[mset], col = "goldenrod")
# # points(p_inpatient_u[lpoint], llpost[lpoint], col = "red")
# # 
# plot(p_pneum_u_pr, llik)
# points(p_pneum_u_pr[mset], llik[mset], col = "goldenrod")
# points(p_pneum_u[lpoint], llpost[lpoint], col = "red")

print(p_pneum) 
print(p_inpatient) 
# print(CFR_inpatient)
# 
# save to file
save(p_pneum_u, p_inpatient_u, p_pneum, p_inpatient, trials,
#     llpost, lpoint,
     file = "PostDistr.Rdata")
