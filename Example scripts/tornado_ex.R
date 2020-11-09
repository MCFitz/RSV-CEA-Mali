# Sensitivity to Economic Parameters
# Each parameter set to its lower and upper CI, all others held at base case

ref <- as.data.frame(cent_d[,1])
int <- as.data.frame(cent_d[,2])

############ QALY score ########################
low_Q <- apply(random_Q, c(1,2), quantile, probs = 0.025, na.rm = TRUE)
high_Q <- apply(random_Q, c(1,2), quantile, probs = 0.975, na.rm = TRUE)

# Infants, low ICER (low QALY score)
new_Q <- Q_array
new_Q[1:3,,] <- low_Q[1:3,]
infant_Q_low <- Just_ICER(1, ref, int, 0, 0, 0, 0.75, "ante", admin_cost_ante, 
                          admin_cost_post, admin_cost_dad, vaccine_price, booster_price, 
                          adv, tf, discount, lit_array, Med_array, Ind, new_Q, 
                          hospital, improvement, recovery, le)

# Infants, high ICER (high QALY score)
new_Q <- Q_array
new_Q[1:3,,] <- high_Q[1:3,]
infant_Q_high <- Just_ICER(1, ref, int, 0, 0, 0, 0.75, "ante", admin_cost_ante, 
                           admin_cost_post, admin_cost_dad, vaccine_price, booster_price, 
                           adv, tf, discount, lit_array, Med_array, Ind, new_Q, 
                           hospital, improvement, recovery, le)

# Adults, low ICER (low QALY score)
new_Q <- Q_array
new_Q[8,,] <- low_Q[8,]
adult_Q_low <- Just_ICER(1, ref, int, 0, 0, 0, 0.75, "ante", admin_cost_ante, 
                          admin_cost_post, admin_cost_dad, vaccine_price, booster_price, 
                          adv, tf, discount, lit_array, Med_array, Ind, new_Q, 
                          hospital, improvement, recovery, le)

# Adults, high ICER (high QALY score)
new_Q <- Q_array
new_Q[8,,] <- high_Q[8,]
adult_Q_high <- Just_ICER(1, ref, int, 0, 0, 0, 0.75, "ante", admin_cost_ante, 
                           admin_cost_post, admin_cost_dad, vaccine_price, booster_price, 
                           adv, tf, discount, lit_array, Med_array, Ind, new_Q, 
                           hospital, improvement, recovery, le)

############ Probability of Severe Outcome #####################
set <- c(1:3, 8)
low_sev <- apply(random_sev, c(2,3), quantile, probs = 0.025, na.rm = TRUE)
low_sev[set,2] <- 1-(low_sev[set,3] + low_sev[set,4] + low_sev[set,5] + low_sev[set,6])
high_sev <- apply(random_sev, c(2,3), quantile, probs = 0.975, na.rm = TRUE)
high_sev[set,2] <- 1-(high_sev[set,3] + high_sev[set,4] + high_sev[set,5] + high_sev[set,6])

# Infants, low ICER (high severity)
new_sev <- lit_array
new_sev[,1:3,] <- high_sev[1:3,]
infant_sev_low <- Just_ICER(1, ref, int, 0, 0, 0, 0.75, "ante", admin_cost_ante, 
                            admin_cost_post, admin_cost_dad, vaccine_price, booster_price, 
                            adv, tf, discount, new_sev, Med_array, Ind, Q_array, 
                            hospital, improvement, recovery, le)

# Infants, high ICER (low severity)
new_sev <- lit_array
new_sev[,1:3,] <- low_sev[1:3,]
infant_sev_high <- Just_ICER(1, ref, int, 0, 0, 0, 0.75, "ante", admin_cost_ante, 
                            admin_cost_post, admin_cost_dad, vaccine_price, booster_price, 
                            adv, tf, discount, new_sev, Med_array, Ind, Q_array, 
                            hospital, improvement, recovery, le)

# Adults, low ICER (high severity)
new_sev <- lit_array
new_sev[,8,] <- high_sev[8,]
adult_sev_low <- Just_ICER(1, ref, int, 0, 0, 0, 0.75, "ante", admin_cost_ante, 
                            admin_cost_post, admin_cost_dad, vaccine_price, booster_price, 
                            adv, tf, discount, new_sev, Med_array, Ind, Q_array, 
                            hospital, improvement, recovery, le)

# Adults, high ICER (low severity)
new_sev <- lit_array
new_sev[,8,] <- low_sev[8,]
adult_sev_high <- Just_ICER(1, ref, int, 0, 0, 0, 0.75, "ante", admin_cost_ante, 
                             admin_cost_post, admin_cost_dad, vaccine_price, booster_price, 
                             adv, tf, discount, new_sev, Med_array, Ind, Q_array, 
                             hospital, improvement, recovery, le)




#################### Time to Improvement #################
low_imp <- apply(random_imp, 2, quantile, probs = 0.025, na.rm = TRUE)
high_imp <- apply(random_imp, 2, quantile, probs = 0.975, na.rm = TRUE)

# Infants, low ICER (long tti)
new_imp <- improvement
new_imp[1:3,] <- high_imp[1:3]
infant_imp_low <- Just_ICER(1, ref, int, 0, 0, 0, 0.75, "ante", admin_cost_ante, 
                            admin_cost_post, admin_cost_dad, vaccine_price, booster_price, 
                            adv, tf, discount, lit_array, Med_array, Ind, Q_array, 
                            hospital, new_imp, recovery, le)

# Infants, high ICER (short tti)
new_imp <- improvement
new_imp[1:3,] <- low_imp[1:3]
infant_imp_high <- Just_ICER(1, ref, int, 0, 0, 0, 0.75, "ante", admin_cost_ante, 
                            admin_cost_post, admin_cost_dad, vaccine_price, booster_price, 
                            adv, tf, discount, lit_array, Med_array, Ind, Q_array, 
                            hospital, new_imp, recovery, le)

# Adults, low ICER (long tti)
new_imp <- improvement
new_imp[8,] <- high_imp[8]
adult_imp_low <- Just_ICER(1, ref, int, 0, 0, 0, 0.75, "ante", admin_cost_ante, 
                            admin_cost_post, admin_cost_dad, vaccine_price, booster_price, 
                            adv, tf, discount, lit_array, Med_array, Ind, Q_array, 
                            hospital, new_imp, recovery, le)

# Adults, high ICER (long tti)
new_imp <- improvement
new_imp[8,] <- low_imp[8]
adult_imp_high <- Just_ICER(1, ref, int, 0, 0, 0, 0.75, "ante", admin_cost_ante, 
                           admin_cost_post, admin_cost_dad, vaccine_price, booster_price, 
                           adv, tf, discount, lit_array, Med_array, Ind, Q_array, 
                           hospital, new_imp, recovery, le)


#################### Time to Recovery #################
low_rec <- apply(random_rec, 2, quantile, probs = 0.025, na.rm = TRUE)
high_rec <- apply(random_rec, 2, quantile, probs = 0.975, na.rm = TRUE)

# Infants, low ICER (long ttr)
new_rec <- recovery
new_rec[1:3,] <- high_rec[1:3]
infant_rec_low <- Just_ICER(1, ref, int, 0, 0, 0, 0.75, "ante", admin_cost_ante, 
                            admin_cost_post, admin_cost_dad, vaccine_price, booster_price, 
                            adv, tf, discount, lit_array, Med_array, Ind, Q_array, 
                            hospital, improvement, new_rec, le)

# Infants, high ICER (short ttr)
new_rec <- recovery
new_rec[1:3,] <- low_rec[1:3]
infant_rec_high <- Just_ICER(1, ref, int, 0, 0, 0, 0.75, "ante", admin_cost_ante, 
                             admin_cost_post, admin_cost_dad, vaccine_price, booster_price, 
                             adv, tf, discount, lit_array, Med_array, Ind, Q_array, 
                             hospital, improvement, new_rec, le)

# Adults, low ICER (long ttr)
new_rec <- recovery
new_rec[8,] <- high_rec[8]
adult_rec_low <- Just_ICER(1, ref, int, 0, 0, 0, 0.75, "ante", admin_cost_ante, 
                           admin_cost_post, admin_cost_dad, vaccine_price, booster_price, 
                           adv, tf, discount, lit_array, Med_array, Ind, Q_array, 
                           hospital, improvement, new_rec, le)

# Adults, high ICER (long ttr)
new_rec <- recovery
new_rec[8,] <- low_rec[8]
adult_rec_high <- Just_ICER(1, ref, int, 0, 0, 0, 0.75, "ante", admin_cost_ante, 
                            admin_cost_post, admin_cost_dad, vaccine_price, booster_price, 
                            adv, tf, discount, lit_array, Med_array, Ind, Q_array, 
                            hospital, improvement, new_rec, le)



#################### Time in Hospital #################
low_hos <- apply(random_hos, 2, quantile, probs = 0.025, na.rm = TRUE)
high_hos <- apply(random_hos, 2, quantile, probs = 0.975, na.rm = TRUE)

# Infants, low ICER (long tih)
new_hos <- hospital
new_hos[1:3,] <- high_hos[1:3]
infant_hos_low <- Just_ICER(1, ref, int, 0, 0, 0, 0.75, "ante", admin_cost_ante, 
                            admin_cost_post, admin_cost_dad, vaccine_price, booster_price, 
                            adv, tf, discount, lit_array, Med_array, Ind, Q_array, 
                            new_hos, improvement, recovery, le)

# Infants, high ICER (short tih)
new_hos <- hospital
new_hos[1:3,] <- low_hos[1:3]
infant_hos_high <- Just_ICER(1, ref, int, 0, 0, 0, 0.75, "ante", admin_cost_ante, 
                             admin_cost_post, admin_cost_dad, vaccine_price, booster_price, 
                             adv, tf, discount, lit_array, Med_array, Ind, Q_array, 
                             new_hos, improvement, recovery, le)

# Adults, low ICER (long tih)
new_hos <- hospital
new_hos[8,] <- high_hos[8]
adult_hos_low <- Just_ICER(1, ref, int, 0, 0, 0, 0.75, "ante", admin_cost_ante, 
                           admin_cost_post, admin_cost_dad, vaccine_price, booster_price, 
                           adv, tf, discount, lit_array, Med_array, Ind, Q_array, 
                           new_hos, improvement, recovery, le)

# Adults, high ICER (long tih)
new_hos <- hospital
new_hos[8,] <- low_hos[8]
adult_hos_high <- Just_ICER(1, ref, int, 0, 0, 0, 0.75, "ante", admin_cost_ante, 
                            admin_cost_post, admin_cost_dad, vaccine_price, booster_price, 
                            adv, tf, discount, lit_array, Med_array, Ind, Q_array, 
                            new_hos, improvement, recovery, le)

############ Medical Costs #####################
set <- c(1:3, 8)
low_Med <- apply(random_Med, c(2,3), quantile, probs = 0.025, na.rm = TRUE)
high_Med <- apply(random_Med, c(2,3), quantile, probs = 0.975, na.rm = TRUE)

# Infants, low ICER (high medical costs)
new_Med <- Med_array
new_Med[,1:3,] <- high_Med[1:3,]
infant_Med_low <- Just_ICER(1, ref, int, 0, 0, 0, 0.75, "ante", admin_cost_ante, 
                            admin_cost_post, admin_cost_dad, vaccine_price, booster_price, 
                            adv, tf, discount, lit_array, new_Med, Ind, Q_array, 
                            hospital, improvement, recovery, le)

# Infants, high ICER (low medical costs)
new_Med <- Med_array
new_Med[,1:3,] <- low_Med[1:3,]
infant_Med_high <- Just_ICER(1, ref, int, 0, 0, 0, 0.75, "ante", admin_cost_ante, 
                             admin_cost_post, admin_cost_dad, vaccine_price, booster_price, 
                             adv, tf, discount, lit_array, new_Med, Ind, Q_array, 
                             hospital, improvement, recovery, le)

# Adults, low ICER (high medical costs)
new_Med <- Med_array
new_Med[,8,] <- high_Med[8,]
adult_Med_low <- Just_ICER(1, ref, int, 0, 0, 0, 0.75, "ante", admin_cost_ante, 
                           admin_cost_post, admin_cost_dad, vaccine_price, booster_price, 
                           adv, tf, discount, lit_array, new_Med, Ind, Q_array, 
                           hospital, improvement, recovery, le)

# Adults, high ICER (low medical costs)
new_Med <- Med_array
new_Med[,8,] <- low_Med[8,]
adult_Med_high <- Just_ICER(1, ref, int, 0, 0, 0, 0.75, "ante", admin_cost_ante, 
                            admin_cost_post, admin_cost_dad, vaccine_price, booster_price, 
                            adv, tf, discount, lit_array, new_Med, Ind, Q_array, 
                            hospital, improvement, recovery, le)






save(infant_Q_low, infant_Q_high, adult_Q_low, adult_Q_high, infant_sev_low,
     infant_sev_high, adult_sev_low, adult_sev_high, infant_imp_low, infant_imp_high,
     adult_imp_low, adult_imp_high, infant_rec_low, infant_rec_high, adult_rec_low,
     adult_rec_high, infant_hos_low, infant_hos_high, adult_hos_low, adult_hos_high,
     infant_Med_low, infant_Med_high, adult_Med_low, adult_Med_high,
     file = "tornado.RData")

#plot
#windows()
t_list <- c(adult_hos_low, adult_hos_high, adult_rec_low, adult_rec_high, 
            adult_sev_low, adult_sev_high, adult_imp_low, adult_imp_high, 
            infant_hos_low, infant_hos_high, adult_Med_low, adult_Med_high, 
            infant_rec_low, infant_rec_high, adult_Q_low, adult_Q_high,
            infant_sev_low, infant_sev_high, infant_Med_low, infant_Med_high, 
            infant_imp_low, infant_imp_high, infant_Q_low, infant_Q_high
             )
t_mat <- matrix(t_list, 2, 12)
t_plot <- t_mat
t_plot[2,] <- t_plot[2,] - t_plot[1,]
barplot(t_plot, main = "Econ Parameters", xlab = "ICER", horiz = TRUE,
        names.arg = c("aHos", "aRec", "aSev", "aImp","iHos", "aMed", "iRec", 
                      "aQ", "iSev", "iMed", "iImp", "iQ"), 
        col = c("gray", "darkred"), xlim = c(90000, 158000))
