## Figures for presentations
## Initially: Frontiers 2019
library(ggplot2)
library("RColorBrewer")

# UMB color palette
UMBred <- rgb(200, 16, 46, maxColorValue = 255)
UMByellow <- rgb(255, 205, 0, maxColorValue = 255)
UMBblue <- rgb(0, 118, 152, maxColorValue = 255)
UMBslate <- rgb(93, 135, 161, maxColorValue = 255)
UMBgray <- rgb(149, 160, 169, maxColorValue = 255)
UMBcharcoal <- rgb(105, 106, 109, maxColorValue = 255)
UMBplum <- rgb(73, 24, 45, maxColorValue = 255)
UMBforest <- rgb(51, 70, 13, maxColorValue = 255)
UMBsea <- rgb(180, 204, 149, maxColorValue = 255)
UMBtan <- rgb(200, 177, 139, maxColorValue = 255)

par(lwd = 3)

CET <- rep.int(891, 5001) #  Mali GDP pc
#####

# # Uncertainty across WTP threshold
quartz("Uncertainty_2ab", 10, 4)
par(lwd = 3, xaxs = "i", yaxs = "i")
par(mfrow = c(1,2))
par(mar = c(4, 4, 2, 2))

plot(WTP_sp, pce_donor_llAb_3, ylim = c(0, 1), xlim = c(0,10000), bty = "l",
     type = "l", col = UMBforest,
     xlab = "Donor willingness to pay (USD)",
     ylab = "Probability cost-effective")
lines(WTP_sp, pce_donor_llAb_9, col = UMBforest, lty = 2)
lines(WTP_sp, pce_donor_mAb_3, col = UMBsea, lty = 1)
lines(WTP_sp, pce_donor_mAb_9, col = UMBsea, lty = 2)
lines(WTP_sp, pce_donor_mVax_3, col = UMBblue, lty =1)
lines(WTP_sp, pce_donor_mVax_9, col = UMBblue, lty = 2)
abline( v = CET_Mali_GDP, col = UMBgray, lty = 3)
text(CET_Mali_GDP, 0.92, labels = "1xGDP", srt = 45, cex = 0.80)
# abline(v = 3* CET_Mali_GDP, col = UMBgray, lty = 3)
# text(3*CET_Mali_GDP, 0.92, labels = "3xGDP", srt = 45, cex = 0.80)
legend(locator(1), legend = c("Short-acting mAb, $3", "Short-acting mAb, $9", "Long-acting mAb, $3", "Long-acting mAb, $9", "Maternal vaccine, $3", "Maternal vaccine, $9"), cex = 0.8, lty = c(1, 2, 1, 2, 1, 2),
       col = c(UMBsea, UMBsea, UMBforest, UMBforest, UMBblue, UMBblue), bty = "n")

plot(WTP_sp, pce_gov_llAb, ylim = c(0, 1), xlim = c(0,10000), bty = "l",
     type = "l", col = UMBforest,
     xlab = "Government willingness to pay (USD)",
     ylab = "Probability cost-effective")
lines(WTP_sp, pce_gov_mAb, col = UMBsea, lty =1)
lines(WTP_sp, pce_gov_mVax, col = UMBblue, lty =1)
abline( v = CET_Mali_GDP, col = UMBgray, lty = 3)
text(CET_Mali_GDP, 0.92, labels = "1xGDP", srt = 45, cex = 0.8)
# abline(v = 3* CET_Mali_GDP, col = UMBgray, lty = 3)
# text(3*CET_Mali_GDP, 0.92, labels = "3xGDP", srt = 45, cex = 0.8)
legend("bottomright", legend = c("Long-acting mAb", "Short-acting mAb", "Maternal vaccine"), lty = c(1, 1, 1),
       col = c(UMBforest, UMBsea, UMBblue), bty = "n")
quartz.save(file = "Figures/uncertainty_fig2ab.pdf", type = "pdf")

# # Probability mVax has greater NHB than llAb
# ####
# quartz("mVax vs llAb", 4, 4)
# par(lwd = 3)
# par(mfrow = c(1,1))
# par(mar=c(4,4,2,2))
# filled.contour(mVax_h2h, llAb_h2h, prob_matrix, nlevels =6, col = brewer.pal(6, "Reds"), lwd =3, 
#                xlab = "Maternal vaccine cost (USD)", ylab = "Long-acting mAb cost (USD)", cex.lab = 0.9)
# quartz.save(file = "Figures/mVax_vs_llAb.pdf", type = "pdf")

#####

quartz("DALYandICER", 7, 4)
par(lwd = 3, xaxs = "i", yaxs = "i")
par(mfrow = c(1,2))
par(mar = c(4, 4, 2, 2))
int_cov <- seq(0, 100, by = 20)

plot(int_cov, DALY_saved_mAb, ylim = c(0, 1750), col = UMBsea, type = "l", bty = "l",
     xlab = "Intervention coverage (%)",
     ylab = "DALYs averted")
lines(int_cov, DALY_saved_mVax, col = UMBblue)
lines(int_cov, DALY_saved_llAb, col = UMBforest)
legend("topleft", legend = c("Maternal vaccine", "Short-acting mAb", "Long-acting mAb"), lty = c(1,1,1), bty = "n",
       col = c(UMBblue, UMBsea, UMBforest, UMBblue, UMBforest))

pricelim <- 71
plot(cost_range[1:pricelim], ICER_range_mVax[1:pricelim], col = UMBblue, type = "l", bty = "l",
     xlab = "Cost per dose (USD, product + delivery)",
     ylab = "ICER")
lines(cost_range, ICER_range_mAb, col = UMBsea)
lines(cost_range, ICER_range_llAb, col = UMBforest)
lines(cost_range, CET, col = UMBgray, lty = 3)
text(6, CET_Mali_GDP + 250, labels = "1xGDP", cex = 0.80)
# lines(cost_range, CET*3, col = UMBgray, lty =3)
# text(6, 3*CET_Mali_GDP + 250, labels = "3xGDP", cex = 0.80)
legend("topleft", inset = c(-.05, 0), legend =c("Maternal vaccine", "Short-acting mAb", "Long-acting mAb"), 
       lty = c(1,1,1), col = c(UMBblue, UMBsea, UMBforest), bty = "n")
quartz.save(file = "Figures/DALYSandICER.pdf", type = "pdf")
###


#####

quartz("costs per death averted", 7, 4)
par(lwd = 3, xaxs = "i", yaxs = "i")
par(mfrow = c(1,1))
par(mar = c(4, 4, 2, 2))
# plot(cost_range, CPC_mVax, xlim = c(0, 10), ylim = c(0,250), col = UMBblue, type = "l", bty = "l",
#      xlab = "Cost per dose (USD, product + delivery)",
#      ylab = "Costs per RSV case averted ratio")
# lines(cost_range, CPC_mAb, col = UMBsea)
# lines(cost_range, CPC_llAb, col = UMBforest)
# legend("topleft", inset = c(-.05, 0), legend =c("Maternal vaccine", "Short-acting mAb", "Long-acting mAb"), 
#        lty = c(1,1,1), col = c(UMBblue, UMBsea, UMBforest), bty = "n")

plot(cost_range, CPD_mVax, xlim = c(0, 10), ylim = c(0, 500000), col = UMBblue, type = "l", bty = "l",
     xlab = "Cost per dose (USD, product + delivery)",
     ylab = "Costs per death averted ratio")
lines(cost_range, CPD_mAb, col = UMBsea)
lines(cost_range, CPD_llAb, col = UMBforest)
legend("topleft", legend =c("Maternal vaccine", "Short-acting mAb", "Long-acting mAb"), 
       lty = c(1,1,1), col = c(UMBblue, UMBsea, UMBforest), bty = "n")
quartz.save(file = "Figures/GAVI_analysis.pdf", type = "pdf")
##

# Sensitivity analysis tornado plot mAb
###

# quartz("tornadoplot", 15, 4)
quartz("sensitivty mAb", 7, 4)
par(lwd = 3)
par(mfrow = c(1,1))
# par(mfrow = c(1,3))
par(mar = c(4, 16, 2, 2), mgp = c(3,1,0), las = TRUE)

tornado_mAb <-((ct_mat_mAb- ct_mAb_bc)/ ct_mAb_bc) * 100
colnames(tornado_mAb) <-c("Outpatient care costs", "Disability weight RSV-LRTI, inpatient", "Disability weight RSV-LRTI, outpatient", "Duration of illness", "Inpatient care costs", "Age-based attack rates 0-3m", "Intervention efficacy", "Age-based attack rates 3-6m", "Probability RSV-LRTI", "Probability RSV-LRTI inpatient", "Case fatality rate RSV-LRTI inpatient")
barplot(tornado_mAb[1,], horiz = T, las=1, xlim = c(-300, 300), xaxt = 's', ylab ='',
        beside=T, col = c(UMBsea), main = "Short-acting mAb", xlab = "Percent change ICER")
barplot(tornado_mAb[2,], horiz = T, las =1, xlim = c(-300,300), xaxt = 's', ylab = '',
        beside = T, col = c(UMBsea), add = TRUE)
quartz.save(file = "Figures/tornado_mAb.pdf", type = "pdf" )
###

# Sensitivity analysis tornado plot llAb
###

quartz("sensitivty llAb", 5, 4)
par(lwd = 3)
par(mfrow = c(1,1))
par(mar = c(4, 6, 2, 2), mgp = c(3,1,0), las = TRUE)
tornado_llAb <-((ct_mat_llAb- ct_llAb_bc)/ ct_llAb_bc) * 100
barplot(tornado_llAb[1,], horiz = T, las=1, xlim = c(-300, 300), xaxt = 's', ylab ='',
        beside=T, col = c(UMBforest), main = "Long-acting mAb", xlab = "Percent change ICER")
barplot(tornado_llAb[2,], horiz = T, las =1, xlim = c(-300, 300), xaxt = 's', ylab = '',
        beside = T, col = c(UMBforest), add = TRUE)
quartz.save(file = "Figures/tornado_llAb.pdf", type = "pdf" )
###

# Sensitivity analysis tornado plot mVax
###

quartz("sensitivity mVax", 5, 4)
par(lwd = 3)
par(mfrow = c(1,1))
par(mar = c(4, 6, 2, 2), mgp = c(3,1,0), las = TRUE)

tornado_mVax <-((ct_mat_mVax - ct_mVax_bc)/ ct_mVax_bc) * 100
barplot(tornado_mVax[1,], horiz = T, las=1, xlim = c(-300, 300), xaxt = 's', ylab ='',
        beside=T, col = c(UMBblue), main = "Maternal vaccine", xlab = "Percent change ICER")
barplot(tornado_mVax[2,], horiz = T, las =1, xlim = c(-300, 300), xaxt = 's', ylab = '',
        beside = T, col = c(UMBblue), add = TRUE)
quartz.save(file = "Figures/tornado_mVax.pdf", type = "pdf")
# quartz.save(file = "Figures/tornadoplot.pdf, type = pdf")
###

# CFR histogram overlay
###

CFR_perch <- data.frame(CFR = CFR_inpatient_u)
CFR_cbs <- data.frame(CFR = rbeta(trials, 1, 12))
CFR_perch$source <- 'PERCH Mali'
CFR_cbs$source <- 'Buchwald et al.'
CFR_data <- rbind(CFR_perch, CFR_cbs)

quartz("CFR overlay", 5, 4)
ggplot(CFR_data, aes(CFR, fill = source)) + geom_density(alpha = 0.5) +
  xlab("Case fatality rate") + ylab("Density") + theme(legend.title=element_blank())
quartz.save(file = "CFR_overlay.tiff", type = "tiff")
###

# AR matrix shaded by intensity
###
AR_bc[AR_bc == 0] <- NA
x = 1:ncol(AR_bc)
y = 1:nrow(AR_bc)
centers <- expand.grid(y,x)
par(lwd = 3)
par(mfrow = c(1,1))
par(mar = c(5.1, 4.1, 4.1, 2.1))
image(x, y, t(AR_bc),
      col = brewer.pal(7, "Reds"),
      breaks = c(0, 0.0005487, 0.00165, 0.00494, 0.0148, 0.0444, 0.133, 0.4),
      xaxt = 'n',
      yaxt = 'n',
      xlab = '', 
      ylab = '',
      ylim = c(max(y) + 0.5, min(y) - 0.5)
)
text(centers[,2], centers[,1], c(round(AR_bc, 3)))
mtext(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May"), at=1:ncol(AR_bc),side = 1)
mtext("Calendar month", side = 1, line = 1, cex = 1.5)
mtext(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), at=1:nrow(AR_bc), side = 2, las = 1, adj = 1.2)
mtext("Birth month", side = 2, line = 2, cex = 1.5)
abline(h=y + 0.5)
abline(v=x + 0.5)
###


###
quartz("Cases", 7, 4)
par(lwd = 3, xaxs = "i", yaxs = "i")
par(mfrow = c(1,1))
par(mar = c(4, 4, 2, 2))
int_cov <- seq(0, 100, by = 20)

plot(int_cov, (SA_mAbcov_cases[1] -SA_mAbcov_cases)/1000, ylim = c(0, 110), col = UMBsea, type = "l", bty = "l",
     xlab = "Intervention coverage (%)",
     ylab = "Cases averted (thousands)")
lines(int_cov, (SA_mVcov_cases[1] - SA_mVcov_cases)/1000, col = UMBblue)
lines(int_cov, (SA_llcov_cases[1] - SA_llcov_cases)/1000, col = UMBforest)
legend("topleft", legend = c("ResVax™", "palivizumab", "nirsevimab"), lty = c(1,1,1), bty = "n",
       col = c(UMBblue, UMBsea, UMBforest, UMBblue, UMBforest))
quartz.save(file = "Figures/FrontiersCases.pdf", type = "pdf")

###

# data frame for scenario analysis plot
d <- data.frame(Scenario = c("Short-acting mAb","Short-acting mAb, URTI included", "Short-acting mAb, appropriate care", "Long-acting mAb", "Maternal vaccine", "Maternal vaccine, URTI included", "Maternal vaccine, appropriate care", "Maternal vaccine preferred characteristics", "Maternal vaccine, ResVax™ global dataset", "Maternal vaccine pre-seasonal administration", "Long-acting mAb birth-dose + catch-up", "Long-acting mAb, URTI included", "Long-acting mAb, appropriate care"),
                CER = c(ct_mAb_bc, ct_URTI_mAb_bc, ct_cSA_mAb_bc, ct_llAb_bc, ct_mVax_bc, ct_URTI_mVax_bc, ct_cSA_mVax_bc, ct_PPC_mVax, ct_CmVax, ct_ps_mVax_bc, ct_SA_llAb_bc, ct_URTI_llAb_bc, ct_cSA_llAb_bc),
                lower = c(CI_ct_mAb[1], CI_ct_URTI_mAb[1], CI_ct_cSA_mAb[1], CI_ct_llAb[1], CI_ct_mVax[1], CI_ct_URTI_mVax[1], CI_ct_cSA_mVax[1], CI_ct_PPC_mVax[1] , CI_ct_CmVax[1], CI_ps_mVax[1], CI_ct_SA_llAb[1], CI_ct_URTI_llAb[1], CI_ct_cSA_llAb[1]),
                upper = c(CI_ct_mAb[2],CI_ct_URTI_mAb[2], CI_ct_cSA_mAb[2], CI_ct_llAb[2], CI_ct_mVax[2], CI_ct_URTI_mVax[2], CI_ct_cSA_mVax[2], CI_ct_PPC_mVax [2], CI_ct_CmVax[2], CI_ps_mVax[2], CI_ct_SA_llAb[2], CI_ct_URTI_llAb[2], CI_ct_cSA_llAb[2]))
quartz("Scenarios", 11, 7)
par(lwd = 3)
par(mfrow = c(1,1))
par(mar = c(4, 4, 2, 2))
ggplot() + 
  geom_point(data = d, mapping = aes(x = CER, y = Scenario), size = 2, shape = 21, fill = c(UMBsea, UMBsea, UMBsea, UMBforest, UMBblue, UMBblue, UMBblue, UMBblue, UMBblue, UMBblue, UMBforest, UMBforest, UMBforest)) +
  geom_errorbar(data = d, mapping = aes(y= Scenario, xmin = lower, xmax = upper), width = 0.2, size =1, color = c(UMBsea, UMBsea, UMBsea, UMBforest, UMBblue, UMBblue, UMBblue, UMBblue, UMBblue, UMBblue, UMBforest, UMBforest, UMBforest)) +
  theme(text = element_text(size=15)) +
  xlab("Incremental cost-effectiveness ratio") + 
  ylab("")
quartz.save(file = "Figures/Scenarios.pdf", type = "pdf")

####