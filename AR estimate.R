#########################################
## build AR matrix in R

# source("birth cohort.R")
# for one-year scenario analysis  ### MCF added
ft <- 23   # 1 year follow-up time
empty_year_cohort <-  matrix (0, 12, ncol = ft)# extrapolate age-based attack rates out to 12 months, for 6-12 months project linear decline starting with AR at 6mo. and ending with AR at3mo.

# months start in January  
sim_mo <- 17
empty_cohort <- matrix (0, 12, ncol = sim_mo)

# incidence rate infant RSV cases in Mali per 1000 person-years, divided by 12 for rate in a single month
mali_inc <- 536.8/12

# raw data for incidence by calendar month
cal_dat <- data.frame(month = seq(1,12, by =1), RSV_pos = c(7,6,4,2,2,1,6,36,78,5,5,5), num_samples = c(56,63,71,95,109,91,68,100,122,56,47,39))

# calculate the proportion of samples that were positive in each month
prop_pos <- cal_dat$RSV_pos / cal_dat$num_samples

# calculate monthly attack rate estimates spanning 1 calendar year
cal_AR <- prop_pos / mean(prop_pos) * mali_inc / 1000

# raw data for incidence by infant age in months
# follow-up time is reported in person-years
age_dat <- data.frame(age = seq(1,6, by =1), cases = c(8,11,28,27,47,32), follow_up = c(56.5,52.7,51.9,46.9,44.9,32.1))

# calculate age-based attack rate estimates, 0 to 6 months
age_AR <- age_dat$cases / (age_dat$follow_up*12)

# develop the base case AR matrix
# start with calendar month AR estimate, divide by overall incidence rate (mali_inc/1000), then multiply by age-based AR estimate
temp1 <- empty_cohort

for (a in 1:12) {
  temp1[a,] <- c(rep(0, times = a-1), age_AR, rep(0, times = 12-a))
}

cal_AR2 <- c(cal_AR, cal_AR[1:5])
temp2 <- matrix(rep(cal_AR2, each = 12), 12, 17)

AR_bc <- temp2/(mali_inc/1000)*temp1

# incorporate age-based uncertainty into ARs

AR_age_sample <- function (cases, fu, num_tri) {
  ARvec <- rbeta(num_tri, cases, fu*12 - cases)
}

AR_age_u <- mapply(AR_age_sample, age_dat$cases, age_dat$follow_up, trials)

temp1_u <- array(0, dim = c(12, 17, trials))

for (a in 1:12) {
  temp1_u[a,a:(a+5),] <- t(AR_age_u)
}

temp2_u <- array(rep(rep(cal_AR2, each = 12), trials), dim = c(12, 17, trials))

AR_u <- temp2_u/(mali_inc/1000)*temp1_u

### create AR matrices for birth cohorts followed for one-year (scenario analysis)


age_AR_y <- c(age_AR, seq(age_AR[6], age_AR[3], length = 6))

temp3 <- empty_year_cohort
for (a in 1:12) {
  temp3[a,] <- c(rep(0, times = a-1), age_AR_y, rep(0, times = 12-a))
}

cal_AR_y <- c(cal_AR, cal_AR[1:11])
temp4 <- matrix(rep(cal_AR_y, each = 12), 12, 23)

AR_y_bc <- temp4/(mali_inc/1000)*temp3

AR_cases_year <- c(age_dat$cases, seq(age_dat$cases[6], age_dat$cases[3], length = 6))
AR_ft_year <- c(age_dat$follow_up, seq(age_dat$follow_up[6], age_dat$follow_up[3], length =6))
AR_age_y_u <- mapply(AR_age_sample, AR_cases_year, AR_ft_year, trials)

temp3_u <- array(0, dim = c(12, 23, trials))
for (a in 1:12) {
  temp3_u[a,a:(a+11),] <- t(AR_age_y_u)
}

temp4_u <- array(rep(rep(cal_AR_y, each = 12), trials), dim = c(12, 23, trials))
AR_y_u <- temp4_u/(mali_inc/1000)*temp3_u

