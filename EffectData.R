# install.packages("triangle")
library("triangle")

# RSV Cost-Effectiveness in Mali
# Effect Data

# Life Expectancy at birth, males and females combined
le_mali <- 58 # life expectancy in Mali (World Bank, 2017)

# DALY disability weight for acute lower respiratory infections (LRTI)
dw_LRTI_severe <- 0.133 # severe LRTI (IHME GBD, 2017), for RSV-LRTI inpatient
dw_LRTI_severe_u <- rtriangle(trials, a = 0.088, b = 0.190 , c = 0.133) # uncertainty distribution
dw_LRTI_mod <- 0.051 # moderate LRTI (IHME GBD, 2017), for RSV-LRTI outpatient
dw_LRTI_mod_u <- rtriangle(trials, a = 0.032, b = 0.074, c = 0.051)

# disability weight for upper respiratory infections (URTI)
dw_URTI_mod <- 0.051 # moderate URTI (IHME GBD, 2017), for RSV-URTI outpatient
dw_URTI_mod_u <-rtriangle(trials, a = 0.032, b = 0.074, c = 0.051)
dw_URTI_mild <- 0.006 # mild URTI (IHME GBD, 2017), for RSV-URTI no care
dw_URTI_mild_u <-rtriangle(trials, a = 0.002, b = 0.012 , c = 0.006)

# Duration of illness
di_bc <- 8.5   
di_yrs <- di_bc/365
di <- rnorm(trials, 8.5, (1.5/1.96)) # uncertainty, 7-10 days (Mathers et al.)
di_yrs_u <- di/365

