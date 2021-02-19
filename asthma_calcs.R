## Asthma/ Wheeze calcs
OR_low <- 2.45
OR_hi <- 4.17
p_asthma <- c(OR_low, OR_hi) * 0.091
asthma_epi_no <- (cases_no_bc - (cases_no_bc * (110.677/1000))) * p_asthma
asthma_epi_mAb <- (cases_mAb_bc - (cases_mAb_bc * (110.677/1000))) * p_asthma
asthma_epi_llAb <- (cases_llAb_bc - (cases_llAb_bc * (110.677/1000))) * p_asthma
asthma_epi_mVax <- (cases_mVax_bc - (cases_mVax_bc * (110.677/1000))) * p_asthma

epi_prev_mAb <- asthma_epi_no - asthma_epi_mAb
epi_prev_llAb <- asthma_epi_no - asthma_epi_llAb
epi_prev_mVax <- asthma_epi_no - asthma_epi_mVax

asthma_epi_no_u <- (cases_no_u - (cases_no_u * (110.677/1000))) * p_asthma[2]
asthma_epi_mAb_u <- (cases_mAb_u - (cases_mAb_u * (110.677/1000))) * p_asthma[2]
asthma_epi_llAb_u <- (cases_llAb_u - (cases_llAb_u * (110.677/1000))) * p_asthma[2]
asthma_epi_mVax_u <- (cases_mVax_u - (cases_mVax_u * (110.677/1000))) * p_asthma[2]

epi_prev_mAb_u <- asthma_epi_no_u - asthma_epi_mAb_u
epi_prev_llAb_u <- asthma_epi_no_u - asthma_epi_llAb_u
epi_prev_mVax_u <- asthma_epi_no_u - asthma_epi_mVax_u

CI_func(epi_prev_mAb_u)
CI_func(epi_prev_llAb_u)
CI_func(epi_prev_mVax_u)
