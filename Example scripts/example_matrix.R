## Building a matrix for cohort 

# here, months start in January (for example)
sim_mo <- 24
matvax <- matrix (0, 12, ncol = sim_mo)

# replace first four months of life with 1s, 
# because that's when matvax is effective

mv_dur <- 4 # matvax has 4 months duration

for (i in 1:12) {
  matvax[i, i:(i+(mv_dur-1))] <- 1
}

## ALTERNATIVE
matvax <- matrix (0, 12, ncol = sim_mo)
mv_vec <- c(0.95, 0.90, 0.7, 0.3)
for (i in 1:12) {
  matvax[i, i:(i+3)] <- mv_vec
}
