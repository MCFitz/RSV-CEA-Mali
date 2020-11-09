### Example loop
trials <- 1000
testing <- rnorm(trials, 500, 100)
thresholds <- seq(0, 800, by = 100)
prob_store <- rep(0, length(thresholds))

for (i in 1:length(thresholds)) {
  prob_store[i] <- sum(testing <= thresholds[i])
}

plot(thresholds, prob_store, type = "l")


thresholds[5]   #just use the 5th value of thresholds
