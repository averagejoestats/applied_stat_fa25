
# load the yearly count data
load("../processed_data/yearly_hurdat.RData")

# calculate the average number of yearly landfalling hurricanes
m <- mean( yearly$n_landfall_hurricanes )

# calculate the average before and after 1970
m1 <- mean( yearly$n_landfall_hurricanes[ yearly$year < 1970 ] )
m2 <- mean( yearly$n_landfall_hurricanes[ yearly$year >= 1970 ] )

# no change
summary(glm( n_landfall_hurricanes ~ year, data=yearly, family=poisson() ))


# calculate the average number of hurricanes per year before and after 1970
m1 <- mean( yearly$n_hurricanes[ yearly$year < 1970 ] )
m2 <- mean( yearly$n_hurricanes[ yearly$year >= 1970 ] )

# statisticall significant increase
summary(glm( n_hurricanes ~ year, data=yearly, family=poisson() ))

# Let's try doing a simulation-based inference
m <- mean( yearly$n_hurricanes )

# Model: Y ~ Poisson(mu1) for year < 1970 and Y ~ Poisson(mu2) for year >= 1970
# H0: mu1 = mu2 = 5.59
# test statistic: |m1 - m2| (diff of sample means)
# what is the distribution of the test statistic under H0?
# Skellam distribution? Not quite
# using the model and H0, we can simulate the distribution of the test statistic
m <- mean( yearly$n_hurricanes )
m1 <- mean( yearly$n_hurricanes[ yearly$year < 1970 ] )
m2 <- mean( yearly$n_hurricanes[ yearly$year >= 1970 ] )
n_sim <- 1e5
sim_stat <- rep(NA, n_sim)
n1 <- sum( yearly$year < 1970 )
n2 <- sum( yearly$year >= 1970 )
for(j in 1:n_sim){
    y1 <- rpois(n1, m)
    y2 <- rpois(n2, m)
    sim_stat[j] <- abs( mean(y1) - mean(y2) )
}

# histogram of the simulated test statistic
hist( sim_stat, breaks=50 )
abline( v=abs(m2-m1), col="red", lwd=2 )
# p-value
mean( sim_stat >= abs(m2-m1) )
sum( sim_stat >= abs(m2-m1) )



# let's do usa landfalling hurricanes for fun
# Model: Y ~ Poisson(mu1) for year < 1970 and Y ~ Poisson(mu2) for year >= 1970
# H0: mu1 = mu2 = 5.59
# test statistic: |m1 - m2| (diff of sample means)
# what is the distribution of the test statistic under H0?
# Skellam distribution? Not quite
# using the model and H0, we can simulate the distribution of the test statistic
m <- mean( yearly$n_landfall_hurricanes )
m1 <- mean( yearly$n_landfall_hurricanes[ yearly$year < 1970 ] )
m2 <- mean( yearly$n_landfall_hurricanes[ yearly$year >= 1970 ] )
n_sim <- 1e5
sim_stat <- rep(NA, n_sim)
n1 <- sum( yearly$year < 1970 )
n2 <- sum( yearly$year >= 1970 )
for(j in 1:n_sim){
    y1 <- rpois(n1, m)
    y2 <- rpois(n2, m)
    sim_stat[j] <- abs( mean(y1) - mean(y2) )
}

# histogram of the simulated test statistic
hist( sim_stat, breaks=50 )
abline( v=abs(m2-m1), col="red", lwd=2 )
# p-value
mean( sim_stat >= abs(m2-m1) )
sum( sim_stat >= abs(m2-m1) )
