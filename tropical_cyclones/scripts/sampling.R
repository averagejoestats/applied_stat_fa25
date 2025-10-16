

# simulate a poisson process on 0 to tmax by simulating the waiting times
# between events as exponential random variables with rate lambda
tmax <- 365
lambda <- 0.05 # expect about 18 events
S <- 0
E <- c()
while( TRUE ){
  T <- rexp(1, rate=lambda)
  if( S + T > tmax ){
      break
  } else {
      S <- S + T
      E <- c(E, S)
  }
}
length(E)
plot( E, 0.01*rnorm(length(E)), ylim = c(-1,1), pch=4, xlab="days", ylab="", yaxt='n' )



# simulate a poisson process on 0 to tmax by first simulating the number of events
# as a Poisson random variable with mean lambda*tmax, then simulating the event
# times as uniform random variables on (0,tmax)
J <- rpois(1, lambda*tmax)
E <- sort( runif(J, min=0, max=tmax) )
length(E)
plot( E, 0.01*rnorm(length(E)), ylim = c(-1,1), pch=4, xlab="days", ylab="", yaxt='n' )


# read in the HURDAT2 data and look at the dates of landfalling hurricanes
dat <- read.csv("../raw_data/hurdat2-1851-2024-040425.txt",header=FALSE)

# create an id column
dat$id <- NA

# fill in the id column with the id if the row starts with AL,
# otherwise fill it with the previous row's id
for(j in 1:nrow(dat)){
  if(substr(dat$V1[j],1,2)=="AL"){
    dat$id[j] <- dat$V1[j]
  } else {
    dat$id[j] <- dat$id[j-1]
  }
}

# remove rows that start with AL
dat <- dat[ substr(dat$V1,1,2) != "AL", ]

# add a date column
dat$date <- as.Date( dat$V1, format="%Y%m%d" )

# add a year column
dat$year <- as.numeric( format(dat$date, "%Y") )

# remove white space from V4
dat$V3 <- gsub(" ", "", dat$V3)
dat$V4 <- gsub(" ", "", dat$V4)

# fix the time column
dat$V2 <- as.numeric( gsub(" ", "", dat$V2) )

# get the times of landfalling hurricanes
# THIS HAS MULTIPLE ROWS FOR SOME HURRICANES!
# DON'T FOLLOW THIS BLINDLY!
HL <- dat[ dat$V3 == "L" & dat$V4 == "HU",]

# create a column for the day of year
HL$doy <- as.numeric( format(HL$date, "%j") )
HL$doy_decimal <- HL$doy + HL$V2/2400
HL <- HL[ order(HL$year + HL$doy_decimal/366), ]

hist( HL$doy_decimal, breaks= 0:366 )

p <- seq(0,1,by=0.001)
q <- quantile( HL$doy_decimal, probs = p )

plot( p, q, type='l' )

# heterogenous poisson process:
# first simulate the total number of events
J <- rpois(1, lambda=0.05*365)
# then simulate the event times from distribution proportional to rate function

# get the empirical quantile function
p <- seq(0,1,by=0.001)
q <- quantile( HL$doy_decimal, probs = p )

plot( p, q, type='l' )


# sample directly from the empirical distribution
E <- sample( HL$doy_decimal, size=J, replace=TRUE )

plot( E, 0.01*rnorm(length(E)), ylim = c(-1,1), xlim = c(0,365), pch=4, xlab="days", ylab="", yaxt='n' )



# get the interarrival times
waiting_times <- diff( HL$year + HL$doy_decimal/366 )
hist( waiting_times, breaks=100 )



