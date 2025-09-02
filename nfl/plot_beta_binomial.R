
# don't use ggplot2

# first, plot the beta distribution for different values of alpha and beta with alpha = beta
x <- seq(0, 1, length.out = 100)
plot( x, dbeta(x, 0.1, 0.1), type = "l")
lines( x, dbeta(x, 1, 1), col = "red" )
lines( x, dbeta(x, 2, 2), col = "blue" )
lines( x, dbeta(x, 10, 10), col = "magenta" )
lines( x, dbeta(x, 100, 100), col = "goldenrod" )

# do the same thing, but with alpha != beta (alpha/(alpha+beta) = 1/3)
plot( x, dbeta(x, 0.1, 0.2), type = "l")
lines( x, dbeta(x, 1, 2), col = "red" )
lines( x, dbeta(x, 2, 4), col = "blue" )
lines( x, dbeta(x, 10, 20), col = "magenta" )
lines( x, dbeta(x, 100, 200), col = "goldenrod" )


# now plot the beta-binomial distribution for different values of alpha and beta with alpha = beta
library("extraDistr") # for dbbinom
n <- 17
x <- 0:n
plot( x, dbinom(x, n, 0.5), type = "h", lwd = 2, ylim = c(0, 0.2) )
lines( x + 0.1, dbbinom(x, n, 20.0, 20.0), col = "red", lwd = 2, type = "h" )
