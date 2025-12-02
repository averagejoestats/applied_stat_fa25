
# Style Guide:
# For functions with many arguments, use this style:
# hist(
#     sims, breaks = (-0.5):(17.5), freq = FALSE, 
#     xlab = "Number of Wins", ylab = "Probability",
#     main = "Simulated Distribution of Wins for Houston Texans in 2025"
# )
# Nothing on the first line after the opening parenthesis
# Each argument on its own line, indented by 4 spaces
# For Short calls, putting everything on one line is fine


# read in the data 
data <- read.csv("raw_data/standings.csv")
head(data)

# remove * and + from data$Tm in one line using gsub
data$Tm <- gsub("[\\*\\+]", "", data$Tm)
# write a comment explaining the regex above
# The regex "[\\*\\+]" matches either a '*' or a '+' character
# say more about how [] works
# The square brackets [] denote a character class, meaning it will match any one of the characters
# say more about the double backslash
# The double backslash \\ is used to escape the special characters * and +,

# redefine team names
data$Tm[data$Tm == "Oakland Raiders"] <- "Las Vegas Raiders"
data$Tm[data$Tm == "St. Louis Rams"] <- "Los Angeles Rams"
data$Tm[data$Tm == "San Diego Chargers"] <- "Los Angeles Chargers"
data$Tm[data$Tm == "Washington Redskins"] <- "Washington Commanders"
data$Tm[data$Tm == "Washington Football Team"] <- "Washington Commanders"

# calculate prior wins and losses
data$prior_wins <- NA
data$prior_losses <- NA
data$prior_SoS <- NA

for( j in 1:nrow(data) ) {
    ii <- which( data$Tm == data$Tm[j] & data$year == data$year[j] - 1 )
    if( length(ii) == 1 ){
        data$prior_wins[j] <- data$W[ii]
        data$prior_losses[j] <- data$L[ii]
        data$prior_SoS[j] <- data$SoS[ii]
    }
}

# calculate prior percent wins
data$prior_pct <- data$prior_wins / (data$prior_wins + data$prior_losses)

# try fitting a binomial glm for wins and losses based on prior percent wins
m1 <- glm( cbind(W, L) ~ prior_pct, data = data, family = binomial(link = "logit") )

# try a transformation
data$logit_prior_pct <- log( data$prior_pct / (1 - data$prior_pct) )
# handle the case where prior_pct is 0
data$logit_prior_pct[ data$prior_pct == 0 ] <- log( (0.5/17) / (16.5/17) )
m2 <- glm( cbind(W, L) ~ logit_prior_pct, data = data, family = binomial(link = "logit") )
summary(m2)

# make a prediction dataset for 2025
pred_data <- data[ data$year == 2024, c("Tm", "W", "L") ]
pred_data$prior_pct <- pred_data$W / (pred_data$W + pred_data$L)
pred_data$logit_prior_pct <- log( pred_data$prior_pct / (1 - pred_data$prior_pct) )

# get predictions using pred_data and model m1
pred_df <- predict( m2, newdata = pred_data, type = "response", se.fit = TRUE )
pred_data$pred_prob <- pred_df$fit
pred_data$pred_se <- pred_df$se.fit

# keep the pred data
pred_data1 <- pred_data

# Fit a beta binomial model
library( "aod" )
#?betabin
data2 <- data[ !is.na(data$prior_pct), ]
m3 <- betabin( cbind(W, L) ~ logit_prior_pct, random  = ~ 1, data = data2 )
summary(m3)

# get the overdispersion parameter
phi <- m3@random.param

# make predictions for 2025 using the beta binomial model
pred_bb <- predict( m3, newdata = pred_data, type = "response", se.fit = TRUE )
pred_data$prob_prob_bb <- pred_bb$fit

# keep the pred data
pred_data2 <- pred_data

# fill in the predictions using a binomial glm and beta binomial glm
pred_csv1 <- read.csv("templates/team_prob_template_2025.csv")
pred_csv2 <- read.csv("templates/team_prob_template_2025.csv")
teams <- unique( pred_csv1$team )

for(j in 1:length(teams)){

    # get the binomial glm prediction
    # find the appropriate rows in the csv
    ii <- which( pred_csv1$team == teams[j] )

    # get this team's binomial glm probabilities
    p <- pred_data1$pred_prob[ pred_data$Tm == teams[j] ]
    pred_csv1$prob[ii] <- dbinom( 0:17, size = 17, prob = p )


    # get this team's beta-binomial glm probabilities
    p_bb <- pred_data2$prob_prob_bb[ pred_data2$Tm == teams[j] ]
    alpha <- p_bb * ( 1/phi - 1 )
    beta <- (1 - p_bb) * ( 1/phi - 1 )
    pred_csv2$prob[ii] <- extraDistr::dbbinom( 0:17, size = 17, alpha = alpha, beta = beta )

}

# write the csv files
write.csv( pred_csv1, file = "submissions/sub4.csv", row.names = FALSE )
write.csv( pred_csv2, file = "submissions/sub5.csv", row.names = FALSE )
