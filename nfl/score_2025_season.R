
# read in the data
stand <- read.csv("raw_data/standings2025.csv")

# read in the probabilities
prob1 <- read.csv("submissions/sub1.csv")
prob2 <- read.csv("submissions/sub2.csv")
prob3 <- read.csv("submissions/sub3.csv")
prob4 <- read.csv("submissions/sub4.csv")
prob5 <- read.csv("submissions/sub5.csv")
probs <- list( prob1, prob2, prob3, prob4, prob5 )

# remove * and + from data$Tm in one line using gsub
stand$Tm <- gsub("[\\*\\+]", "", stand$Tm)

# simple projection: multiply win pct by 17 and round
stand$Wproj1 <- round( stand$W.L. * 17 )

# create a data frame for scoring the predictions
scores <- data.frame(
    team = stand$Tm,
    wins = stand$Wproj1,
    exp1 = NA, exp2 = NA, exp3 = NA, exp4 = NA, exp5 = NA,
    prob1 = NA, prob2 = NA, prob3 = NA, prob4 = NA, prob5 = NA,
    log_score1 = NA, log_score2 = NA, log_score3 = NA, log_score4 = NA, log_score5 = NA
)

# loop over the groups and the teams, filling in information
win_col <- "Wproj1"
for( j in 1:length(probs) ) {

    # extract the jth group's probs
    prob <- probs[[j]]

    # loop over the teams
    for( k in 1:nrow(stand) ) {

        # extract the projected number of wins
        proj_wins <- scores$wins[k]

        # figure out this team's expected number of wins
        ii <- prob$team == scores$team[k]
        scores[k, paste0("exp", j) ] <- sum( prob$wins[ii] * prob$prob[ii] )

        # get the probability assigned to this number of wins and record prob and log score
        rr <- which( prob$team == stand$Tm[k] & prob$wins == proj_wins )
        scores[k, paste0("prob", j) ] <- prob$prob[rr]
        scores[k, paste0("log_score", j) ] <- log( prob$prob[rr] )

    }
}

# plot the projected wins for each team, and each group's expected wins
cols <- c("black","blue","magenta","green","darkorange")
par(mfrow = c(1,2), mar = c(5,10,2,2) )
for( inds in list( 1:16, 17:32 ) ) {
    plot( scores$wins[inds], 1:16,
        xlim = c(0,17), ylim = c(1,16), ylab = "", xlab = "Wins", axes = FALSE, pch = 8
    )
    points( scores$exp1[inds], 1:16, col = cols[1], pch = 16 )
    points( scores$exp2[inds], 1:16, col = cols[2], pch = 16 )
    points( scores$exp3[inds], 1:16, col = cols[3], pch = 16 )
    points( scores$exp4[inds], 1:16, col = cols[4], pch = 16 )
    points( scores$exp5[inds], 1:16, col = cols[5], pch = 16 )
    axis( 1 )
    axis( 2, at = 1:16, labels = scores$team[inds], las = 2 )
    box()
    if( inds[[1]] == 1 ) {
        legend( "topright",
            legend = c("group 1", "group 2", "group 3","group 4", "group 5"),
            col = cols, pch = 16
        )
    }
}

# score based on root mean squared error
c(
    sqrt( mean( ( scores$wins - scores$exp1 )^2 ) ),
    sqrt( mean( ( scores$wins - scores$exp2 )^2 ) ),
    sqrt( mean( ( scores$wins - scores$exp3 )^2 ) ),
    sqrt( mean( ( scores$wins - scores$exp4 )^2 ) ),
    sqrt( mean( ( scores$wins - scores$exp5 )^2 ) )
)


# plot the probabilities for the New England Patriots
this_team <- "New England Patriots"
ii <- prob1$team == this_team
plot( prob1$wins[ii], prob1$prob[ii], col = cols[1], pch = 16, type = "o", ylim = c(0, 0.25 ) )
ii <- prob2$team == this_team
points( prob2$wins[ii], prob2$prob[ii], col = cols[2], pch = 16, type = "o" )
ii <- prob3$team == this_team
points( prob3$wins[ii], prob3$prob[ii], col = cols[3], pch = 16, type = "o" )
ii <- prob4$team == this_team
points( prob4$wins[ii], prob4$prob[ii], col = cols[4], pch = 16, type = "o" )
ii <- prob5$team == this_team
points( prob5$wins[ii], prob5$prob[ii], col = cols[5], pch = 16, type = "o" )
abline( h = 0 )
abline( v = stand$Wproj1[ stand$Tm == this_team ], lty = 2 )
legend( "topright",
    legend = c("group 1", "group 2", "group 3","group 4", "group 5"),
    col = cols, pch = 16
)
mtext( paste("Probabilities for", this_team), side = 3, line = 1 )

# log scores for this_team, using prob4 as a baseline
rr <- scores$team == this_team
scores[rr,]

scores[rr,"prob1"]/scores[rr,"prob4"]
scores[rr,"prob2"]/scores[rr,"prob4"]
scores[rr,"prob3"]/scores[rr,"prob4"]
scores[rr,"prob4"]/scores[rr,"prob4"]
scores[rr,"prob5"]/scores[rr,"prob4"]

log(scores[rr,"prob1"]/scores[rr,"prob4"])
log(scores[rr,"prob2"]/scores[rr,"prob4"])
log(scores[rr,"prob3"]/scores[rr,"prob4"])
log(scores[rr,"prob4"]/scores[rr,"prob4"])
log(scores[rr,"prob5"]/scores[rr,"prob4"])


# plot the scores for each team
ref_score <- scores$log_score4

par(mfrow = c(1,2), mar = c(5,10,2,2) )
for( inds in list( 1:16, 17:32 ) ) {
    plot( NA, type = "n", xlim = c(-5,5), ylim = c(1,16), ylab = "", xlab = "Log Score", axes = FALSE )
    points( scores$log_score1[inds] - ref_score[inds], 1:16, col = cols[1], pch = 16 )
    points( scores$log_score2[inds] - ref_score[inds], 1:16, col = cols[2], pch = 16 )
    points( scores$log_score3[inds] - ref_score[inds], 1:16, col = cols[3], pch = 16 )
    points( scores$log_score4[inds] - ref_score[inds], 1:16, col = cols[4], pch = 16 )
    points( scores$log_score5[inds] - ref_score[inds], 1:16, col = cols[5], pch = 16 )
    abline( v = 0 )
    axis( 1 )
    axis( 2, at = 1:16, labels = scores$team[inds], las = 2 )
    box()
    legend( "topright",
        legend = c("group 1", "group 2", "group 3","group 4", "group 5"),
        col = cols, pch = 16
    )
}

# print the sums
colSums( scores[ paste0("log_score", 1:5) ] )

rel_scores <- colSums( scores[ paste0("log_score", 1:5) ] ) - sum( ref_score )
rel_scores

# geometric average ratio of probabilities
exp(rel_scores/32)

scores[ c("team", "prob1", "prob2", "prob3", "prob4", "prob5" ) ]

