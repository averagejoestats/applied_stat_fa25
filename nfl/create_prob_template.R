
# read in the 2025 schedule data
data <- read.csv("raw_data/season2025.csv")

# remove the rows that start with "Pre" in the "Week" column
data <- data[!grepl("^Pre", data$Week), ]

# improve the column names
colnames(data) <- c("week","weekday","date","visitor","visitor_score","at","home","home_score","time")

# remove the score columns and the at column
data <- data[, c("week","weekday","date","visitor","home","time")]

# create a home probability column with NA values
data$home_prob <- NA

# write to a csv file
write.csv(data, "templates/game_prob_template_2025.csv", row.names = FALSE)


# create another template, which has a row for each team, and each number of wins from 0 to 17
teams <- unique(c(data$home, data$visitor))
prob_template <- expand.grid(wins = 0:17, team = sort(teams), prob = NA )
prob_template <- prob_template[ c("team","wins","prob")]

# write to a csv file
write.csv(prob_template, "templates/team_prob_template_2025.csv", row.names = FALSE)
