
# This script will read variable names as arguments from the command line
# and perform a linear regression using those variables from a housing dataset
# It will then output the summary of the regression model
# Usage: Rscript regression01.R <response_variable> <predictor_variable1> <predictor_variable2> ...
# Example: Rscript regression01.R price sqft_living bedrooms

# Read the arguments from the command line
args <- commandArgs(trailingOnly = TRUE)

# Check if at least two arguments are provided (one response and one predictor)
if (length(args) < 2) {
    stop("Please provide at least one response variable and one predictor variable.")
}
response_var <- args[1]
predictor_vars <- args[2:length(args)]
predictor_formula <- paste(predictor_vars, collapse = " + ")
formula <- as.formula(paste(response_var, "~", predictor_formula))
print(paste("Using formula:", deparse(formula)))

# load the housing dataset (loads an object named 'dat')
# You want to write code that will work on any computer
# Good practice is to assume that the working directory is set to the location of this script
# You should not use setwd() in scripts
# You should not use absolute paths in scripts
load("stlcounty_sales.RData")

# If the dataset were in a parallel directory called 'datasets', you would use:
# load("../datasets/stlcounty_sales.RData")

# fit the linear model
model <- lm(formula, data = dat)

# output the summary of the model
print("Regression model summary:")
print(summary(model))
print("End of regression01.R")
