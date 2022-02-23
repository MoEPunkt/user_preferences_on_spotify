# clear workspace
rm(list = ls())

# Import the required packages
library(xgboost)
library(caret)

# Source the script that generates the dataset
source("./data_pred.R")

# Convert the variable top20 to numeric
data[, top20 := as.numeric(top20)]
# Create a new variable hit that contains the values from data$top20
hit <- data$top20

# Set the seed to 100
set.seed(100)

# Create three indices: training, test, validation
estIndex <- sample(1:3, size = nrow(data), replace = TRUE, p = c(0.7, 0.2, 0.1))

# Generate the train-, test-, validation dataset
trainset<- data[estIndex == 1,]
testset <- data[estIndex == 2,]
validationset <- data[estIndex == 3,]

# Generate the trainhit, testhit, and validationhit from hit
trainhit <- hit[estIndex ==1]
testhit <- hit[estIndex == 2]
validationhit <- hit[estIndex == 3] %>% as.factor()

# Remove the unneccesary variables from the training-, test-, and validationset
train_temp <- trainset %>% select(c(-top20, -n_days, -streams))
test_temp<- testset %>% select(c(-top20, -n_days, -streams))
val_temp <- validationset %>% select(c(-top20, -n_days, -streams))

# Create dMatrices from the training-, test-, and validationset
dMatrix <- xgb.DMatrix(label = trainset$top20, data = as.matrix(train_temp))
dMatrixTest <- xgb.DMatrix(label = testset$top20, data = as.matrix(test_temp))
dMatrixVal <- xgb.DMatrix(label = validationset$top20, data = as.matrix(val_temp))

# Set up the parameter list for the xgBoost model
# Here, "gbtree" is used as booster, the learning rate has the value 0.4,
# trees are limited to a maximum depth of 25, and the minimum loss reduction is
# set to 0.01, the learning objective is a logistic regression for binary
# classification
param <- list(booster = "gbtree", eta = 0.4, max_depth = 25, gamma = 0.01,
                  objective = "binary:logistic")
# The model performance is evaluated with dMatrixTest
wl <- list(validation = dMatrixTest)
# Train the xgBoost model for the set up parameters and watchlist
t20_xgb <- xgb.train(params = param, data = dMatrix, nrounds = 2000,
                     watchlist = wl, verbose = 2)

# Save the xgBoost model
save(t20_xgb, file = "./output/t20_xgb.RData")