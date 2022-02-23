# Clear the workspace
rm(list = ls())

# Load the required packages
library(rpart)
library(Cubist)
library(hydroGOF)

# Source the file that generates the datasets
source("./data_pred.R")

# Factorize certain columns
data$key <- as.factor(data$key)
data$mode <- as.factor(data$mode)
data$time_signature <- as.factor(data$time_signature)
data$major <- as.factor(data$major)
data$explicit <- as.factor(data$explicit)
data$min_month <- as.factor(data$min_month)
data$duration <- as.factor(data$duration)

# Set the seed to 100
set.seed(100)
# Create a data partition on the dataset, based on the variable n_days
estIndex <- caret::createDataPartition(data$n_days, p = 0.75, times = 1, list = FALSE)
# Create the trainset and the testset
trainset<- data[estIndex,]
testset <- data[-estIndex,] 
# Remove the outcome variable and other unnecesary variables
train_temp <- trainset %>% select(c(-top20, -streams, -n_days))
test_temp<- testset %>% select(c(-top20, -streams, -n_days))

# Train a regression tree for the number of days
days_ct <- cubist(train_temp, trainset$n_days)
# Predict the model on the test data
days_ct_pred <- predict(days_ct, test_temp)
# Compute MAE, MSE, RMSE to assess the fit of the model
dct_mae <- mae(days_ct_pred, testset$n_days)
dct_mse <- mse(days_ct_pred, testset$n_days)
dct_rmse <- rmse(days_ct_pred, testset$n_days)

# Save the regression tree
save(days_ct, file = "./output/days_ct.RData")
