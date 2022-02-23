# Clear the workspace
rm(list = ls())

# Import the required packages
library(rpart)
library(Cubist)
library(hydroGOF)

# Source the file that creates the dataaset
source("./data_pred_streams.R")

# Factorize certain independent variables
data$key <- as.factor(data$key)
data$mode <- as.factor(data$mode)
data$time_signature <- as.factor(data$time_signature)
data$major <- as.factor(data$major)
data$explicit <- as.factor(data$explicit)
data$min_month <- as.factor(data$min_month)
data$duration <- as.factor(data$duration)

# Set the seed to 100
set.seed(100)
# Create a data partition of data, based on total_streams
estIndex <- caret::createDataPartition(data$total_streams, p = 0.75, times = 1, list = FALSE)
# Create the trainset and testset
trainset<- data[estIndex,]
testset <- data[-estIndex,] 
# Remove the outcome variable and unnecessary variables
train_temp <- trainset %>% select(c(-top20, -n_days, -total_streams))
test_temp<- testset %>% select(c(-top20, -n_days, -total_streams))

# Train the regression tree
streams_ct <- cubist(train_temp, trainset$total_streams)
#predict the regression tree, based on test_temp and convert the result to numeric
streams_ct_pred <- predict(streams_ct, test_temp) %>% as.numeric()
# Compute the MAE, MSE, and RMSE
streams_mae <- mae(streams_ct_pred, testset$total_streams)
streams_mse <- mse(streams_ct_pred, testset$total_streams)
streams_rmse <- rmse(streams_ct_pred, testset$total_streams)

save(streams_ct, file = "/Users/moritzerdt/Documents/ds_project/streams_ct.RData")