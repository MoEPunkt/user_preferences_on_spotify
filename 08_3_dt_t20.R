# clear workspace
rm(list = ls())

# Import the required 
if (!require("caret")) install.packages("caret"); library(caret)
if (!require("C50")) install.packages("C50"); library(C50)

# Source the file that creates the dataset to train and test the model
source("./data_pred.R")

# Factorize certain columns
data[, top20 := as.factor(as.numeric(top20))]
data$key <- as.factor(data$key)
data$mode <- as.factor(data$mode)
data$time_signature <- as.factor(data$time_signature)
data$major <- as.factor(data$major)
data$explicit <- as.factor(data$explicit)
data$min_month <- as.factor(data$min_month)
data$duration <- as.factor(data$duration)

# Interaction term for the overall success
form <- formula(top20
                ~ artist_occurrences
                + artist_previous_occurrences
                + (danceability 
                   + energy 
                   + loudness 
                   + speechiness 
                   + acousticness 
                   + instrumentalness 
                   + liveness
                   + valence
                   + tempo
                   + key
                   + mode
                   + duration
                   + time_signature)
                * (pdi
                   + idv
                   + mas
                   + uai
                   + ltowvs
                   + ivr)
                + min_month
                * (danceability 
                   + energy 
                   + loudness 
                   + speechiness 
                   + acousticness 
                   + instrumentalness 
                   + liveness
                   + valence
                   + tempo
                   + key
                   + mode
                   + duration
                   + time_signature)
                + explicit
                + major)

# Set the seed to 100
set.seed(100)
# Create a data partition of the dataset, based on the variable top20
trainIndex <- caret::createDataPartition(data$top20, p = 0.75, times = 1, list = FALSE)
# Create a trainset and a testset
trainset <- data[trainIndex,]
testset <- data[-trainIndex,]
# Remove the outcome and other unnecessary variables
train_temp <- trainset %>% select(c(-streams, -n_days))
test_temp<- testset %>% select(c( -n_days, -top20, -streams))

# Create a list that contains the dimensions for the errorcost matrix
m_dim <- list(c("0", "1"), c("0", "1"))
# Name the dimensions of the errorcost matrix
names(m_dim) <- c("predicted", "actual")
# Create and fill the errorcost matrix, such that false negatives are penalized
# 2 times more expensive than false positives
errorcost <- matrix(c(0, 1, 2, 0), nrow = 2, dimnames = m_dim)

# Train a decision tree for classification, considering the errorcosts
dt_cost_t20_int <- C5.0(form, train_temp, costs = errorcost)
# Predict the model on test_temp
pred_dt_cost_t20_int <- predict(dt_cost_t20_int, test_temp)
# Compute a confusionMatrix
confusionMatrix(pred_dt_cost_t20_int, testset$top20, positive = "1")
# Save the decision tree
save(dt_cost_t20_int, file = "./output_test/dt_cost_t20_int.RData")