# clear workspace
rm(list = ls())

# Install the randomForest package
if (!require("randomForest")) install.packages("randomForest"); library(randomForest)

# Source the file that creates the dataset
source("./data_pred.R")

# Factorize selected columns
data[, top20 := as.factor(as.numeric(top20))]
data$key <- as.factor(data$key)
data$mode <- as.factor(data$mode)
data$time_signature <- as.factor(data$time_signature)
data$major <- as.factor(data$major)
data$explicit <- as.factor(data$explicit)
data$min_month <- as.factor(data$min_month)
data$duration <- as.factor(data$duration)

# Set the seed to 100
set.seed(100)
# Create a data partition, based on the variable top20
estIndex <- caret::createDataPartition(data$top20, p = 0.75, times = 1, list = FALSE)
# Split the data into training and test data
trainset<- data[estIndex,]
testset <- data[-estIndex,] 
# Remove the outcome variable and unnecceary ones
train_temp <- trainset[, c("n_days", "streams") := NULL]
test_temp<- testset[, c("top20", "n_days", "streams") := NULL]

# RandomForest model
rf_t20 <- randomForest(top20 ~ ., 
                    data = train_temp,
                    na.action = na.omit, 
                    ntree = 500,
                    importance = T,
                    keep.forest = TRUE)

# Save the randomForest
save(rf_t20, file = "./output_test/rf_t20_newf2.RData")