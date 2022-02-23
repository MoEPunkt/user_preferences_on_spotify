# clear workspace
rm(list = ls())
# Import the required data sets
if (!require("caret")) install.packages("caret");library(caret)
if (!require("BBmisc")) install.packages("BBmisc");library(BBmisc)
if (!require("elasticnet")) install.packages("elasticnet");library(elasticnet)

# Source the file that creates the dataset
source("./data_pred.R")

# Normalize the independent variables to a range between 0 and 1
data[, c("pdi", "idv", "mas", "uai", "ltowvs", "ivr", "danceability", "energy",
         "key", "loudness", "mode", "speechiness", "acousticness", 
         "instrumentalness", "liveness", "valence", "tempo", "time_signature", 
         "artist_occurrences", "artist_previous_occurrences", "major", 
         "explicit", "min_month", "duration") := 
       normalize(data[, c("pdi", "idv", "mas", "uai", "ltowvs", "ivr", "danceability", "energy",
                          "key", "loudness", "mode", "speechiness", "acousticness", 
                          "instrumentalness", "liveness", "valence", "tempo", "time_signature", 
                          "artist_occurrences", "artist_previous_occurrences", "major", 
                          "explicit", "min_month", "duration")], method = "range",
                 range = c(0, 1))]

# Factorize certain variables
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
# Create a data partition of data, based on n_days
estIndex <- caret::createDataPartition(data$n_days, p = 0.75, times = 1, list = FALSE)
# Create the trainset and testset
trainset<- data[estIndex,]
testset <- data[-estIndex,] 
# Remove the unnecessary variables
train_temp <- trainset %>% select(c(-top20, -streams))
test_temp<- testset %>% select(c(-top20,-streams))

# Set up the control function
ctrl1 <- trainControl(method = "boot", number = 25, selectionFunction = "best")

# Set up the grid to improve the elastic net
grid_enet <- expand.grid(.lambda = c(0, .0001, .001, .01, 1),
                         .fraction = c(0, 0.1, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8,
                                       0.9, 1))

# Train the elaastic net on the trainset, with ctrl1 as the training control
# and grid_enet as the tuning grid
days_enet <- train(n_days ~., data = train_temp, method = "enet",
                   trControl = ctrl1, tuneGrid = grid_enet)
# Save the resulting model
save(days_enet, file = "./output_test/days_enet.RData")

