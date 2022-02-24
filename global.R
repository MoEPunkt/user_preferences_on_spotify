# clear workspace ---------------------------------------------------------

rm(list = ls())

# load packages -----------------------------------------------------------

library(shiny)
library(tidyverse)
library(vroom)
library(plotly)
library(lubridate)
library(data.table)
library(countrycode)
library(leaflet)
library(leaflet.extras)
library(geojsonio)
library(spotifyr)
library(bslib)
library(randomForest)
library(formattable)
library(shinyWidgets)
library(tippy)
library(shinyBS)
library(bsplus)
library(xgboost)
library(C50)
library(Cubist)
library(caret)
library(BBmisc)
library(elasticnet)
library(caret)
library(hydroGOF)

# source files ------------------------------------------------------------

source("data/spotify_credentials.R")

# set up spotify access token ---------------------------------------------

Sys.setenv(SPOTIFY_CLIENT_ID = spotify_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = spotify_secret)
access_token <- get_spotify_access_token()

# set accent color --------------------------------------------------------

accent_color <- "#65D46E"

# import data -------------------------------------------------------------

corr_matrix_streams <- read_csv("data/corr_matrix_streams.csv")
# corr_matrix_hofstede <- read_csv("data/corr_matrix_hofstede.csv")
dist_matrix_hofstede <- read_csv("data/dist_matrix_hofstede.csv")

corr_map_regions <- as.character(read_csv("data/regions.csv")$region) %>%
  countrycode("country.name", "country.name")
regions <- as.character(read_csv("data/regions.csv")$region)

# corr_map_regions <- as.character(read_csv("./data/regions.csv")$region) %>%
#   countrycode("country.name", "country.name")

hofstede <- read_csv("data/hofstede.csv") %>%
  rename(id = region) %>%
  pivot_longer(cols = pdi:ivr, names_to = "cult_dim", values_to = "value") %>%
  mutate(cult_dim = recode(cult_dim,
                           idv = "Individualism",
                           pdi = "Power Distance",
                           mas = "Masculinity",
                           uai = "Uncertainty Avoidance",
                           ltowvs = "Long-term orientation",
                           ivr = "Indulgence")) %>%
  mutate(value = if_else(value > 100, 100, value))

hofstede_prediction <- read_csv("./data/hofstede.csv") %>% rename(id = region) %>%
  pivot_longer(cols = pdi:ivr, names_to = "cult_dim", values_to = "value")
hofstede_prediction$id <- countrycode(hofstede_prediction$country, 
                                      "country.name", "iso3c")

artist_tracks <- read_csv("data/artist_tracks.csv")
artist_stats <- read_csv("data/artist_stats.csv")
artist_track_stats <- read_csv("data/artist_track_stats.csv")
track_dates <- read_csv("data/track_dates.csv") %>% drop_na()

audio_features_over_time <- read_csv("data/audio_features_over_time.csv")
audio_features_by_country <- read_csv("data/audio_features_by_country.csv")
audio_features_by_country_month <- read_csv("data/audio_features_by_country_month.csv")

# Load the reference dataset (needed later to set factor levels)
data_ref <- fread("./data/dataset_fin.csv") %>%
  select(c(-top20, -streams, -n_days))

# Load the data with total_streams
data_streams <- fread("./data/dataset_streams.csv") %>%
  select(c(-top20, -total_streams, -n_days))

# Copy data_ref into a data table that will be factorized for later use
data_fa <- data_ref

data_fa$key <- as.factor(data_fa$key)
data_fa$mode <- as.factor(data_fa$mode)
data_fa$time_signature <- as.factor(data_fa$time_signature)
data_fa$major <- as.factor(data_fa$major)
data_fa$explicit <- as.factor(data_fa$explicit)
data_fa$min_month <- as.factor(data_fa$min_month)
data_fa$duration <- as.factor(data_fa$duration)

# Set up an empty data frame
initData <- data.frame(pdi = 0,
                       idv = 0,
                       mas = 0,
                       uai = 0,
                       ltowvs = 0,
                       ivr = 0,
                       danceability = 0,
                       energy = 0,
                       key = 0,
                       loudness = 0,
                       mode = 0,
                       speechiness = 0,
                       acousticness = 0,
                       instrumentalness = 0,
                       liveness = 0,
                       valence = 0,
                       tempo = 0,
                       time_signature = 0,
                       artist_occurrences = 0,
                       artist_previous_occurrences = 0,
                       major = 0,
                       explicit = 0,
                       min_month = 11,
                       duration = 4) %>% as.data.table()

# Add the empty data frame into the last line of data_ref as a placeholder for
# later values that need to be normalized for the prediction
data_ref <- rbind(data_ref, initData)
# Number of rows in data_ref
n <- nrow(data_ref)
# Add the empty data frame into the last line of data_streams as a placeholder 
# for later values that need to be normalized for the prediction
data_streams <- rbind(data_streams, initData)
# Number of rows in data_streams
m <- nrow(data_streams)

# Dataset to get the testset for the top20 and n_days prediction
data_test1 <- fread("./data/dataset_fin.csv")

# import models -----------------------------------------------------------

# Load randomForest for the Top20 prediction (models requires more factors)
get(load("/srv/shiny-server/dsp/models/rf_t20_newf2.RData"))

# Load the decisionTree for the Top20 prediction
get(load("/srv/shiny-server/dsp/models/dt_cost_t20_int.RData"))

# Load the xgBoost for the Top20 prediction
get(load("/srv/shiny-server/dsp/models/t20_xgb.RData"))

# Load the {Cubist} regression tree for the n_days prediction
get(load("/srv/shiny-server/dsp/models/days_ct.RData"))

# Load the elastic net for the n_days prediction
get(load("/srv/shiny-server/dsp/models/days_enet.RData"))

# Load the xgBoost for the n_days_prediction
get(load("/srv/shiny-server/dsp/models/m_xgb_n_days.RData"))

# Load the xgBoost for the streams prediction
get(load("/srv/shiny-server/dsp/models/m_xgb.RData"))

# Load the {Cubist} regressino tree for the streams prediction
get(load("/srv/shiny-server/dsp/models/streams_ct.RData"))

# Load the elastic net for the streams prediction
get(load("/srv/shiny-server/dsp/models/streams_enet.RData"))


# define functions --------------------------------------------------------

get_artist_image <- function(artist_id) {
  artist_image_url = get_artist(artist_id)$images$url[1]
  out = artist_image_url
  return(out)
}

# Function to turn some variables into factors
factorize <- function(x) {
  
  x$key <- as.factor(x$key)
  x$mode <- as.factor(x$mode)
  x$time_signature <- as.factor(x$time_signature)
  x$major <- as.factor(x$major)
  x$explicit <- as.factor(x$explicit)
  x$min_month <- as.factor(x$min_month)
  x$duration <- as.factor(x$duration)
  
  return(x)
}

# Function to normalize the independent variables of a dataset
normalize_data <- function(x) {
  
  x[, c("pdi", "idv", "mas", "uai", "ltowvs", "ivr", "danceability", "energy",
           "key", "loudness", "mode", "speechiness", "acousticness", 
           "instrumentalness", "liveness", "valence", "tempo", "time_signature", 
           "artist_occurrences", "artist_previous_occurrences", "major", 
           "explicit", "min_month", "duration") := 
         normalize(x[, c("pdi", "idv", "mas", "uai", "ltowvs", "ivr", "danceability", "energy",
                            "key", "loudness", "mode", "speechiness", "acousticness", 
                            "instrumentalness", "liveness", "valence", "tempo", "time_signature", 
                            "artist_occurrences", "artist_previous_occurrences", "major", 
                            "explicit", "min_month", "duration")], method = "range",
                   range = c(0,1))]
  
  return(x)
}

# Function to create an empty dataset with some independent variables as factors
createData_factorized <- function() {
  
  pred_data <- data.frame(pdi = numeric(),
                          idv = numeric(),
                          mas = numeric(),
                          uai = numeric(),
                          ltowvs = numeric(),
                          ivr = numeric(),
                          danceability = numeric(),
                          energy = numeric(),
                          key = factor(levels = levels(data_fa$key)),
                          loudness = numeric(),
                          mode = factor(levels = levels(data_fa$mode)),
                          speechiness = numeric(),
                          acousticness = numeric(),
                          instrumentalness = numeric(),
                          liveness = numeric(),
                          valence = numeric(),
                          tempo = numeric(),
                          time_signature = factor(levels =levels(data_fa$time_signature)),
                          artist_occurrences = numeric(),
                          artist_previous_occurrences = numeric(),
                          major = factor(levels = levels(data_fa$major)),
                          explicit = factor(levels = levels(data_fa$explicit)),
                          min_month = factor(levels = levels(data_fa$min_month)),
                          duration = factor(levels = levels(data_fa$duration)))
  
  return(pred_data)
}

# Function to create an empty datasets with only numeric independent variables
createData_numeric <- function() {
  
  pred_data <- data.frame(pdi = numeric(),
                          idv = numeric(),
                          mas = numeric(),
                          uai = numeric(),
                          ltowvs = numeric(),
                          ivr = numeric(),
                          danceability = numeric(),
                          energy = numeric(),
                          key = numeric(),
                          loudness = numeric(),
                          mode = numeric(),
                          speechiness = numeric(),
                          acousticness = numeric(),
                          instrumentalness = numeric(),
                          liveness = numeric(),
                          valence = numeric(),
                          tempo = numeric(),
                          time_signature = numeric(),
                          artist_occurrences = numeric(),
                          artist_previous_occurrences = numeric(),
                          major = numeric(),
                          explicit = numeric(),
                          min_month = numeric(),
                          duration = numeric())
  
  return(pred_data)
}

# Function to fill an empty dataset (y) with the input values (raw)
fillData <- function(raw, y) {
  y[1, "pdi"] <- raw[1, "pdi"]
  y[1, "idv"] <- raw[1, "idv"]
  y[1, "mas"] <- raw[1, "mas"]
  y[1, "uai"] <- raw[1, "uai"]
  y[1, "ltowvs"] <- raw[1, "ltowvs"]
  y[1, "ivr"] <- raw[1, "ivr"]
  y[1, "danceability"] <- raw[1, "danceability"]
  y[1, "energy"] <- raw[1, "energy"]
  y[1, "key"] <- raw[1, "key"]
  y[1, "loudness"] <- raw[1, "loudness"]
  y[1, "mode"] <- raw[1, "mode"]
  y[1, "speechiness"] <- raw[1, "speechiness"]
  y[1, "acousticness"] <- raw[1, "acousticness"]
  y[1, "instrumentalness"] <- raw[1, "instrumentalness"]
  y[1, "liveness"] <- raw[1, "liveness"]
  y[1, "valence"] <- raw[1, "valence"]
  y[1, "tempo"]<- raw[1, "tempo"]
  y[1, "time_signature"] <- raw[1, "time_signature"]
  y[1, "artist_occurrences"] <- raw[1, "artist_occurrences"]
  y[1, "artist_previous_occurrences"] <- raw[1, "artist_previous_occurrences"]
  y[1, "major"] <- raw[1, "major"]
  y[1, "explicit"] <- raw[1, "explicit"]
  y[1, "min_month"] <- raw[1, "min_month"]
  y[1, "duration"] <- raw[1, "duration"]
  
  return(y)
}

# Import and transform the dataset previous_chart_occurrences
previous_chart_occurrences <- read_csv("./data/previous_chart_occurrences.csv") %>%
   filter(date == max(date)) %>%
   mutate(region_id = countrycode(region, "country.name", "iso3c"))

# In data_test1, convert the variable top20 into numeric and then into a factor
data_test1[, top20 := as.factor(as.numeric(top20))]

# Set the seed to create a data partition to test the models for the top20 prediction
set.seed(100)
# Create a validationset index to test the top20 prediction
estIndex0 <- sample(1:3, size = nrow(data_test1), replace = TRUE,
                    p = c(0.7, 0.2, 0.1))
# Create the validationset
validationset <- data_test1[estIndex0 == 3,]
# Remove the dependent variable and variables that are not needed for the prediction
val_temp <- validationset %>% select(c(-top20, -n_days, -streams))
# Create a dMatrix with the top20-factor as a label and the remaining values of the validationset
dMatrixVal <- xgb.DMatrix(label = validationset$top20, data = as.matrix(val_temp))

# Prediction on the testset and computation of the confusion matrix for 
# the xgBoost prediction of the overall success
xgb_t20_pred <- predict(t20_xgb, dMatrixVal)
xgb_t20_pred <- xgb_t20_pred %>% round() %>% as.factor()
coma_xgb_t20 <- caret::confusionMatrix(xgb_t20_pred, validationset$top20, positive = "1")

# Factorize data_test1 in order to test the models
data_test1 <- data_test1 %>% factorize()

# Set the seed to create a data partition to test the models for the top20 prediction
set.seed(100)
# Create a testset index to test the top20 prediction
estIndex1 <- caret::createDataPartition(data_test1$top20, p = 0.75, times = 1, list = FALSE)
# Create the testset
testset_t20 <- data_test1[-estIndex1, ] 
# Remove the dependent variable and variables that are not needed for the prediction
test_temp_t20 <- testset_t20 %>% select(c(-top20, -n_days, -streams))

# Prediction on the testset and computation of the confusion matrix for 
# the randomForest prediction of the overall success
rf_t20_pred <- predict(rf_t20, test_temp_t20)
coma_rf_t20 <- caret::confusionMatrix(rf_t20_pred, testset_t20$top20, positive = "1")

# Prediction on the testset and computation of the confusion matrix for 
# the decision tree prediction of the overall success
dt_t20_pred <- predict(dt_cost_t20_int, test_temp_t20)
coma_dt_t20 <- caret::confusionMatrix(dt_t20_pred, testset_t20$top20, positive = "1")

# MAE, MSE, and RMSE for the xgBoost prediction of n_days have been calculated before
xgb_days_mae <- 2.31331
xgb_days_mse <- 362.98873
xgb_days_rmse <- 19.05226

# Insert the MAE, MSE, and RMSE into one data frame and rename the rows and columns
xgb_days_fit <- rbind(xgb_days_mae, xgb_days_mse, xgb_days_rmse) %>% as.data.frame()
rownames(xgb_days_fit) <- c("MAE", "MSE", "RMSE")
colnames(xgb_days_fit) <- "Value"

# MAE, MSE, and RMSE for the regression tree prediction of n_days have been calculated before
ct_days_mae <- 49.36192
ct_days_mse <- 14609.34767
ct_days_rmse <- 120.86913
# Insert the MAE, MSE, and RMSE into one data frame and rename the rows and columns
ct_days_fit <- rbind(ct_days_mae, ct_days_mse, ct_days_rmse) %>% as.data.frame()
rownames(ct_days_fit) <- c("MAE", "MSE", "RMSE")
colnames(ct_days_fit) <- "Value"

# MAE, MSE, and RMSE for the elastic net prediction of n_days have been calculated before
enet_days_mae <- 64.15068
enet_days_mse <- 13942.29270
enet_days_rmse <- 118.07749
# Insert the MAE, MSE, and RMSE into one data frame and rename the rows and columns
enet_days_fit <- rbind(enet_days_mae, enet_days_mse, enet_days_rmse) %>% as.data.frame()
rownames(enet_days_fit) <- c("MAE", "MSE", "RMSE")
colnames(enet_days_fit) <- "Value"

# MAE, MSE, and RMSE for the xgBoost prediction of total_streams has been calculated before
xgb_streams_mae <- 124223.92517
xgb_streams_mse <- 3524672940996.66
xgb_streams_rmse <- 1877411.23385

# Insert the MAE, MSE, and RMSE into one data frame and rename the rows and columns
xgb_streams_fit <- rbind(xgb_streams_mae, xgb_streams_mse, xgb_streams_rmse) %>% as.data.frame()
rownames(xgb_streams_fit) <- c("MAE", "MSE", "RMSE")
colnames(xgb_streams_fit) <- "Value"

# MAE, MSE, and RMSE for the regression tree prediction of total_streams have been calculated before
ct_streams_mae <- 357343.86907
ct_streams_mse <- 1210410118084.45
ct_streams_rmse <- 1100186.40152
# Insert the MAE, MSE, and RMSE into one data frame and rename the rows and columns
ct_streams_fit <- rbind(ct_streams_mae, ct_streams_mse, ct_streams_rmse) %>% as.data.frame()
rownames(ct_streams_fit) <- c("MAE", "MSE", "RMSE")
colnames(ct_streams_fit) <- "Value"

# MAE, MSE, and RMSE for the elastic net prediction of total_streams have been calculated before
enet_streams_mae <- 2221786.66174
enet_streams_mse <- 7212056012727.27
enet_streams_rmse <- 2685527.13871
# Insert the MAE, MSE, and RMSE into one data frame and rename the rows and columns
enet_streams_fit <- rbind(enet_streams_mae, enet_streams_mse, enet_streams_rmse) %>% as.data.frame()
rownames(enet_streams_fit) <- c("MAE", "MSE", "RMSE")
colnames(enet_streams_fit) <- "Value"