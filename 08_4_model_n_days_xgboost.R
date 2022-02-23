# clear workspace
rm(list = ls())

# load packages
library(tidyverse)
library(countrycode)
library(modelr)
library(caret)
library(xgboost)
library(fastDummies)

# set seed
set.seed(42)

# import data
charts <- read_csv("data/spotify_charts.csv")
features <- read_csv("data/track_features.csv")
dates <- read_csv("data/track_dates.csv")
hofstede <- read_csv("data/hofstede.csv")
artists <- read_csv("data/artists.csv")
artist_stats <- read_csv("data/artist_stats.csv")

# prepare data
features <- features %>%
  filter(is.na(id) == F)

regions <- data.frame(region = distinct(charts, region)) %>%
  filter(region != "Global") %>%
  mutate(iso3c = countrycode(region, "country.name", "iso3c"))

hofstede <- hofstede %>%
  rename(iso3c = region) %>%
  mutate(iso3c = countrycode(iso3c, "iso3c", "iso3c")) %>%
  left_join(regions, by = "iso3c") %>%
  select(-iso3c)

data <- charts %>%
  # remove observations from region "Global"
  filter(region != "Global") %>%
  # remove title and artist columns
  select(c(-title, -artist)) %>%
  group_by(id, region) %>%
  # count days in charts per track and regions
  summarise(n_days = n()) %>%
  # pivot wider over all regions
  pivot_wider(names_from = "region", values_from = "n_days") %>%
  # set NA to 0 to indicate 0 streams for tracks in regions with no chart appearance
  mutate_all(~replace(., is.na(.), 0)) %>%
  # pivot_longer: transform data back to its original form
  pivot_longer(cols = !id, names_to = "region", values_to = "n_days") %>%
  # merge with remaining datasets
  left_join(features, by = "id") %>%
  left_join(artists[1:2], by = "id") %>%
  left_join(artist_stats, by = c("artist_id", "region")) %>%
  left_join(dates[1:3], by = c("region", "id")) %>%
  left_join(hofstede, by = "region") %>%
  # add debut_month
  mutate(debut_month = as.factor(lubridate::month(min_date)),
         region = as.factor(region))

# clear memory by removing objects no longer needed
rm(list = setdiff(ls(), "data"))

# define function to compute rsquared
rsq <- function(predict, actual) {
  rss <- sum((predict - actual) ^ 2)
  tss <- sum((actual - mean(actual)) ^ 2)
  rsq <- 1 - rss/tss
  return(rsq)
}

# select variables
data <- select(data,
               c(-id, -artist_id, -main_artist, -artist_total_streams, -min_date))
data <- data[-1]

# mutate NAs
data <- mutate_at(data,
                  vars(artist_chart_appearences:artist_days_in_charts),
                  ~replace(., is.na(.), 0))
data <- mutate_at(data,
                  vars(artist_streams_per_capita),
                  ~replace(., is.na(.), 0))

# create dummy variables for debut_month and region
data <- dummy_cols(data, select_columns = "debut_month")
data <- dummy_cols(data, select_columns = "region", remove_first_dummy = T)

# edit colnames
names(data) <- gsub("\\s+", "_", names(data))
names(data) <- gsub(",", "_", names(data))

# drop original region and debut_mont columns
data <- select(data, -region, -debut_month, -debut_month_NA)

# mutates NAs
data <- mutate_at(data,
                  vars(contains("debut_month")),
                  ~replace(., is.na(.), 0))

# transform to matrix
data <- as.matrix(data)

# create data partition
train_index <- createDataPartition(data[,1],
                                   p = .75,
                                   times = 1,
                                   list = FALSE)

# create test and training datasets
train <- data[train_index, ]
test <- data[-train_index, ]

# specify and train XGBoost model
m_xgb_n_days <- xgboost(label = train[,1],
                 data = train[,-1],
                 nrounds = 250,
                 max_depth = 6,
                 eta = 0.3,
                 gamma = 0,
                 lambda = 1,
                 alpha = 0,
                 tree_method = "auto",
                 objective = "reg:squarederror",
                 eval_metric = "rmse",
                 booster = "gbtree"
)

# save XGBoost model as .RData-file
save(m_xgb_n_days, file = "output/m_xgb_n_days.RData")

# predict outcome on test data
p <- predict(m_xgb_n_days, test[,-1])

# compare actual with predicted outcomes
res <- data.frame(actual = test[,1],
                  predict = p)

# compute rsquared
rsq(res$predict, res$actual)

# plot actual vs. predicted outcomes
ggplot(res, aes(x = actual, y = predict)) +
  geom_point(alpha = .2) +
  geom_smooth(method = "lm") +
  geom_abline(intercept = 0, slope = 1, color = "red")