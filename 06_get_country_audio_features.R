# load packages
library(tidyverse)

# import data
charts <- read_csv("data/spotify_charts.csv") %>%
  select(region, date, id, streams) %>%
  filter(region != "Global")

features <- read_csv("data/track_features.csv") %>%
  select(id, danceability:tempo, duration_ms) %>%
  filter(is.na(id) == F)

data <- left_join(charts, features, by = "id")

by_country <- data %>%
  group_by(region) %>%
  summarise(across(danceability:duration_ms, mean, na.rm = T))

write_csv(by_country, "data/audio_features_by_country.csv")