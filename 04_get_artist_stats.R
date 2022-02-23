# clear workspace
rm(list = ls())

# load packages
library(tidyverse)
library(countrycode)

# import data
charts <- read_csv("data/spotify_charts.csv")
artists <- read_csv("data/artists.csv")
population <- read_csv("data/population-since-1800.csv") %>%
  filter(Year == max(Year)) %>%
  rename(iso3c = Code,
         population = `Population (historical estimates)`) %>%
  select(iso3c, population)

# prepare data to be merged
regions <- data.frame(region = unique(charts$region)) %>%
  filter(region != "Global") %>%
  mutate(iso3c = countrycode(region, "country.name", "iso3c"))

regions <- left_join(regions, population, by = "iso3c") %>%
  select(-iso3c)

# get aggregated stats for each artist in each region
artist_stats <- charts %>%
  filter(region != "Global") %>%
  left_join(artists, by = "id") %>%
  group_by(artist_id, main_artist, region) %>%
  summarise(artist_total_streams = sum(streams, na.rm = T),
            artist_chart_appearences = n(),
            artist_titles_in_charts = n_distinct(title),
            artist_days_in_charts = n_distinct(date),
            artist_top_position = min(position, na.rm = T),
            artist_avg_position = mean(position, na.rm = T)
            ) %>%
  left_join(regions, by = "region") %>%
  mutate(artist_streams_per_capita = (artist_total_streams / population)) %>%
  select(-population)

# write to .csv-file
write_csv(artist_stats, file = "data/artist_stats.csv")

# get aggregated stats for each track in each region
artist_track_stats <- charts %>%
  filter(region != "Global") %>%
  left_join(artists, by = "id") %>%
  group_by(id, artist_id, title, main_artist, region) %>%
  summarise(artist_total_streams = sum(streams, na.rm = T),
            artist_chart_appearences = n(),
            artist_titles_in_charts = n_distinct(title),
            artist_days_in_charts = n_distinct(date),
            artist_top_position = min(position, na.rm = T),
            artist_avg_position = mean(position, na.rm = T)
  ) %>%
  left_join(regions, by = "region") %>%
  mutate(artist_streams_per_capita = (artist_total_streams / population)) %>%
  select(-population)

# write to .csv-file
write_csv(artist_track_stats, file = "data/artist_track_stats.csv")
