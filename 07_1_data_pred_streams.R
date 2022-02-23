# https://www.kdnuggets.com/2019/04/normalization-vs-standardization-quantitative-analysis.html/2
# https://sebastianraschka.com/Articles/2014_about_feature_scaling.html
# clear workspace
rm(list = ls())

# Load the required packages
if (!require("data.table")) install.packages("data.table"); library(data.table)
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("tidyr")) install.packages("tidyr"); library(tidyr)
if (!require("tidyverse")) install.packages("tidyverse"); library(tidyverse)
if (!require("countrycode")) install.packages("countrycode");library(countrycode)
if (!require("readr")) install.packages("readr");library(readr)

#setwd("/Users/moritzerdt/Documents/ds_project/final")

# Read in all the required data sets
charts <- fread("./data/spotify_charts.csv")
info <- fread("./data/track_info.csv")
features <- fread("./data/track_features.csv")
dates <- fread("./data/track_dates.csv")
hofstede <- fread("./data/hofstede.csv")
previous_chart_occurrences <- fread("./data/previous_chart_occurrences.csv")
artists <- fread("./data/artists.csv")
top_song_position <- fread("./data/top_song_position.csv")
album <- fread("./data/album_info.csv")
album <- distinct(album)

# Define major labels
major <- c("Universal", "Capitol", "Motown", "Geffen", "Lost Highway", 
           "Polydor", "Island Records", "Island", "Def Jam", "Virgin", 
           "Interscope Records", "Interscope", "Blue Note", "Warner", 
           "Atlantic", "Rhino", "Elektra", "Sire", "Reprise", "WEA", 
           "Roadrunner", "Chrysalis", "Parlophone", "Sony", "Columbia", "Epic", 
           "CBS", "Arista Records", "RCA", "Four Music", "Sony Classical", 
           "Deutsche Harmonia Mundi")

# Create a pattern out of the vector
pattern = paste(major, collapse="|")

album[, major := grepl(pattern, label)]

track_artist <- album[artists[info, on = .(id)], on = .(id = album.id)]
setnames(track_artist, c("id", "i.id", "popularity", "i.popularity"), c("album_id", "id", "album_popularity", "popularity"))

features <- features[is.na(id) == F]

charts2 <- charts[, c("title", "artist") := NULL][, .(total_streams = sum(streams, na.rm = T)), by = .(id, region)]

charts <- charts2[charts, on = .(id, region)]

data <- hofstede[top_song_position[
  features[
  dates[, "max_date" := NULL][
  previous_chart_occurrences[
  track_artist[
    charts, 
    on = .(id)],
  on = .(region, artist_id, date)],
  on = .(region, id)],
  on = .(id)],
  on = .(id, region)][, region_name := region][, region := countrycode(sourcevar = region_name,
                                                                 origin = "country.name",
                                                                 destination = "iso3c")][region_name != "Global"][, min_month := lubridate::month(min_date)],
  on = .(region)][, explicit := as.numeric(explicit)][, major := as.numeric(major)][duration_ms < 900000 & date == min_date,]

data[, duration := 
       ifelse(.SD < 120000, 1,
              ifelse(.SD >= 120000 & .SD < 180000, 2,
                     ifelse(.SD >= 180000 & .SD < 240000, 3,
                            ifelse(.SD>= 240000 & .SD < 300000, 4,
                                   ifelse(.SD >= 300000 & .SD < 360000, 5,
                                          ifelse(.SD >= 360000 & .SD < 600000, 6,
                                                 ifelse(.SD>= 600000 & .SD < 900000, 7,
                                                        ifelse(.SD >= 900000 & .SD < 1200000, 8,
                                                               ifelse(.SD>= 1200000 & .SD < 1800000, 9, 10))))))))),
     .SDcols = "duration_ms"][, top20 := ifelse(.SD <= 20, TRUE, FALSE), .SDcols = "top_position"]

data[, c("country", "min_date", "duration_ms", "top_position", "position",
     "artist_popularity", "artist_followers_total", "album.release_date",
     "region_name", "id", "artist_id", "date", "album_id", "album_popularity", 
     "label", "popularity", "region", "main_artist", "streams") := NULL]

data <- na.omit(data)

rm(list = setdiff(ls(), "data"))

#fwrite(data, "/Users/moritzerdt/Documents/ds_project/final/dataset_streams.csv")
#fread("/Users/moritzerdt/Documents/ds_project/final/dataset_streams.csv")

#data$top20 <- factor(data$top20, levels = c("0", "1"), labels = c("0", "1"))