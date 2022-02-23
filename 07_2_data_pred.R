# clear workspace
rm(list = ls())

# Load the required packages
if (!require("data.table")) install.packages("data.table"); library(data.table)
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("tidyr")) install.packages("tidyr"); library(tidyr)
if (!require("tidyverse")) install.packages("tidyverse"); library(tidyverse)
if (!require("countrycode")) install.packages("countrycode");library(countrycode)
if (!require("readr")) install.packages("readr");library(readr)

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

# Create a pattern of major labels
pattern = paste(major, collapse="|")

# In the dataset album, create the variable major that captures whether an album
# has been releeased under a major label
album[, major := grepl(pattern, label)]

# Perform a left join  of album on artists
track_artist <- album[artists[info, on = .(id)], on = .(id = album.id)]
# Change the variable names in the new dataset track_artists
setnames(track_artist, c("id", "i.id", "popularity", "i.popularity"), c("album_id", "id", "album_popularity", "popularity"))

# Remove the features that have no value for the id
features <- features[is.na(id) == F]

# Merge all the datasets, set up the variable region that captures the 
# country code of a country, remove the global observations, create the variable
# min_month that captures the month of the first chart appearance of an 
# observation, create the variables explicit and major that capture whether
# song has explicit lyrics and whether it is realeased under a major label, and
# eventually, only consider songs that have a duration < 15 minutes and
# observations from the corresponding first appearance only
data <- hofstede[top_song_position[
  features[
  dates[, "max_date" := NULL][
  previous_chart_occurrences[
  track_artist[
    charts[, c("title", "artist") := NULL], 
    on = .(id)],
  on = .(region, artist_id, date)],
  on = .(region, id)],
  on = .(id)],
  on = .(id, region)][, region_name := region][, region := countrycode(sourcevar = region_name,
                                                                 origin = "country.name",
                                                                 destination = "iso3c")][region_name != "Global"][, min_month := lubridate::month(min_date)],
  on = .(region)][, explicit := as.numeric(explicit)][, major := as.numeric(major)][duration_ms < 900000 & date == min_date,]

# Create the variable duration that classifies the song length
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

# Remove unneccesary variables
data[, c("country", "min_date", "duration_ms", "top_position", "position",
     "artist_popularity", "artist_followers_total", "album.release_date",
     "region_name", "id", "artist_id", "date", "album_id", "album_popularity", 
     "label", "popularity", "region", "main_artist") := NULL]

# Remove observations that contain NA values
data <- na.omit(data)

# Remove all the datasets but data
rm(list = setdiff(ls(), "data"))