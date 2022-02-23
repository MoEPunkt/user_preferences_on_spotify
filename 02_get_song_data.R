# setup -------------------------------------------------------------------

# clear workspace
rm(list = ls())

# load packages
library(tidyverse)
library(spotifyr)
library(countrycode)

# set up credentials for Spotify API
# source secret credentials from external R script and create access token
source("spotify_credentials.R")
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
access_token <- get_spotify_access_token()

# set crawlrate (minimum time per request in seconds)
cr <- 0

# read charts data
data <- read_csv("data/spotify_charts.csv")

# get unique track ids from charts data
ids <- unique(na.omit(data$id))


# define function for Spotify API -----------------------------------------


spotify_api <- function(func, iterate_over, iteration_step, vars, outfile, cr = 0) {
  #' Collects data using Spotify's API
  #' 
  #' @param func The function to apply from the spotifyr package
  #' @param iterate_over The list of ids to iterate over
  #' @param iteration_step Number of ids contained in each request
  #' @param vars Variables of interest
  #' @param outfile The file the results are stored
  #' @param cr The crawlrate
  
  # create df
  df <- data.frame(matrix(ncol = length(vars), nrow = 0))
  colnames(df) <- vars
  write_csv(df, file = outfile)
  
  # iterate over defined list of ids using the defined iteration step
  i <- 1
  while (i + (iteration_step - 1) <= (length(iterate_over) - (length(iterate_over) %% iteration_step))) {
    # get system time to control crawlrate
    t1 <- Sys.time()
    # apply function of choice and extract variables of interest
    df <- func(iterate_over[i:(i + (iteration_step - 1))])[vars]
    # write to .csv-file
    write_csv(df, file = outfile, append = TRUE)
    # get system time again to compute time delta
    t2 <- Sys.time()
    # pause if scraping process was faster than defined crawlrate
    if (as.numeric(difftime(t2, t1, units = "secs")) < cr) {
      Sys.sleep(cr - as.numeric(difftime(t2, t1, units = "secs")))
    }
    t3 <- Sys.time()
    # print information on progress to console
    message(paste0("Processed: ", i, "/", length(iterate_over), " - Time: ",
                   round(t3 - t1, 2), " seconds."))
    i <- i + iteration_step
  }
  
  # repeat for last iteration smaller than the iteration step
  t1 <- Sys.time()
  
  df <- func(iterate_over[i:length(iterate_over)])[vars]
  write_csv(df, file = outfile, append = TRUE)
  
  t2 <- Sys.time()
  
  if (as.numeric(difftime(t2, t1, units = "secs")) < cr) {
    Sys.sleep(cr - as.numeric(difftime(t2, t1, units = "secs")))
  }
  
  t3 <- Sys.time()
  
  message(paste0("Processed: ", i, "/", length(iterate_over), " - Time: ",
                 round(t3 - t1, 2), " seconds."))
  }


# track features ----------------------------------------------------------

# get track audio features
spotify_api(func = get_track_audio_features,
            iterate_over = ids,
            iteration_step = 100,
            vars = c("danceability", "energy", "key", "loudness", "mode",
                     "speechiness", "acousticness", "instrumentalness",
                     "liveness", "valence", "tempo", "id", "duration_ms",
                     "time_signature"),
            outfile = "data/track_features.csv",
            cr)


# track info --------------------------------------------------------------

# get track info
spotify_api(func = get_tracks,
            iterate_over = ids,
            iteration_step = 50,
            vars = c("id", "popularity", "album.release_date", "explicit", "album.id"),
            outfile = "data/track_info.csv",
            cr)


# album info --------------------------------------------------------------

# get album info using album ids from the previously created track info file
track_info <- read_csv("data/track_info.csv")
album_ids <- unique(na.omit(track_info$album.id))

spotify_api(func = get_albums,
            iterate_over = album_ids,
            iteration_step = 20,
            vars = c("id", "popularity", "label"),
            outfile = "data/album_info.csv",
            cr)


# artist ids --------------------------------------------------------------

# create empty data.frame and write to .csv-file
artist_ids <- data.frame(matrix(ncol = 2 ,nrow = 0))
colnames(artist_ids) <- c("id", "artist_id")
write_csv(artist_ids, file = "data/artist_ids.csv")

# extract artist ids from track info using track ids
i <- 1
while (i + 49 <= (length(ids) - (length(ids) %% 50))) {
  t1 <- Sys.time()
  df <- NULL
  tracklist <- get_tracks(ids[i:(i + 49)])
  for (j in seq(1, nrow(tracklist))) {
    id <- tracklist[["id"]][j]
    tracklist_artists <- tracklist[["artists"]][[j]]
    tracklist_artists_ids <- tracklist_artists["id"]
    df <- rbind(df, data.frame(id, tracklist_artists_ids))
  }
  colnames(df) <- c("id", "artist_id")
  write_csv(df, file = "data/artist_ids.csv", append = T)
  t2 <- Sys.time()
  if (as.numeric(difftime(t2, t1, units = "secs")) < cr) {
    Sys.sleep(cr - as.numeric(difftime(t2, t1, units = "secs")))
  }
  t3 <- Sys.time()
  message(paste0("Processed: ", i, "/", length(ids), " - Time: ",
                 round(t3 - t1, 2), " seconds."))
  i <- i + 50
}
df <- NULL
tracklist <- get_tracks(ids[i:length(ids)])
for (j in seq(1, nrow(tracklist))) {
  id <- tracklist[["id"]][j]
  tracklist_artists <- tracklist[["artists"]][[j]]
  tracklist_artists_ids <- tracklist_artists["id"]
  df <- rbind(df, data.frame(id, tracklist_artists_ids))
}
colnames(df) <- c("id", "artist_id")
write_csv(df, file = "data/artist_ids.csv", append = T)
message(paste0("Processed: ", i, "/", length(ids)))


# artists info ------------------------------------------------------------

# get artist info on popularity and followers using previously obtained artist ids
artist_ids <- read_csv("data/artist_ids.csv")
artist_ids <- unique(na.omit(artist_ids$artist_id))

spotify_api(func = get_artists,
            iterate_over = artist_ids,
            iteration_step = 50,
            vars = c("id", "popularity", "followers.total"),
            outfile = "data/artist_info.csv",
            cr)

# assign most pppular artist to songs with more than one artist involved --

artist_ids <- read_csv("data/artist_ids.csv")
artist_info <- read_csv("data/artist_info.csv")
colnames(artist_info) <- c("artist_id", "artist_popularity", "artist_followers_total")

artist_info <- left_join(artist_ids, artist_info)

# create new df, add dummies indicating whether artist is most popular and has most followers among all artists for a given track
x <- artist_info %>%
  group_by(id) %>%
  mutate(max_pop = ifelse(artist_popularity == max(artist_popularity), 1, 0),
         max_followers = ifelse(artist_followers_total == max(artist_followers_total), 1, 0))

# remove less popular artists from the df, in case of a tied popularity measure max_followers serves as tie-breaker
x <- x[order(x[,"id"], -x[,"max_pop"], -x[,"max_followers"]),]
x <- x[!duplicated(x$id),]

# remove previously created dummies
x <- select(x, c(-max_pop, -max_followers))

# create main_artist column
x$main_artist <- NA

# assign artist names to main_artist column
i <- 1
while (i + 49 <= (nrow(x) - (nrow(x) %% 50))) {
  x$main_artist[i:(i + 49)] <- get_artists(ids = x$artist_id[i:(i + 49)])$name
  message(paste0(i, "/", nrow(x)))
  i <- i + 50
}
x$main_artist[i:nrow(x)] <- get_artists(ids = x$artist_id[i:nrow(x)])$name

write_csv(x, file = "data/artists.csv")

# get artists previous occurrences in charts ------------------------------

artists <- read_csv("data/artists.csv")

# count each artists chart occurences per date and region
# cumulatively sum them up
previous_chart_occurrences <- data %>%
  left_join(artists, by = "id") %>%
  group_by(region, artist_id, date) %>%
  summarise(artist_occurrences = n()) %>%
  mutate(artist_previous_occurrences = cumsum(artist_occurrences))

write_csv(previous_chart_occurrences, file = "data/previous_chart_occurrences.csv")

# get song's top position -------------------------------------------------

# get each song's top position for each region
top_song_position <- data %>%
  group_by(region, id) %>%
  summarise(top_position = min(position))

write_csv(top_song_position, file = "data/top_song_position.csv")


# track dates -------------------------------------------------------------

# get relevant dates for each track and region, e.g. entry date
track_dates <- data %>%
  group_by(region, id) %>%
  summarise(min_date = min(date),
            max_date = max(date),
            n_days = n())

write_csv(track_dates, file = "data/track_dates.csv")


# artist's tracks ---------------------------------------------------------

charts <- read_csv("data/spotify_charts.csv") %>%
  select(id, title) %>%
  distinct(id, title) %>%
  drop_na()

artists <- read_csv("data/artists.csv") %>%
  select(id, artist_id, main_artist) %>%
  drop_na()

artists_tracks <- left_join(charts, artists, by = "id")

write_csv(artists_tracks, file = "data/artist_tracks.csv")