# clear workspace
rm(list = ls())

# load packages
library(tidyverse)
library(rvest)
library(lubridate)

# set url
url <- "https://spotifycharts.com/regional"

# get all dates
dates <- read_html(url) %>%
  html_elements(css = "#content > div > div > div > span > div.wrapper > div > div > div > div:nth-child(3) > ul > li") %>%
  html_text() %>% mdy()

# check if data from previous collection exists
if (file.exists("data/spotify_charts.csv")) {
  # get dates since last data collection
  previous_dates <- read_csv("data/spotify_charts.csv") %>%
    select(date) %>%
    distinct()
} else {
  # else collect all available dates
  previous_dates$date <- 0
  }

# define set of dates to collect data on
dates <- sort(dates[dates > max(previous_dates$date)])

# get all regions
regions <- read_html(url) %>%
  html_elements(css = "#content > div > div > div > span > div.wrapper > div > div > div > div:nth-child(1) > ul > li") %>%
  as.character() %>%
  str_sub(17, 18)
regions[1] <- "global"

# set css paths
region_css <- '#content > div > div > div > span > div.wrapper > div > div > div > div:nth-child(1) > div'
date_css <- '#content > div > div > div > span > div.wrapper > div > div > div > div:nth-child(3) > div'
position_css <- '#content > div > div > div > span > table > tbody > tr > td.chart-table-position'
title_css <- '#content > div > div > div > span > table > tbody > tr > td.chart-table-track > strong'
artist_css <- '#content > div > div > div > span > table > tbody > tr > td.chart-table-track > span'
streams_css <- '#content > div > div > div > span > table > tbody > tr > td.chart-table-streams'
track_id_css <- '#content > div > div > div > span > table > tbody > tr > td.chart-table-image > a'

# define function to collect data from the previously set css paths and store in data.frame 
read_url <- function(url) {
  # define tryCatch function to handle errors and warnings (e.g. server errors) during the scraping process
  out <- tryCatch(
    {
      page <- read_html(url)
      
      position <- page %>%
        html_elements(css = position_css) %>%
        html_text()
      
      title <- page %>%
        html_elements(css = title_css) %>%
        html_text()
      
      artist <- page %>%
        html_elements(css = artist_css) %>%
        html_text() %>% str_sub(4)
      
      streams <- page %>%
        html_elements(css = streams_css) %>%
        html_text() %>%
        gsub(",", "",.) %>%
        as.numeric()
      
      date <- page %>%
        html_element(css = date_css) %>%
        mdy()
      
      region <- page %>%
        html_element(css = region_css) %>%
        html_text()
      
      track_id <- page %>%
        html_elements(css = track_id_css) %>%
        as.character() %>%
        str_sub(41, 62)
      
      data.frame(position, title, artist, streams, region, date, track_id)
    },
    error = function(cond) {
      message("Error message:")
      message(cond)
      return(NULL)
    },
    warning = function(cond) {
      message("Warning message:")
      message(cond)
      return(NULL)
    },
    finally = {
      message(paste0("Processed URL: ", url))
    }
  )
  return(out)
}

# create empty data.frame and set colnames
data <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(data) <- c("position", "title", "artist", "streams", "region", "date", "track_id")

# set crawlrate (minimum time per request, in seconds)
cr <- 0

# set crwaldelay (pause after each processed date, in seconds)
cd <- 0

# perform the scraping process iterating over regions and dates
i <- 1
for (r in regions) {
  for (d in dates) {
    # get system time to control crawlrate
    t1 <- Sys.time()
    # set specific url for region r and date d
    u <- paste0(url, "/", r, "/daily/", as_date(d))
    # call previously defined read_url() function
    data <- read_url(u)
    # check if created data.frame has content, if so write to .csv-file
    if (is.null(data) == FALSE) {
      write_csv(data,
                file = "~/Documents/R/Spotify Charts/spotify_charts.csv", append = TRUE)
    }
    # get system time again to compute time delta
    t2 <- Sys.time()
    # pause if scraping process was faster than defined crawlrate
    if (as.numeric(difftime(t2, t1, units = "secs")) < cr) {
      Sys.sleep(cr - as.numeric(difftime(t2, t1, units = "secs")))
    }
    t3 <- Sys.time()
    # print scraping progress to console
    message(paste0("Processed: ", floor((i / (length(regions) * length(dates))) * 100),
                   "% (", i, "/", (length(regions) * length(dates)), ")", " - Time: ",
                   round(t3 - t1, 2), " seconds."))
    i <- i + 1
  }
  # pause scraping process after each date for the defined crawldelay
  message(paste0("Process paused for ", cd, " seconds."))
  Sys.sleep(cd)
}
