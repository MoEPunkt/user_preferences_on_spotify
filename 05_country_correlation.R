# clear workspace
rm(list = ls())

# load packages
library(tidyverse)
library(corrplot)
library(countrycode)

# import data
charts <- read_csv("data/spotify_charts.csv")
hofstede <- read_csv("data/hofstede.csv")


# correlation streams -----------------------------------------------------

# get max and min streams per country
streams <- charts %>%
  select(region, streams) %>%
  filter(region != "Global") %>%
  group_by(region) %>%
  summarise(max_streams = max(streams),
            min_streams = min(streams)) %>%
  mutate(region = countrycode::countrycode(region, "country.name", "iso3c"))

# get streams per country and standardize
streams_by_country <- charts %>%
  select(region, id, streams) %>%
  filter(region != "Global") %>%
  mutate(region = countrycode::countrycode(region, "country.name", "iso3c")) %>%
  left_join(streams, by = "region") %>%
  mutate(streams = ((streams - min_streams) / (max_streams - min_streams))) %>%
  select(c(-min_streams, -max_streams)) %>%
  group_by(region, id) %>%
  summarise(streams = mean(streams, na.rm = T)) %>%
  pivot_wider(names_from = "region", values_from = "streams") %>%
  select(-id)

# replace NAs with 0
streams_by_country[is.na(streams_by_country)] <- 0

# get corr matrix
corr_matrix_streams <- cor(streams_by_country, use = "pairwise.complete.obs")
# store as data frame
corr_matrix_streams <- as.data.frame(corr_matrix_streams)
# add rownames as column
corr_matrix_streams <- cbind(id = rownames(corr_matrix_streams), corr_matrix_streams)
rownames(corr_matrix_streams) <- 1:nrow(corr_matrix_streams)
# write csv
write_csv(corr_matrix_streams, "data/corr_matrix_streams.csv")


# dist matrix hofstede ----------------------------------------------------


dist_hofstede <- hofstede %>%
  inner_join(select(streams, region), by = "region") %>%
  drop_na()

# create empty matrix with dimensions (nrow(dist_hofstede),nrow(dist_hofstede))
m <- matrix(nrow = nrow(dist_hofstede),
            ncol = nrow(dist_hofstede))

# calculate euclidean distance for each country combination
for (i in seq(1, nrow(dist_hofstede))) {
  for (j in seq(1, nrow(dist_hofstede))) {
    sum_d = 0
    for (k in seq(2, ncol(dist_hofstede))) {
      d = (dist_hofstede[[k]][i] - dist_hofstede[[k]][j]) ^ 2
      sum_d = sum_d + d
    }
    m[i,j] = sqrt(sum_d)
  }
}

# set row and column names
rownames(m) <- dist_hofstede$region
colnames(m) <- dist_hofstede$region

# store as data frame
dist_hofstede <- as.data.frame(m)

# define standardize function
standardize <- function(x) {
  x <- x %>%
    select(where(is.numeric))
  abs((x - min(x)) / (max(x) - min(x)) - 1)
}

# standardize dist matrix
dist_hofstede <- standardize(dist_hofstede)

# add rownames as column
dist_hofstede <- cbind(id = rownames(dist_hofstede), dist_hofstede)
rownames(dist_hofstede) <- 1:nrow(dist_hofstede)
# write csv
write_csv(dist_hofstede, file = "data/dist_matrix_hofstede.csv")