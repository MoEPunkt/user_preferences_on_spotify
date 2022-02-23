# setup -------------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(lme4)
library(countrycode)
library(modelr)
library(caret)
library(jtools)

set.seed(42)

charts <- read_csv("data/spotify_charts.csv")
features <- read_csv("data/track_features.csv")
dates <- read_csv("data/track_dates.csv")
hofstede <- read_csv("data/hofstede.csv")
artists <- read_csv("data/artists.csv")
artist_stats <- read_csv("data/artist_stats.csv")
top_song_position <- read_csv("data/top_song_position.csv")


# data preparation --------------------------------------------------------

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
  filter(region != "Global") %>%
  select(c(-title, -artist)) %>%
  group_by(id, region) %>%
  summarise(total_streams = sum(streams, na.rm = T)) %>%
  pivot_wider(names_from = "region", values_from = "total_streams") %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  pivot_longer(cols = !id, names_to = "region", values_to = "total_streams") %>%
  left_join(features, by = "id") %>%
  left_join(artists[1:2], by = "id") %>%
  left_join(artist_stats, by = c("artist_id", "region")) %>%
  left_join(dates, by = c("id", "region")) %>%
  left_join(hofstede, by = "region") %>%
  left_join(top_song_position, by = c("region", "id")) %>%
  mutate(debut_month = as.factor(lubridate::month(min_date)),
         region = as.factor(region))

rm(list = setdiff(ls(), "data"))

rsq <- function(predict, actual) {
  rss <- sum((predict - actual) ^ 2)
  tss <- sum((actual - mean(actual)) ^ 2)
  rsq <- 1 - rss/tss
  return(rsq)
}

data <- select(data,
               c(-id, -artist_id, -main_artist, -artist_total_streams,
                 -min_date, -max_date))
data <- data[-1]
data <- mutate_at(data,
                  vars(artist_chart_appearences:artist_days_in_charts),
                  ~replace(., is.na(.), 0))
data <- mutate_at(data,
                  vars(artist_top_position:artist_avg_position),
                  ~replace(., is.na(.), 200))
data <- mutate_at(data,
                  vars(artist_streams_per_capita),
                  ~replace(., is.na(.), 0))
data <- mutate_at(data,
                  vars(n_days),
                  ~replace(., is.na(.), 0))

# streams -----------------------------------------------------------------

form_streams_lmer <- formula(
  total_streams
  ~ danceability
  + energy
  + key
  + loudness
  + mode
  + speechiness
  + acousticness
  + instrumentalness
  + liveness
  + valence
  + tempo
  + duration_ms
  + time_signature
  + artist_chart_appearences
  + artist_titles_in_charts
  + artist_days_in_charts
  + artist_top_position
  + artist_avg_position
  + pdi
  + idv
  + mas
  + uai
  + ltowvs
  + ivr
  + debut_month
  + (1 | region)
)

form_streams_lmer_int <- formula(
  total_streams
  ~ (
    danceability
    + energy
    + key
    + loudness
    + mode
    + speechiness
    + acousticness
    + instrumentalness
    + liveness
    + valence
    + tempo
  )
  * (pdi
     + idv
     + mas
     + uai
     + ltowvs
     + ivr)
  + duration_ms
  + time_signature
  + artist_chart_appearences
  + artist_titles_in_charts
  + artist_days_in_charts
  + artist_top_position
  + artist_avg_position
  + debut_month
  * (
    danceability
    + energy
    + key
    + loudness
    + mode
    + speechiness
    + acousticness
    + instrumentalness
    + liveness
    + valence
    + tempo
  ) +
    (1 | region)
)

form_streams_lm <- formula(
  total_streams
  ~ danceability
  + energy
  + key
  + loudness
  + mode
  + speechiness
  + acousticness
  + instrumentalness
  + liveness
  + valence
  + tempo
  + duration_ms
  + time_signature
  + artist_chart_appearences
  + artist_titles_in_charts
  + artist_days_in_charts
  + artist_top_position
  + artist_avg_position
  + pdi
  + idv
  + mas
  + uai
  + ltowvs
  + ivr
  + debut_month
)

form_streams_lm_int <- formula(
  total_streams
  ~ (
    danceability
    + energy
    + key
    + loudness
    + mode
    + speechiness
    + acousticness
    + instrumentalness
    + liveness
    + valence
    + tempo
  )
  * (pdi
     + idv
     + mas
     + uai
     + ltowvs
     + ivr)
  + duration_ms
  + time_signature
  + artist_chart_appearences
  + artist_titles_in_charts
  + artist_days_in_charts
  + artist_top_position
  + artist_avg_position
  + debut_month
  * (
    danceability
    + energy
    + key
    + loudness
    + mode
    + speechiness
    + acousticness
    + instrumentalness
    + liveness
    + valence
    + tempo
  )
)

# n_days ------------------------------------------------------------------

form_n_days_lmer <- formula(
  n_days
  ~ danceability
  + energy
  + key
  + loudness
  + mode
  + speechiness
  + acousticness
  + instrumentalness
  + liveness
  + valence
  + tempo
  + duration_ms
  + time_signature
  + artist_chart_appearences
  + artist_titles_in_charts
  # + artist_days_in_charts
  + artist_top_position
  + artist_avg_position
  + pdi
  + idv
  + mas
  + uai
  + ltowvs
  + ivr
  + debut_month
  + (1 | region)
)

form_n_days_lmer_int <- formula(
  n_days
  ~ (
    danceability
    + energy
    + key
    + loudness
    + mode
    + speechiness
    + acousticness
    + instrumentalness
    + liveness
    + valence
    + tempo
  )
  * (pdi
     + idv
     + mas
     + uai
     + ltowvs
     + ivr)
  + duration_ms
  + time_signature
  + artist_chart_appearences
  + artist_titles_in_charts
  + artist_top_position
  + artist_avg_position
  + debut_month
  * (
    danceability
    + energy
    + key
    + loudness
    + mode
    + speechiness
    + acousticness
    + instrumentalness
    + liveness
    + valence
    + tempo
  ) +
    (1 | region)
)

form_n_days_lm <- formula(
  n_days
  ~ danceability
  + energy
  + key
  + loudness
  + mode
  + speechiness
  + acousticness
  + instrumentalness
  + liveness
  + valence
  + tempo
  + duration_ms
  + time_signature
  + artist_chart_appearences
  + artist_titles_in_charts
  + artist_top_position
  + artist_avg_position
  + pdi
  + idv
  + mas
  + uai
  + ltowvs
  + ivr
  + debut_month
)

form_n_days_lm_int <- formula(
  n_days
  ~ (
    danceability
    + energy
    + key
    + loudness
    + mode
    + speechiness
    + acousticness
    + instrumentalness
    + liveness
    + valence
    + tempo
  )
  * (pdi
     + idv
     + mas
     + uai
     + ltowvs
     + ivr)
  + duration_ms
  + time_signature
  + artist_chart_appearences
  + artist_titles_in_charts
  + artist_top_position
  + artist_avg_position
  + debut_month
  * (
    danceability
    + energy
    + key
    + loudness
    + mode
    + speechiness
    + acousticness
    + instrumentalness
    + liveness
    + valence
    + tempo
  )
)

# hit ---------------------------------------------------------------------

data <- data %>%
  mutate(hit = ifelse(top_position < 20, 1, 0))

form_hit_glm <- formula(
  hit
  ~ danceability
  + energy
  + key
  + loudness
  + mode
  + speechiness
  + acousticness
  + instrumentalness
  + liveness
  + valence
  + tempo
  + duration_ms
  + time_signature
  + artist_chart_appearences
  + artist_titles_in_charts
  + artist_days_in_charts
  + pdi
  + idv
  + mas
  + uai
  + ltowvs
  + ivr
  + debut_month
)

form_hit_glm_int <- formula(
  hit
  ~ (
    danceability
    + energy
    + key
    + loudness
    + mode
    + speechiness
    + acousticness
    + instrumentalness
    + liveness
    + valence
    + tempo
  )
  * (pdi
     + idv
     + mas
     + uai
     + ltowvs
     + ivr)
  + duration_ms
  + time_signature
  + artist_chart_appearences
  + artist_titles_in_charts
  + artist_days_in_charts
  + debut_month
  * (
    danceability
    + energy
    + key
    + loudness
    + mode
    + speechiness
    + acousticness
    + instrumentalness
    + liveness
    + valence
    + tempo
  )
)

# output ------------------------------------------------------------------

# define function that centers, scales and plots coefficients
coef_plot <- function(model) {
  model_summ <- summ(model,
       scale = TRUE,
       confint = TRUE)
  
  model_coefs <- data.frame(model_summ$coeftable)
  model_coefs <- cbind(term = rownames(model_coefs), model_coefs)
  rownames(model_coefs) <- 1:nrow(model_coefs)
  model_coefs <- filter(model_coefs, term != "(Intercept)")
  
  p <- ggplot(data = model_coefs, aes(x = Est., y = reorder(term, -Est.))) +
    geom_point(color = "red") +
    geom_pointrange(aes(xmin = X2.5., xmax = X97.5.), color = "#65D46E") +
    geom_vline(xintercept = 0, color = "white", size = 1) +
    scale_x_continuous(sec.axis = dup_axis()) +
    labs(y = "Coefficient",
         x = "Estimate (mean-centered and scaled by 1 s.d.)") +
    theme(plot.background = element_rect(fill = "#222222"),
          axis.title = element_text(colour = "#FFFFFF"),
          axis.text.x = element_text(color = "#FFFFFF"),
          axis.text.y = element_text(color = "#FFFFFF"),
          panel.grid.major.x = element_line(size = 0.25, linetype = 'solid',
                                            colour = "white"),
          panel.grid.minor.x = element_line(size = 0.1, linetype = 'solid',
                                            colour = "white"),
          panel.grid.major.y = element_blank(),
          panel.background = element_rect(fill = "grey20",
                                          color = NA,
                                          size = 0.5, linetype = "solid")
    )
  return(p)
}

# create models for total streams
m_streams_lm <- lm(form_streams_lm, data)
m_streams_lm_int <- lm(form_streams_lm_int, data)
m_streams_lmer <- lmer(form_streams_lmer, data)
m_streams_lmer_int <- lmer(form_streams_lmer_int, data)

# create models for number of days
m_n_days_lm <- lm(form_n_days_lm, data)
m_n_days_lm_int <- lm(form_n_days_lm_int, data)
m_n_days_lmer <- lmer(form_n_days_lmer, data)
m_n_days_lmer_int <- lmer(form_n_days_lmer_int, data)

# create logistics models for hit dummy variable
m_hit_glm <- glm(formula = form_hit_glm, data = data, family = binomial(link = "logit"))
m_hit_glm_int <- glm(formula = form_hit_glm_int, data = data, family = binomial(link = "logit"))

# create coefficient plot using the previously defined function and save as graphic file
coef_plot(m_streams_lm)
ggsave(filename = paste0("output/m_streams_lm_coef_plot.png"),
       width = 2500,
       height = 1500,
       units = "px",
       limitsize = FALSE)

coef_plot(m_streams_lmer)
ggsave(filename = paste0("output/m_streams_lmer_coef_plot.png"),
       width = 2500,
       height = 1500,
       units = "px",
       limitsize = FALSE)

coef_plot(m_streams_lm_int)
ggsave(filename = paste0("output/m_streams_lm_int_coef_plot.png"),
       width = 2500,
       height = 10000,
       units = "px",
       limitsize = FALSE)

coef_plot(m_streams_lmer_int)
ggsave(filename = paste0("output/m_streams_lmer_int_coef_plot.png"),
       width = 2500,
       height = 10000,
       units = "px",
       limitsize = FALSE)

coef_plot(m_n_days_lm)
ggsave(filename = paste0("output/m_n_days_lm_coef_plot.png"),
       width = 2500,
       height = 1500,
       units = "px",
       limitsize = FALSE)

coef_plot(m_n_days_lmer)
ggsave(filename = paste0("output/m_n_days_lmer_coef_plot.png"),
       width = 2500,
       height = 1500,
       units = "px",
       limitsize = FALSE)

coef_plot(m_n_days_lm_int)
ggsave(filename = paste0("output/m_n_days_lm_int_coef_plot.png"),
       width = 2500,
       height = 10000,
       units = "px",
       limitsize = FALSE)

coef_plot(m_n_days_lmer_int)
ggsave(filename = paste0("output/m_n_days_lmer_int_coef_plot.png"),
       width = 2500,
       height = 10000,
       units = "px",
       limitsize = FALSE)

coef_plot(m_hit_glm)
ggsave(filename = paste0("output/m_hit_glm_coef_plot.png"),
       width = 2500,
       height = 1500,
       units = "px",
       limitsize = FALSE)

coef_plot(m_hit_glm_int)
ggsave(filename = paste0("output/m_hit_glm_int_coef_plot.png"),
       width = 2500,
       height = 10000,
       units = "px",
       limitsize = FALSE)