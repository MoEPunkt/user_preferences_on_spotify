shinyServer(function(input, output, session) {
  thematic::thematic_shiny()
  
  # create countries()-reactive to store geoJSON data from URL
  countries <- reactive({
    geojson_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json",
                 what = "sp")
  })


# start -------------------------------------------------------------------

  # counter for start page
  t <- Sys.time()
  output$live_stats <- renderText({
    invalidateLater(10, session)
    paste0('<h2 align="left">As you read this, there have been <span style="color:', accent_color,'">',
           comma(round(as.numeric(difftime(Sys.time(), t, units = "mins")) * (1.5 * 10^6)), digits = 0),
           '</span><br> songs streamed and <span style="color:', accent_color,'">',
           round(as.numeric(difftime(Sys.time(), t, units = "mins") * 28), digits = 0),
           '</span> new songs released on Spotify.</h2>')
  })
  

# countries ---------------------------------------------------------------
  
  # render leaflet map
  output$country_map <- renderLeaflet({
    
    # prepare map data for similarity measures
    if ((input$country_var == "Streaming similarity") || (input$country_var == "Cultural proximity")) {
      
    # select correlation matrix based on chosen variable of interest
    if (input$country_var == "Streaming similarity") {
      corr_matrix = corr_matrix_streams
    } else {
      corr_matrix = dist_matrix_hofstede
    }
    
    # create corr_data df with data of the selected region
    corr_data <- select(corr_matrix, id, countrycode(input$region, "country.name", "iso3c"))
    # rename cols
    colnames(corr_data) <- c("id", "value")
    # create countries dataframe from reactive
    countries <- countries()
    # rename country names
    countries$name <- countrycode(countries$name, "country.name", "country.name")
    # merge countries@data with corr_data
    countries@data <- left_join(countries@data, corr_data, by = "id")
    # create color palette
    pal <- colorBin(rev(RColorBrewer::brewer.pal(n = 5, name = "Greys")),
                    na.color = "black",
                    domain = countries$value)
    # create hover labels
    labels <- sprintf(
      "<strong>%s</strong><br/> Similarity with %s: %s",
      countrycode(countries@data$id, "iso3c", "country.name"),
      input$region,
      ifelse(
        is.na(countries@data$value) == T,
        "no data available",
        round(countries@data$value, 4)
      )
    ) %>% lapply(htmltools::HTML)
    
    # prepare map data for cultural dimensions
    } else if (input$country_var == "Cultural dimensions") {
      # load hofstede dataset
      hofstede <- filter(hofstede, cult_dim == input$cult_dim) %>%
        select(-cult_dim)
      # create countries dataframe from reactive
      countries <- countries()
      # rename country names
      countries$name <- countrycode(countries$name, "country.name", "country.name")
      # merge countries@data with hofstede
      countries@data <- left_join(countries@data, hofstede, by = "id")
      # create color palette
      pal <- colorBin(rev(RColorBrewer::brewer.pal(n = 5, name = "Greys")),
                      na.color = "black",
                      domain = countries$value)
      # create hover labels
      labels <- sprintf(
        "<strong>%s</strong><br/> Level of %s: %s",
        countrycode(countries@data$id, "iso3c", "country.name"),
        input$cult_dim,
        ifelse(is.na(countries$value) == T, "no data available", countries$value)
      ) %>% lapply(htmltools::HTML)
    
    # prepare map data for cultural dimensions  
    } else if (input$country_var == "Audio features") {
      
      # load audio features data and select feature chosen
      audio_features <- audio_features_by_country %>%
        select(region, input$audio_features) %>%
        mutate(region = countrycode(region, "country.name", "iso3c"))
      
      # rename cols
      colnames(audio_features) <- c("id", "value")
      # create countries dataframe from reactive
      countries <- countries()
      # rename country names
      countries$name <- countrycode(countries$name, "country.name", "country.name")
      # merge countries@data with audio_features
      countries@data <- left_join(countries@data, audio_features, by = "id")
      # create color palette
      pal <- colorBin(rev(RColorBrewer::brewer.pal(n = 5, name = "Greys")),
                      na.color = "black",
                      domain = countries$value)
      # create hover labels
      labels <- sprintf(
        "<strong>%s</strong><br/> %s: %s",
        countrycode(countries@data$id, "iso3c", "country.name"),
        str_to_title(input$audio_features),
        round(countries@data$value, 2)
      ) %>% lapply(htmltools::HTML)
      
    }
    # create leaflet map
    leaflet(countries) %>%
      setView(lat = 40, lng = 0, zoom = 1.5) %>%
      addTiles() %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      addPolygons(
        fillColor = ~pal(countries@data$value),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "white",
          fillColor = accent_color,
          dashArray = "",
          fillOpacity = 1,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        layerId = ~name
      ) %>%
      # add highlighted country
      addPolygons(stroke = TRUE,
                  weight = 2,
                  color = "white",
                  fillColor = accent_color,
                  fillOpacity = 1,
                  data = subset(countries, countries$name == input$region)) %>%
      addLegend(pal = pal, values = ~countries@data$value,
                opacity = 0.7, title = NULL,
                position = "bottomright") %>%
      addFullscreenControl()
    
  })
  
  # add selection on click
  observe({
    click <- input$country_map_shape_click
    updateSelectInput(session,
                      inputId = "region",
                      selected = click$id)
  })
  
  # add info text on cultural dimensions
  output$cult_dim_text <- renderText({
    paste0("<strong>Information on ", input$cult_dim,"</strong></br>",
           if (input$cult_dim == "Power Distance") {
             "Power Distance is the extent to which the less powerful members of organizations and institutions (like the family) accept and expect that power is distributed unequally."
           } else if (input$cult_dim == "Individualism") {
             "Individualism is the extent to which people feel independent, as opposed to being interdependent as members of larger wholes."
           } else if (input$cult_dim == "Masculinity") {
             "Masculinity is the extent to which the use of force in endorsed socially."
           } else if (input$cult_dim == "Uncertainty Avoidance") {
             "Uncertainty avoidance deals with a society’s tolerance for uncertainty and ambiguity."
           } else if (input$cult_dim == "Long-term orientation") {
             "Long-term orientation deals with change. In a long-time-oriented culture, the basic notion about the world is that it is in flux, and preparing for the future is always needed."
           } else if (input$cult_dim == "Indulgence") {
             "Indulgence is about the good things in life. In an indulgent culture it is good to be free. Doing what your impulses want you to do, is good."
           },
           '</br></br><em>Source:</em> <a href="https://geerthofstede.com">geerthofstede.com</a>')
  })
  
  # add info text on selected country variable
  output$country_var_text <- renderText({
    paste0(if (input$country_var == "Streaming similarity") {
             "A measure from 0 to 1 describing how similar two countries are in terms of their streaming preferences. A value of 0 is least similar and 1 is most similar."
           } else if (input$country_var == "Cultural proximity") {
             "A measure from 0 to 1 describing how similar two countries are in terms of their cultural dimensions. A value of 0 is least similar and 1 is most similar."
           } else if (input$country_var == "Cultural dimensions") {
             "Cultural dimensions following Hofstede's cultural dimensions theory."
           } else if (input$country_var == "Audio features") {
             "Spotify's average audio features for each country."
           })
  })
  
  # add info text on audio features
  output$audio_features_text <- renderText({
    paste0("<strong>Information on ", input$audio_features,"</strong></br>",
           if (input$audio_features == "danceability") {
             "Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable."
           } else if (input$audio_features == "energy") {
             "Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. For example, death metal has high energy, while a Bach prelude scores low on the scale. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy."
           } else if (input$audio_features == "valence") {
             "A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry)."
           } else if (input$audio_features == "loudness") {
             "The overall loudness of a track in decibels (dB). Loudness values are averaged across the entire track and are useful for comparing relative loudness of tracks. Loudness is the quality of a sound that is the primary psychological correlate of physical strength (amplitude). Values typically range between -60 and 0 db."
           } else if (input$audio_features == "speechiness") {
             "Speechiness detects the presence of spoken words in a track. The more exclusively speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value."
           } else if (input$audio_features == "acousticness") {
             "A confidence measure from 0.0 to 1.0 of whether the track is acoustic. 1.0 represents high confidence the track is acoustic."
           } else if (input$audio_features == "instrumentalness") {
             'Predicts whether a track contains no vocals. "Ooh" and "aah" sounds are treated as instrumental in this context. Rap or spoken word tracks are clearly "vocal". The closer the instrumentalness value is to 1.0, the greater likelihood the track contains no vocal content.'
           } else if (input$audio_features == "liveness") {
             "Detects the presence of an audience in the recording. Higher liveness values represent an increased probability that the track was performed live. A value above 0.8 provides strong likelihood that the track is live."
           } else if (input$audio_features == "tempo") {
             "The overall estimated tempo of a track in beats per minute (BPM). In musical terminology, tempo is the speed or pace of a given piece and derives directly from the average beat duration."
           } else if (input$audio_features == "duration_ms") {
             "The duration of the track in milliseconds."
           },
           '</br></br><em>Source:</em> <a href="https://developer.spotify.com/documentation/web-api/reference/#/operations/get-several-audio-features">spotify.com</a>')
  })


# artists -----------------------------------------------------------------

  # create selection choices for artists
  updateSelectizeInput(session,
                       "artist",
                       choices = sort(unique(artist_stats$main_artist)),
                       server = TRUE,
                       selected = "Ed Sheeran")
  
  # create artist_id()-reactive
  artist_id <- reactive(filter(artist_stats, main_artist == input$artist)$artist_id[1])
  
  # create selection choices for artist tracks
  observeEvent(input$artist, {
    updateSelectizeInput(session,
                         "track",
                         choices = c("- All tracks -", unique(artist_tracks[artist_tracks["main_artist"] == input$artist, ]$title)),
                         selected = NULL)
  })
  
  # get artist image src
  src_artist_image <- reactive({
    div(tags$img(src = paste0(get_artist_image(artist_id())), width = 280, height = 280))
  })
  
  # render artist image
  output$artist_image <- renderUI({
    src_artist_image()
  })
  
  # render artist leaflet map
  output$artist_map <- renderLeaflet({
    
    # get data for artist's whole discography, select artist and measure of success
    if (input$track == "- All tracks -") {
      artist_map_data <- artist_stats %>%
        filter(main_artist == input$artist) %>%
        select(region, input$artist_measure) %>%
        rename(value = input$artist_measure) %>%
        mutate(region = countrycode(region, "country.name", "iso3c"))
      
      # rename cols
      colnames(artist_map_data) <- c("id", "value")
      # create countries dataframe from reactive
      countries <- countries()
      # rename country names
      countries$name <- countrycode(countries$name, "country.name", "country.name")
      # merge counries@data with artist_map_data
      countries@data <- left_join(countries@data, artist_map_data, by = "id")
      
      # consider only selected track, select artist, track, measure of success
    } else {
      artist_map_data <- artist_track_stats %>%
        filter((main_artist == input$artist) & (title == input$track)) %>%
        select(region, input$artist_measure) %>%
        rename(value = input$artist_measure) %>%
        mutate(region = countrycode(region, "country.name", "iso3c")) %>%
        distinct(region, .keep_all = TRUE)
      # rename cols
      colnames(artist_map_data) <- c("id", "value")
      # create countries dataframe from reactive
      countries <- countries()
      # rename country names
      countries$name <- countrycode(countries$name, "country.name", "country.name")
      # merge counries@data with artist_map_data
      countries@data <- left_join(countries@data, artist_map_data, by = "id")
    }
    # create color palette and reverse for position variables
    if (input$artist_measure == "artist_top_position" | input$artist_measure == "artist_avg_position") {
      pal <- colorBin((RColorBrewer::brewer.pal(n = 5, name = "Greys")),
                      na.color = "black",
                      domain = countries$value)
    } else {
      pal <- colorBin(rev(RColorBrewer::brewer.pal(n = 5, name = "Greys")),
                      na.color = "black",
                      domain = countries$value)
    }
    # create label names
    label_names <- c("artist_total_streams" = "Total streams",
                     "artist_streams_per_capita" = "Streams per capita",
                     "artist_chart_appearences" = "Total chart appearances",
                     "artist_titles_in_charts" = "Number of titles in charts",
                     "artist_days_in_charts" = "Days in charts",
                     "artist_top_position" = "Top position",
                     "artist_avg_position" = "Mean position")
    # create hover labels
    labels <- sprintf(
      "<strong>%s</strong><br/> %s: %s",
      countrycode(countries@data$id, "iso3c", "country.name"),
      label_names[[input$artist_measure]],
      ifelse(is.na(countries@data$value) == T, "no data available", as.character(comma(round(countries@data$value, 2))))
    ) %>% lapply(htmltools::HTML)
    # create leaflet map
    leaflet(countries) %>%
      setView(lat = 40, lng = 0, zoom = 1.5) %>%
      addTiles() %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      addPolygons(
        fillColor = ~pal(countries@data$value),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "white",
          fillColor = accent_color,
          dashArray = "",
          fillOpacity = 1,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        layerId = ~name
      ) %>%
      addLegend(pal = pal, values = ~countries@data$value,
                opacity = 0.7, title = NULL,
                position = "bottomright") %>%
      addFullscreenControl()
  })

  
# prediction --------------------------------------------------------------

  
  # dynamic ui
  # Choices of artists for the selectInput "artist_prediction"
  updateSelectizeInput(session,
                       "artist_prediction",
                       choices = sort(unique(artist_tracks$main_artist)),
                       server = TRUE,
                       selected = "Ed Sheeran")
  
  # Dynamic/reactive choice set for the track, dependent on the selected artist
  observeEvent(input$artist_prediction, {
    tracks <- artist_tracks[artist_tracks$main_artist == input$artist_prediction, ]$title
    updateSelectizeInput(session,
                         "track_prediction",
                         choices = unique(tracks),
                         server = TRUE,
                         selected = NULL)
    output$empty_space <- renderText({
      if (input$artist_prediction == "Rick Astley") {
        '<iframe src="https://giphy.com/embed/Ju7l5y9osyymQ" width="800" height="600" frameBorder="0" class="giphy-embed" allowFullScreen></iframe><p><a href="https://giphy.com/gifs/rick-astley-Ju7l5y9osyymQ">via GIPHY</a></p>'
      } else {
        '</br>'
      }
    })
  })
  
  to_listen <- reactive({
    list(input$artist_prediction, input$track_prediction, input$region_prediction)
  })
  
  # Dynamic/reactive choice set, dependent on artist, track and region
  observeEvent(to_listen(), {
    updateNumericInput(session,
                       "artist_previous_occurrences",
                       value = max(0, filter(artist_stats, (main_artist == input$artist_prediction) &
                                               (region == input$region_prediction))$artist_chart_appearences)
                       )
    updateNumericInput(session,
                       "days_in_charts",
                       value = max(0, filter(artist_stats, (main_artist == input$artist_prediction) &
                                               (region == input$region_prediction))$artist_days_in_charts)
                       )
    updateNumericInput(session,
                       "titles_in_charts",
                       value = max(0, filter(artist_stats, (main_artist == input$artist_prediction) &
                                               (region == input$region_prediction))$artist_titles_in_charts)
                       )
    updateNumericInput(session,
                       "artist_top_position",
                       value = min(200, filter(artist_stats, (main_artist == input$artist_prediction) &
                                               (region == input$region_prediction))$artist_top_position)
                       )
    updateNumericInput(session,
                       "artist_avg_position",
                       value = round(min(200, filter(artist_stats, (main_artist == input$artist_prediction) &
                                                 (region == input$region_prediction))$artist_avg_position))
                       )
   updateSelectizeInput(session,
                        "min_month",
                        selected = max(1,lubridate::month(track_dates[(track_dates["id"] == artist_tracks[artist_tracks["title"] == input$track_prediction,]["id"][[1]][1]) & (track_dates["region_name"] == input$region_prediction), ]["min_date"][[1]]))
                        )
  })

  # Dynamic/reactive choice set, dependent on track
  observeEvent(input$track_prediction, {
    
    selected_track_id <- artist_tracks[(artist_tracks$main_artist == input$artist_prediction) & (artist_tracks$title == input$track_prediction),]$id[1]
    features <- get_track_audio_features(id = selected_track_id)
    
    updateSliderInput(inputId = "danceability",
                      value = features$danceability)
    updateSliderInput(inputId = "energy",
                      value = features$energy)
    updateSelectizeInput(inputId = "key",
                         selected = features$key)
    updateSliderInput(inputId = "loudness",
                      value = features$loudness)
    updateSelectizeInput(inputId = "mode",
                         selected = features$mode)
    updateSliderInput(inputId = "speechiness",
                      value = features$speechiness)
    updateSliderInput(inputId = "acousticness",
                      value = features$acousticness)
    updateSliderInput(inputId = "instrumentalness",
                      value = features$instrumentalness)
    updateSliderInput(inputId = "liveness",
                      value = features$liveness)
    updateSliderInput(inputId = "valence",
                      value = features$valence)
    updateSliderInput(inputId = "tempo",
                      value = features$tempo)
    updateSelectizeInput(inputId = "time_signature",
                         selected = features$time_signature)
    updateSliderInput(inputId = "duration",
                      value = (features$duration_ms / 60000))
    updateSelectizeInput(inputId = "explicit",
                         selected = factor(FALSE,
                                           levels = c(TRUE, FALSE),
                                           labels = c(1, 0)))
  })

  # Reactive choice of the models
  mod1 <- reactive(as.numeric(input$top20_model)) # Overall success
  mod2 <- reactive(as.numeric(input$days_model)) # Number of days
  mod3 <- reactive(as.numeric(input$streams_model)) # Number of streams
  
  # Reactive selection of variables for the prediciton
  p_danceability <- reactive(as.numeric(input$danceability))
  p_energy <- reactive(as.numeric(input$energy))
  p_key <- reactive(as.numeric(input$key))
  p_loudness <- reactive(as.numeric(input$loudness))
  p_mode <- reactive(as.numeric(input$mode))
  p_speechiness <- reactive(as.numeric(input$speechiness))
  p_acousticness <- reactive(as.numeric(input$acousticness))
  p_instrumentalness <- reactive(as.numeric(input$instrumentalness))
  p_liveness <- reactive(as.numeric(input$liveness))
  p_valence <- reactive(as.numeric(input$valence))
  p_tempo <- reactive(as.numeric(input$tempo))
  p_time_signature <- reactive(as.numeric(input$time_signature))
  p_min_month <- reactive(as.numeric(input$min_month))
  p_artist_id <- reactive(unique(filter(artist_stats, main_artist == input$artist_prediction)$artist_id))
  p_major <- reactive(as.numeric(input$major))
  p_explicit <- reactive(as.numeric(input$explicit))
  p_duration <- reactive(as.numeric(input$duration))
  p_region <- reactive(input$region_prediction)
  
  p_pdi <- reactive(filter(hofstede_prediction, id == countrycode(input$region_prediction, "country.name", "iso3c") &
                             cult_dim == "pdi")$value)
  p_idv <- reactive(filter(hofstede_prediction, id == countrycode(input$region_prediction, "country.name", "iso3c") &
                             cult_dim == "idv")$value)
  p_mas <- reactive(filter(hofstede_prediction, id == countrycode(input$region_prediction, "country.name", "iso3c") &
                             cult_dim == "mas")$value)
  p_uai <- reactive(filter(hofstede_prediction, id == countrycode(input$region_prediction, "country.name", "iso3c") &
                             cult_dim == "uai")$value)
  p_ltowvs <- reactive(filter(hofstede_prediction, id == countrycode(input$region_prediction, "country.name", "iso3c") &
                                cult_dim == "ltowvs")$value)
  p_ivr <- reactive(filter(hofstede_prediction, id == countrycode(input$region_prediction, "country.name", "iso3c") &
                             cult_dim == "ivr")$value)
  
  ao <- reactive(filter(previous_chart_occurrences, 
                        artist_id == p_artist_id() &
                          region_id == countrycode(input$region_prediction, 
                                                   "country.name", 
                                                   "iso3c"))$artist_occurrences)
  p_artist_occurrences <- reactive(ifelse(length(ao()) == 0, 0, ao()))                               
  
  
  apo <- reactive(filter(previous_chart_occurrences, 
                         artist_id == p_artist_id() &
                           region_id == countrycode(input$region_prediction, 
                                                    "country.name",
                                                    "iso3c"))$artist_previous_occurrences)
  
  # Reshape duration for the prediction
  duration <- reactive(ifelse(p_duration() < 2, 1,
                              ifelse(p_duration() >= 2 & p_duration() < 3, 2,
                                     ifelse(p_duration() >= 3 & p_duration() < 4, 3,
                                            ifelse(p_duration() >= 4 & p_duration() < 5, 4,
                                                   ifelse(p_duration() >= 5 & p_duration() < 7, 5,
                                                          ifelse(p_duration() >= 6 & p_duration() < 10, 6,
                                                                 ifelse(p_duration()>= 10 & p_duration() < 15, 7))))))))
  
  p_artist_previous_occurrences <- reactive(max(0,filter(artist_stats, (main_artist == input$artist_prediction) &
                                                           (region == input$region_prediction))$artist_chart_appearences))
  p_artist_chart_appearences <- reactive(p_artist_previous_occurrences())
  p_artist_titles_in_charts <- reactive(input$titles_in_charts)
  p_artist_days_in_charts <- reactive(input$days_in_charts)
  p_artist_top_position <- reactive(input$artist_top_position)
  p_artist_avg_position <- reactive(input$artist_avg_position)
  p_artist_streams_per_capita = reactive(max(0, filter(artist_stats, (main_artist == input$artist_prediction) &
                                                   (region == input$region_prediction))$artist_streams_per_capita))
  
  # infotext1 contains structural information about the model of choice
  output$infotext1 <- renderText({
    if(mod1() == 1){
      "Classification via XGBoost, with a learning rate of 0.4 and up to 25 nodes per tree."
    } else if(mod1() == 2) {
      "Classification via a randomForest with 500 trees that does not consider interaction between the input variables."
      } else if(mod1() == 3) {
      "Classification via a decision tree that considers interaction between the input variables, false negatives are 2 times more expensive than false positives."
    }
  })
  
  # modelinfo1 contains the confusion matrix for each model for testdata
  output$modelinfo1 <- renderPrint({
    if(mod1() == 1) { # XGBoost
      print(coma_xgb_t20)
    } else if(mod1() == 2){ # RandomForest
      print(coma_rf_t20)
    } else if(mod1() == 3) { # Decision tree
      print(coma_dt_t20)
    }
  })
  
  # infotext2 contains structural information about the model of choice
  output$infotext2 <- renderText({
    if(mod2() == 1){
      "Regression via XGBoost with, a learning rate of 0.3 and up to 6 nodes per tree."
    } else if(mod2() == 2) {
      "Regression tree."
    } else if(mod2() == 3) {
      "Regression via an elastic net, with lambda = 0.0001 and fraction = 1."
    }
  })
  
  # modelinfo2 contains the confusion matrix for each model for testdata
  output$modelinfo2 <- renderPrint({
     if(mod2() == 1) { # XGBoost
        print(xgb_days_fit)
      } else if(mod2() == 2){ # {Cubist} regression tree
        print(ct_days_fit)
      } else if(mod2() == 3){ # Elastic net
        print(enet_days_fit)
      }
  })
  
  # infotext3 contains structural information about the model of choice
  output$infotext3 <- renderText({
    if(mod3() == 1){
      "Regression via XGBoost, with a learning rate of 0.1 and up to 8 nodes per tree."
    } else if(mod3() == 2) {
      "Regression tree."
    } else if(mod3() == 3) {
      "Regression via an elastic net, with lambda = 0 and fraction = 1."
    }
  })
  
  # modelinfo3 contains the confusion matrix for each model for testdata
  output$modelinfo3 <- renderPrint({
    if(mod3() == 1) { # XGBoost
      print(xgb_streams_fit)
    } else if(mod3() == 2){ # {Cubist} regression tree
      print(ct_streams_fit)
    } else if(mod3() == 3){ # Elastic net
      print(enet_streams_fit)
    }
  })
  
  # Dataframe that contains the selected values
  rawData <- eventReactive(input$startPred, {data.frame(pdi = p_pdi(),
               idv = p_idv(),
               mas = p_mas(),
               uai = p_uai(),
               ltowvs = p_ltowvs(),
               ivr = p_ivr(),
               danceability = p_danceability(),
               energy = p_energy(),
               key = p_key(),
               loudness = p_loudness(),
               mode = p_mode(),
               speechiness = p_speechiness(),
               acousticness = p_acousticness(),
               instrumentalness = p_instrumentalness(),
               liveness = p_liveness(),
               valence = p_valence(),
               tempo = p_tempo(),
               time_signature = p_time_signature(),
               artist_occurrences = p_artist_occurrences(),
               artist_previous_occurrences = p_artist_previous_occurrences(),
               major = p_major(),
               explicit = p_explicit(),
               min_month = p_min_month(),
               duration = duration())})
  
  # Return the outcome of the prediction of the overall success
  output$result_t20 <- renderPrint({
    if(mod1() == 1){ # XGBoost
      # Set top20 to zero in order to have a label for the dMatrix
      top20 <- 0
      # Create the dMatrix for XGBoost
      dMatrix <- xgb.DMatrix(label = top20, data = as.matrix(rawData()))
      # Predict the result
      p_result <- as.numeric(predict(t20_xgb, dMatrix))
      # Print the output dependent on the prediction result
      if(p_result >= 0.5) HTML("A song with these features will land in the Top 20.")
      else HTML("A song with these features will not land in the Top 20.")
    } else if(mod1() == 2) { # RandomForest
      # Create a empty data frame with factors, based on data_ref
      data_f <- createData_factorized()
      # Fill the empty data frame 
      pred_data <- fillData(rawData(), data_f)
      # Predict the overall success
      p_result <- predict(rf_t20, pred_data)
      # Print the output dependent on the prediction result
      if(p_result == "1") HTML("A song with these features will land in the Top 20.")
      else HTML("A song with these features will not land in the Top 20.")
    } else if(mod1() == 3) { # Decision tree
      data_f <- createData_factorized()
      pred_data <- fillData(rawData(), data_f)
      p_result <- predict(dt_cost_t20_int, pred_data) %>% as.numeric()
      if(p_result >= 0.5) HTML("A song with these features will land in the Top 20.")
      else HTML("A song with these features will not land in the Top 20.")
      #print(p_result)
    }
  })
  
  output$result_days <- renderPrint({
    
    if (mod2() == 1) { # XGBoost
      # Create a data frame and fill with the reactive input values
      m_xgb_n_days_input <- data.frame(
        danceability = p_danceability(),
        energy = p_energy(),
        key = p_key(),
        loudness = p_loudness(),
        mode = p_mode(),
        speechiness = p_speechiness(),
        acousticness = p_acousticness(),
        instrumentalness = p_instrumentalness(),
        liveness = p_liveness(),
        valence = p_valence(),
        tempo = p_tempo(),
        duration_ms = (p_duration() * 60000),
        time_signature = p_time_signature(),
        artist_chart_appearences = p_artist_chart_appearences(),
        artist_titles_in_charts = p_artist_titles_in_charts(),
        artist_days_in_charts = p_artist_days_in_charts(),
        artist_top_position = p_artist_top_position(),
        artist_avg_position = p_artist_avg_position(),
        artist_streams_per_capita = p_artist_streams_per_capita(),
        pdi = p_pdi(),
        idv = p_idv(),
        mas = p_mas(),
        uai = p_uai(),
        ltowvs = p_ltowvs(),
        ivr = p_ivr(),
        debut_month_4 = as.numeric(0),
        debut_month_3 = as.numeric(0),
        debut_month_1 = as.numeric(0),
        debut_month_6 = as.numeric(0),
        debut_month_5 = as.numeric(0),
        debut_month_2 = as.numeric(0),
        debut_month_8 = as.numeric(0),
        debut_month_12 = as.numeric(0),
        debut_month_11 = as.numeric(0),
        debut_month_9 = as.numeric(0),
        debut_month_10 = as.numeric(0),
        debut_month_7 = as.numeric(0),
        region_Australia = as.numeric(0),
        region_Austria = as.numeric(0),
        region_Belgium = as.numeric(0),
        region_Bolivia = as.numeric(0),
        region_Brazil = as.numeric(0),
        region_Bulgaria = as.numeric(0),
        region_Canada = as.numeric(0),
        region_Chile = as.numeric(0),
        region_Colombia = as.numeric(0),
        region_Costa_Rica = as.numeric(0),
        region_Czech_Republic = as.numeric(0),
        region_Denmark = as.numeric(0),
        region_Dominican_Republic = as.numeric(0),
        region_Ecuador = as.numeric(0),
        region_Egypt = as.numeric(0),
        region_El_Salvador = as.numeric(0),
        region_Estonia = as.numeric(0),
        region_Finland = as.numeric(0),
        region_France = as.numeric(0),
        region_Germany = as.numeric(0),
        region_Greece = as.numeric(0),
        region_Guatemala = as.numeric(0),
        region_Honduras = as.numeric(0),
        region_Hong_Kong = as.numeric(0),
        region_Hungary = as.numeric(0),
        region_Iceland = as.numeric(0),
        region_India = as.numeric(0),
        region_Indonesia = as.numeric(0),
        region_Ireland = as.numeric(0),
        region_Israel = as.numeric(0),
        region_Italy = as.numeric(0),
        region_Japan = as.numeric(0),
        region_Korea__Republic_of = as.numeric(0),
        region_Latvia = as.numeric(0),
        region_Lithuania = as.numeric(0),
        region_Luxembourg = as.numeric(0),
        region_Malaysia = as.numeric(0),
        region_Mexico = as.numeric(0),
        region_Morocco = as.numeric(0),
        region_Netherlands = as.numeric(0),
        region_New_Zealand = as.numeric(0),
        region_Nicaragua = as.numeric(0),
        region_Norway = as.numeric(0),
        region_Panama = as.numeric(0),
        region_Paraguay = as.numeric(0),
        region_Peru = as.numeric(0),
        region_Philippines = as.numeric(0),
        region_Poland = as.numeric(0),
        region_Portugal = as.numeric(0),
        region_Romania = as.numeric(0),
        region_Russian_Federation = as.numeric(0),
        region_Saudi_Arabia = as.numeric(0),
        region_Singapore = as.numeric(0),
        region_Slovakia = as.numeric(0),
        region_South_Africa = as.numeric(0),
        region_Spain = as.numeric(0),
        region_Sweden = as.numeric(0),
        region_Switzerland = as.numeric(0),
        region_Taiwan = as.numeric(0),
        region_Thailand = as.numeric(0),
        region_Turkey = as.numeric(0),
        region_Ukraine = as.numeric(0),
        region_United_Arab_Emirates = as.numeric(0),
        region_United_Kingdom = as.numeric(0),
        region_United_States = as.numeric(0),
        region_Uruguay = as.numeric(0),
        region_Viet_Nam = as.numeric(0)
      )
      # rename selected region to match colnames data frame
      r <- paste0("region_",p_region()) %>%
        gsub("\\s+", "_", .) %>%
        gsub(",", "_", .)
      # rename selected month to match colnames in data frame
      m <- paste0("debut_month_", p_min_month())
      # set dummy variables to 1 in the respective region and month columns
      if (r %in% colnames(m_xgb_n_days_input)) {
        m_xgb_n_days_input[r] = 1
      }
      if (m %in% colnames(m_xgb_n_days_input)) {
        m_xgb_n_days_input[m] = 1
      }
      # Transform the dataframe into a matrix
      m_xgb_n_days_input_mat <- as.matrix(m_xgb_n_days_input)
      # Use the matrix to predict the number of days
      m_xgb_n_days_res <- eventReactive(input$startPred,{
        max(0, as.numeric(predict(m_xgb_n_days, m_xgb_n_days_input_mat)))
      })
      # Print the output
      HTML(paste0("A song with these features is expected to remain ",
                  round(m_xgb_n_days_res()),
                  " days in the charts of ",
                  input$region_prediction, "."))
    } else if (mod2() == 2) { # {Cubist} regression tree
      
      # Create an empty data frame with factors, based on data_ref
      data_f <- createData_factorized()
      # Fill the empty data frame 
      pred_data <- fillData(rawData(), data_f)
      # Predict the number of days
      p_result <- predict(days_ct, pred_data)
      # Print the result
      HTML(paste0("A song with these features is expected to remain ",
                  comma(round(p_result), digits = 0),
                  " days in the charts of ",
                  input$region_prediction, "."))
    } else if (mod2() == 3) { # Elastic net
      
      # Transform rawData() into a data table
      data_f <- reactive(as.data.table(rawData()))
      # Insert the values from data_f() in the last line of data_f()
      data_ref[n, ] <- data_f()
      # Normalize data_ref
      data_norm <- normalize_data(data_ref)
      # Factorize data_norm
      data_norm <- factorize(data_norm)
      # Extract the normalized data from the last line
      pred_data <- data_norm[n, ]
      # Use pred_data as input data for the prediction for the number of days
      p_result <- predict(days_enet, pred_data) %>% as.numeric()
      # Print the result
      HTML(paste0("A song with these features is expected to remain ",
                  comma(round(p_result), digits = 0),
                  " days in the charts of ",
                  input$region_prediction, "."))
    }
    
  })

  output$result_streams <- renderPrint({
    if (mod3() == 1) { # XGBoost
      # Create a data frame and fill with the reactive input values
      m_xgb_input <- data.frame(
        total_streams = as.numeric(0),
        danceability = p_danceability(),
        energy = p_energy(),
        key = p_key(),
        loudness = p_loudness(),
        mode = p_mode(),
        speechiness = p_speechiness(),
        acousticness = p_acousticness(),
        instrumentalness = p_instrumentalness(),
        liveness = p_liveness(),
        valence = p_valence(),
        tempo = p_tempo(),
        duration_ms = (p_duration() * 60000),
        time_signature = p_time_signature(),
        artist_chart_appearences = p_artist_chart_appearences(),
        artist_titles_in_charts = p_artist_titles_in_charts(),
        artist_days_in_charts = p_artist_days_in_charts(),
        artist_top_position = p_artist_top_position(),
        artist_avg_position = p_artist_avg_position(),
        pdi = p_pdi(),
        idv = p_idv(),
        mas = p_mas(),
        uai = p_uai(),
        ltowvs = p_ltowvs(),
        ivr = p_ivr(),
        debut_month_4 = as.numeric(0),
        debut_month_3 = as.numeric(0),
        debut_month_1 = as.numeric(0),
        debut_month_6 = as.numeric(0),
        debut_month_5 = as.numeric(0),
        debut_month_2 = as.numeric(0),
        debut_month_8 = as.numeric(0),
        debut_month_12 = as.numeric(0),
        debut_month_11 = as.numeric(0),
        debut_month_9 = as.numeric(0),
        debut_month_10 = as.numeric(0),
        debut_month_7 = as.numeric(0),
        region_Australia = as.numeric(0),
        region_Austria = as.numeric(0),
        region_Belgium = as.numeric(0),
        region_Bolivia = as.numeric(0),
        region_Brazil = as.numeric(0),
        region_Bulgaria = as.numeric(0),
        region_Canada = as.numeric(0),
        region_Chile = as.numeric(0),
        region_Colombia = as.numeric(0),
        region_Costa_Rica = as.numeric(0),
        region_Czech_Republic = as.numeric(0),
        region_Denmark = as.numeric(0),
        region_Dominican_Republic = as.numeric(0),
        region_Ecuador = as.numeric(0),
        region_Egypt = as.numeric(0),
        region_El_Salvador = as.numeric(0),
        region_Estonia = as.numeric(0),
        region_Finland = as.numeric(0),
        region_France = as.numeric(0),
        region_Germany = as.numeric(0),
        region_Greece = as.numeric(0),
        region_Guatemala = as.numeric(0),
        region_Honduras = as.numeric(0),
        region_Hong_Kong = as.numeric(0),
        region_Hungary = as.numeric(0),
        region_Iceland = as.numeric(0),
        region_India = as.numeric(0),
        region_Indonesia = as.numeric(0),
        region_Ireland = as.numeric(0),
        region_Israel = as.numeric(0),
        region_Italy = as.numeric(0),
        region_Japan = as.numeric(0),
        region_Korea__Republic_of = as.numeric(0),
        region_Latvia = as.numeric(0),
        region_Lithuania = as.numeric(0),
        region_Luxembourg = as.numeric(0),
        region_Malaysia = as.numeric(0),
        region_Mexico = as.numeric(0),
        region_Morocco = as.numeric(0),
        region_Netherlands = as.numeric(0),
        region_New_Zealand = as.numeric(0),
        region_Nicaragua = as.numeric(0),
        region_Norway = as.numeric(0),
        region_Panama = as.numeric(0),
        region_Paraguay = as.numeric(0),
        region_Peru = as.numeric(0),
        region_Philippines = as.numeric(0),
        region_Poland = as.numeric(0),
        region_Portugal = as.numeric(0),
        region_Romania = as.numeric(0),
        region_Russian_Federation = as.numeric(0),
        region_Saudi_Arabia = as.numeric(0),
        region_Singapore = as.numeric(0),
        region_Slovakia = as.numeric(0),
        region_South_Africa = as.numeric(0),
        region_Spain = as.numeric(0),
        region_Sweden = as.numeric(0),
        region_Switzerland = as.numeric(0),
        region_Taiwan = as.numeric(0),
        region_Thailand = as.numeric(0),
        region_Turkey = as.numeric(0),
        region_Ukraine = as.numeric(0),
        region_United_Arab_Emirates = as.numeric(0),
        region_United_Kingdom = as.numeric(0),
        region_United_States = as.numeric(0),
        region_Uruguay = as.numeric(0),
        region_Viet_Nam = as.numeric(0)
      )
      # rename selected region to match colnames data frame
      r <- paste0("region_",p_region()) %>%
        gsub("\\s+", "_", .) %>%
        gsub(",", "_", .)
      # rename selected month to match colnames data frame
      m <- paste0("debut_month_", p_min_month())
      # set dummy variables to 1 in the respective region and month columns
      if (r %in% colnames(m_xgb_input)) {
        m_xgb_input[r] = 1
      }
      if (m %in% colnames(m_xgb_input)) {
        m_xgb_input[m] = 1
      }
      # Transform the input data into a dMatrix
      m_xgb_input_mat <- xgb.DMatrix(label = m_xgb_input[1], data = as.matrix(m_xgb_input[-1]))
      # Use the dMatrix for the prediction of the number of streams
      m_xgb_res <- eventReactive(input$startPred,{
        max(0, as.numeric(predict(m_xgb, m_xgb_input_mat)))
        })
      # Print the result
      HTML(paste0("A song with these features is expected to generate ",
                  comma(round(m_xgb_res()), digits = 0),
                   " total streams in ",
                   input$region_prediction, "."))
    } else if (mod3() == 2) { # {Cubist} regression tree
      # Create a empty data frame with factors, based on data_ref
      data_f <- createData_factorized()
      # Fill the empty data frame 
      pred_data <- fillData(rawData(), data_f)
      # Predict the number of streams
      p_result <- predict(streams_ct, pred_data)
      # Print the result
      HTML(paste0("A song with these features is expected to generate ",
                  comma(round(p_result), digits = 0),
                  " total streams in ",
                  input$region_prediction, "."))
    } else if (mod3() == 3) { # Elastic net
      
      # Transform rawData() into a data table
      data_s <- reactive(as.data.table(rawData()))
      # Insert the values of data_s() in the last line of data_streams
      data_streams[m, ] <- data_s()
      # Normalize data_streams
      data_norm <- normalize_data(data_streams)
      # Extract the normalized data from the last line
      pred_data <- data_norm[m, ]
      # Predict the number of streams
      p_result <- predict(streams_enet, pred_data) %>% as.numeric()
      # Print the result
      HTML(paste0("A song with these features is expected to generate ",
                  comma(round(p_result), digits = 0),
                  " total streams in ",
                  input$region_prediction, "."))
    }

  })
  
  
# drivers of success ------------------------------------------------------
  
  # render coefficient plot
  output$coef_plot <- renderText({
    # select outcome variable
    if (input$outcome_var == "Hit") {
      coef_plot <- "hit_glm"
    } else if (input$outcome_var == "Total streams") {
      coef_plot <- "streams_lm"
    } else if (input$outcome_var == "Days in charts") {
      coef_plot <- "n_days_lm"
    }
    # select interaction terms
    if (input$interactions == TRUE) {
      int <- "_int"
    } else (int <- NULL)
    # paste file patch to selected plot
    paste0('<left><img src="m_', coef_plot, int,'_coef_plot.png" width="900"></left>')
    
  })
  
  # create info text for selected model / selected outcome variable
  output$outcome_var_text <- renderText({
    if (input$outcome_var == "Hit") {
      "Logistic regression model aiming to identify significant drivers of whether a given track becomes a chart hit (top position of 20 or better)."
    } else if (input$outcome_var == "Total streams") {
      "Linear mixed-effects regression model aiming to identify significant drivers of total streams of a given track."
    } else if (input$outcome_var == "Days in charts") {
      "Linear mixed-effects regression model aiming to identify significant drivers of number of days a given trck remains in the charts."
    }
  })
  
})