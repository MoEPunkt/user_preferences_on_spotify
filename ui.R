shinyUI(fluidPage(
  theme = bs_theme(bg = "#222222",
                   fg = "#FFFFFF",
                   primary = accent_color,
                   secondary = "#FFFFFF",
                   success = accent_color,
                   # base_font = font_google("Roboto"),
                   heading_font = font_google("Rubik")),
  setSliderColor(color = rep(accent_color, 10), sliderId = seq(1,10)),
  
  # css style ---------------------------------------------------------------
  
  tags$style(paste0("
#controls {
  /* Appearance */
  background-color: black;
  padding: 10px 10px 10px 10px;
  cursor: move;
  /* Fade out while not hovering */
  opacity: 0.3;
  zoom: 1;
  transition: opacity 100ms 300ms;
  border-radius: 5px;
}
#controls:hover {
  /* Fade in while hovering */
  opacity: 0.95;
  transition-delay: 0;
}

#startPred {
background-color:", accent_color, ";
border-color:#222222;
color:#222222;
}
               ")),
  
  # #artist_previous_occurrences {
  # background-color:#222222;
  #   color:grey10
  # }
  
  # headerPanel("User preferences on Spotify"),
  br(),
  tabsetPanel(type = "pills",

              
# start -------------------------------------------------------------------

              tabPanel("Start", icon = icon("play"),
                       fluidPage(
                         fluidRow(
                           column(2),
                           column(8, align = "center",
                                  br(),
                                  h1("Understanding and predicting (international) user preferences on Spotify", align = "left"),
                                  br(),
                                  htmlOutput("live_stats"),
                                  br(),
                                  h4("How will they evolve? Will they remain quietly under the radar, will they be a local success, or will they become the next global music phenomenon, earning fame, fortune, and billions of streams? And what are the crucial factors on which that depends on in the first place?", align = "justify"),
                                  h4("To answer these questions, we collected and analyzed daily Spotify charts of the past five years from 68 countries across the globe.
                          The result is a dataset of more than 21 million observations that provides insight into the number of streams, chart position, and time spent in the charts, serving as a measure of the overall success of each of the more than 120,000 unique tracks created by one of more than 30,000 artists.", align = "justify"),
                                  h4("Building on this, we have created a set of interactive tools that aim to provide a tangible experience of various music trends and user preferences, illustrating and explaining them to the best of our ability.", align = "justify"),
                                  br(),
                                  div(h3(strong("Drivers of Success")), style = paste0("color:", accent_color), align = "left"),
                                  h4("By selecting one of different regression models you can identify the variables significantly affecting the three relevant outcome variables (total number of streams, top position in charts and the number of days in charts).
                          Explore how the addition of various interaction terms influences the parameter estimates.", align = "justify"),
                                  br(),
                                  div(h3(strong("Predict")), style = paste0("color:", accent_color), align = "left"),
                                  h4("At the very heart of our project lies our prediction tool. It allows predictions about the total number of streams, the time spent in the charts, and the top position for any given track.
                          For each of these three outcome variables, a selection of sophisticated modeling approaches is available. Furthermore, numerous track-, artist- or country-specific characteristics can be taken from real tracks or modified as desired.
                          Last Christmas in summer or a Beatles classic at 180 BPM - which is expected to be more successful?  There are no limits to creativity!", align = "justify"),
                                  br(),
                                  div(h3(strong("Explore Artists")), style = paste0("color:", accent_color), align = "left"),
                                  h4("Learn more about your favorite artists with our interactive artist world map. Discover where an artist is most popular, looking at their entire discography or selecting individual tracks. For that purpose, we provide several popularity measures to choose from.", align = "justify"),
                                  br(),
                                  div(h3(strong("Explore Countries")), style = paste0("color:", accent_color), align = "left"),
                                  h4("How similar are the countries in terms of their streaming preferences? And is there a connection with their general cultural proximity? Take a look at our interactive country-level world map to explore these and other questions.
                          There you can also learn more about Hofstede's theory of cultural dimensions, which is part of our forecasting models as an explanatory variable at country level.", align = "justify"),
                                  br(),
                                  div(h3(strong("General Remarks")), style = paste0("color:", accent_color), align = "left"),
                                  HTML(paste0('<h4 align="justify">This work has been developed as part of the Data Science Project of the Master\'s program Data Science in Business and Economics at the University of Tübingen.
                                     The entire code of the project as well as samples of the datasets used can be accessed <a href="https://github.com/MoEPunkt/user_preferences_on_spotify">here</a>.
                                     The complete datasets used in this project are available <a href="https://bwedu-my.sharepoint.com/:f:/g/personal/adrian_zarbock_bwedu_de/EhMnETiTQd1MrP0PIIgvXVwBkGJbecjv9fi04hRtUmP-Xg?e=eSgjxG">here</a>.
                                     An introductory video for this app can be found <a href=https://youtu.be/75lsmJHlhSo">here</a>.
                                     All content provided is for academic purposes only.</h4>')),
                                  br()
                           )
                         )
                       )
              ),

# drivers of success ------------------------------------------------------

              tabPanel("Drivers of Success", icon = icon("backward"),
                       fluidPage(
                       div(h3("Identifying the drivers of success", align = "left"), style = paste0("color:", accent_color)),
                       br(),
                       sidebarLayout(
                         sidebarPanel(style = "max-width: 400px",
                           selectInput(inputId = "outcome_var",
                                       label = em("Select outcome variable"),
                                       choices = c("Hit", "Total streams", "Days in charts")),
                           checkboxInput(inputId = "interactions",
                                         label = em("Include interaction terms")),
                           htmlOutput("outcome_var_text"),
                           ),
                         mainPanel(htmlOutput("coef_plot"))
                         )
                       )
                       ),

# prediction --------------------------------------------------------------
# Panel for the prediction
              tabPanel("Predict", icon = icon("forward"),
                       fluidPage(
                         div(h3("Predict the success of a song with selected features", align = "left"), style = paste0("color:", accent_color)),
                         br(),
                         sidebarLayout(
                           sidebarPanel(
                             div(h5("Select models", align = "left"), style = paste0("color:", accent_color)),
                             # selectInput to choose a model for the top20 prediction
                             selectInput("top20_model", em("Select a model to predict the overall success:"), 
                                         choices = c("XGBoost" = "1",
                                                     "RandomForest" = "2",
                                                     "Decision tree" = "3")),
                             # Information about the model
                             textOutput("infotext1"),
                             br(),
                             checkboxInput("show_modelinfo1",
                                           em("Show details"),
                                           value = FALSE),
                             conditionalPanel('input.show_modelinfo1',
                                              verbatimTextOutput("modelinfo1")),
                             br(),
                             
                             # selectInput to choose a model for the n_days prediction
                             selectInput("days_model", em("Select a model to predict how long a song will remain in the charts:"),
                                         choices = c("XGBoost" = "1",
                                                     "Regression tree" = "2",
                                                     "Elastic Net" = "3")),
                             # Information about the model
                             textOutput("infotext2"),
                             br(),
                             checkboxInput("show_modelinfo2",
                                           em("Show details"),
                                           value = FALSE),
                             conditionalPanel('input.show_modelinfo2',
                                              verbatimTextOutput("modelinfo2")),
                             br(),
                             # plotOutput("modelPlot"),
                             
                             # selectInput to choose a model for the streams prediction
                             selectInput("streams_model", em("Select a model to predict the total number of streams:"),
                                         choices = c("XGBoost" = "1",
                                                     "Regression tree" = "2",
                                                     "Elastic Net" = "3")),
                             # Information about the model
                             textOutput("infotext3"),
                             br(),
                             checkboxInput("show_modelinfo3",
                                           em("Show details"),
                                           value = FALSE),
                             conditionalPanel('input.show_modelinfo3',
                                              verbatimTextOutput("modelinfo3"))
                             ),
                           mainPanel(
                               fluidPage(
                                 br(),
                                 # Header and information 
                                 div(h5("Select an artist, the desired country and the song features", align = "left"), style = paste0("color:", accent_color)),
                                 # First row: selection of artist, song and country
                                 fluidRow(
                                   # selectInput to choose the artist
                                   column(4,
                                          selectInput("artist_prediction", "Artist", choices = NULL)
                                          ),
                                   # selectInput to choose the track
                                   column(4,
                                          selectInput("track_prediction", "Track", choices = NULL)
                                   ),
                                   # selectInput to choose the region
                                   column(4,
                                          selectInput("region_prediction", "Country",
                                                      choices = regions,
                                                      selected = "United States"))
                                 ),
                                 # Placeholder
                                 htmlOutput("empty_space", align = "center"),
                                 
                                 # Second row: altering the number of days the artist occurred in the charts (indepent of songs)
                                 fluidRow(
                                   
                                   # numericInput to select the number of days the artist occurred in the charts so far
                                   column(3,
                                          # numericInput("artist_previous_occurrences",
                                          #              "Previous chart appearances",
                                          #              value = 0)
                                          numericInput("days_in_charts",
                                                       "Days in Charts",
                                                       value = 0) %>%
                                            with_tippy(tooltip = "Total number of days that the artist was present in the charts of the selected country.")
                                   ),
                                   
                                   # numericInput to select the number of tracks the artist occurres with in the charts
                                   column(3,
                                          numericInput("titles_in_charts",
                                                       "Titles in Charts",
                                                       value = 0) %>%
                                            with_tippy(tooltip = "Number of unique titles that the artist placed in the charts of the selected country.")
                                   ),
                                   
                                   # numericInput to select the best position the artist reached so far
                                   column(3,
                                          numericInput("artist_top_position",
                                                       "Previous Top Position",
                                                       value = 200) %>%
                                            with_tippy(tooltip = "Top chart position of the artist in the selected country.")
                                   ),
                                   
                                   # numericInput to select the artists average position
                                   column(3,
                                          numericInput("artist_avg_position",
                                                       "Average Position",
                                                       value = 200) %>%
                                            with_tippy(tooltip = "Average chart position of the artist in the selected country.")
                                   )
                                 ),
                                 
                                 # Third row: altering the song features
                                 fluidRow(
                                   # selectInput to choose the release month
                                   column(3,
                                          selectInput("min_month", "Debut Month", 
                                                      choices = c("January" = "1",
                                                                  "February" = "2",
                                                                  "March" = "3",
                                                                  "April" = "4",
                                                                  "May" = "5",
                                                                  "June" = "6",
                                                                  "July" = "7",
                                                                  "August" = "8",
                                                                  "September" = "9",
                                                                  "October" = "10",
                                                                  "November" = "11",
                                                                  "December" = "12"),
                                                      selected = "1") %>%
                                            with_tippy(tooltip = "Month in which the title first appeared in the charts of the selected country.")
                                          ),
                                   
                                   
                                   # selectInput to choose whether the track is released with a major label or not
                                   column(3,
                                          selectInput("major", "Major Label", c("Yes" = "1",
                                                                                "No" = "0")) %>%
                                            with_tippy(tooltip = "Indicates if the artist is signed with a major record label.")
                                          ),
                                   
                                   # selectInput to choose whether the track contains explicit lyrics or not
                                   column(3,
                                          selectInput("explicit", "Explicit Lyrics", c("Yes" = "1", 
                                                                                       "No" = "0")) %>%
                                            with_tippy(tooltip = "Indicates whether the song lyrics are marked as explicit.")
                                          ),
                                   
                                   # selectInput to choose the key
                                   column(3,
                                          selectInput("key", "Key",
                                                      choices = c("C" = "0",
                                                                  "C#/Db" = "1",
                                                                  "D" = "2",
                                                                  "D#/Eb" = "3",
                                                                  "E/Fb" = "4",
                                                                  "E#/F" = "5",
                                                                  "F#/Gb" = "6",
                                                                  "G" = "7",
                                                                  "G#/Ab" = "8",
                                                                  "A" = "9",
                                                                  "A#/bb" = "10",
                                                                  "b" = "11")) %>%
                                            with_tippy(tooltip = "The key the track is in.")
                                          )
                                 ),
                                 
                                 # Fourth row: altering the song features
                                 fluidRow(
                                   
                                   # selectInput to choose the time signature
                                   column(3,
                                          selectInput("time_signature", "Time Signature",
                                                      choices = c("3/4" = "3",
                                                                  "4/4" = "4",
                                                                  "5/4" = "5",
                                                                  "6/4"= "6",
                                                                  "7/4" = "7")) %>%
                                            with_tippy(tooltip = "An estimated time signature. Indicates how many beats are in each bar.")
                                          ),
                                   
                                   # selectInput to choose the mode
                                   column(3,
                                          selectInput("mode", "Mode", choices = c("Major" = "1", 
                                                                                  "Minor" = "0")) %>%
                                            with_tippy(tooltip = "Mode indicates the modality (major or minor) of a track, the type of scale from which its melodic content is derived.")
                                          ),
                                   
                                   # sliderInput to choose the danceability
                                   column(3,
                                          sliderInput("danceability", "Danceability",
                                                      min = 0, max = 1, value = 0.5, step = 0.01) %>%
                                            with_tippy(tooltip = "Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable.")
                                          ),
                                   
                                   # sliderInput to choose the energy
                                   column(3,
                                          sliderInput("energy", "Energy",
                                                      min = 0, max = 1, value = 0.5, step = 0.01) %>%
                                            with_tippy(tooltip = "Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. For example, death metal has high energy, while a Bach prelude scores low on the scale. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy.")
                                          )
                                 ),
                                 
                                 # Fifth row: altering the song features
                                 fluidRow(
                                   
                                   # sliderInput to choose the loudness
                                   column(3,
                                          sliderInput("loudness", "Loudness in dB",
                                                      min = -60, max = 0, value = -30, step = 0.01) %>%
                                            with_tippy(tooltip = "The overall loudness of a track in decibels (dB). Loudness values are averaged across the entire track and are useful for comparing relative loudness of tracks. Loudness is the quality of a sound that is the primary psychological correlate of physical strength (amplitude). Values typically range between -60 and 0 db.")
                                          ),
                                   
                                   # sliderInput to choose the speechiness
                                   column(3,
                                          sliderInput("speechiness", "Speechiness", 
                                                      min = 0, max = 1, value = 0.5, step = 0.001) %>%
                                            with_tippy(tooltip = "Speechiness detects the presence of spoken words in a track. The more exclusively speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value.")
                                          ),
                                   
                                   # sliderInput to choose the acousticness
                                   column(3,
                                          sliderInput("acousticness", "Acousticness",
                                                      min = 0, max = 1, value = 0.5, step = 0.01) %>%
                                            with_tippy(tooltip = "A confidence measure from 0.0 to 1.0 of whether the track is acoustic. 1.0 represents high confidence the track is acoustic.")
                                          ),
                                   
                                   # sliderInput to choose the instrumentalness
                                   column(3,
                                          sliderInput("instrumentalness", "Instrumentalness",
                                                      min = 0, max = 1, value = 0.5, step = 0.01) %>%
                                            with_tippy(tooltip = 'Predicts whether a track contains no vocals. "Ooh" and "aah" sounds are treated as instrumental in this context. Rap or spoken word tracks are clearly "vocal". The closer the instrumentalness value is to 1.0, the greater likelihood the track contains no vocal content.')
                                          )
                                 ),
                                 
                                 # Sixth row: altering the song features
                                 fluidRow(
                                   
                                   # sliderInput to choose the liveness
                                   column(3,
                                          sliderInput("liveness", "Liveness",
                                                      min = 0, max = 1, value = 0, step = 0.001) %>%
                                            with_tippy(tooltip = "Detects the presence of an audience in the recording. Higher liveness values represent an increased probability that the track was performed live. A value above 0.8 provides strong likelihood that the track is live.")
                                          ),
                                   
                                   # sliderInput to choose the valence
                                   column(3,
                                          sliderInput("valence", "Valence",
                                                      min = 0, max = 1, value = 0.5, step = 0.01) %>%
                                            with_tippy(tooltip = "A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry).")
                                          ),
                                   
                                   # sliderInput to choose the tempo
                                   column(3,
                                          sliderInput("tempo", "Tempo", 
                                                      min = 1, max = 200, value = 120, step = 1) %>%
                                            with_tippy(tooltip = "The overall estimated tempo of a track in beats per minute (BPM). In musical terminology, tempo is the speed or pace of a given piece and derives directly from the average beat duration.")
                                          ),
                                   
                                   # sliderInput to choose duration
                                   column(3,
                                          sliderInput("duration", "Duration",
                                                      min = 0, max = 15, value = 3, step = 0.5) %>%
                                            with_tippy(tooltip = "The duration of the track in minutes.")
                                          )
                                 ),
                                 
                                 # Seventh row: start of prediction
                                 fluidRow(
                                   
                                   # actionButton to start the prediction
                                   column(12, align = "center",
                                          actionButton("startPred", "Start the prediction",
                                                       width = "35%")
                                          )
                                 ),
                                 br(),
                                 
                                 # Eigth row: prediction results: overall success
                                 fluidRow(
                                   
                                   # Result: Overall success / top20
                                   column(12,
                                          verbatimTextOutput("result_t20"))
                                 ),
                                 
                                 # Ninth row: prediction results: number of days the song remains in the top20 charts
                                 fluidRow(column(12,
                                                 verbatimTextOutput("result_days"))),
                                 
                                 # Tenth row: prediction results: number of streams
                                 fluidRow(
                                   column(12,
                                          verbatimTextOutput("result_streams"))
                                 )
                                 
                               )
                             )
                           )
                         )
                       ),

# artists -----------------------------------------------------------------

              tabPanel("Explore Artists", icon = icon("users"),
                       br(),
                       leafletOutput("artist_map", width = "auto", height = "700"),
                       absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                     draggable = TRUE, top = 135, left = 70, right = "auto", bottom = "auto",
                                     width = "300", height = "auto",
                                     selectInput(inputId = "artist", label = em("Select artist"),
                                                 choices = NULL),
                                     uiOutput("artist_image"),
                                     br(),
                                     selectInput(inputId = "track", label = em("Select track"),
                                                 choices = NULL),
                                     selectInput("artist_measure", em("Select measure of success"),
                                                  choices = c("Total streams" = "artist_total_streams",
                                                              "Streams per capita" = "artist_streams_per_capita",
                                                              "Total chart appearances" = "artist_chart_appearences",
                                                              "Number of titles in charts" = "artist_titles_in_charts",
                                                              "Days in charts" = "artist_days_in_charts",
                                                              "Top position" = "artist_top_position",
                                                              "Mean position" = "artist_avg_position")
                                                 )
                                     )
                       ),

# countries ---------------------------------------------------------------

              tabPanel("Explore Countries", icon = icon("globe-europe"),
                       br(),
                       leafletOutput("country_map", width = "auto", height = "700"),
                       absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                     draggable = TRUE, top = 135, left = 70, right = "auto", bottom = "auto",
                                     width = 300, height = "auto",
                                     selectInput(inputId = "country_var",
                                                  label = em("Select variable of interest"),
                                                  choices = c("Streaming similarity",
                                                              "Cultural proximity",
                                                              "Cultural dimensions",
                                                              "Audio features")
                                                 ),
                                     htmlOutput("country_var_text"),
                                     br(),
                                     conditionalPanel("input.country_var == 'Streaming similarity' || input.country_var == 'Cultural proximity'",
                                                      selectInput(inputId = "region",
                                                                  label = em("Select country"),
                                                                  choices = corr_map_regions, 
                                                                  selected = "United Kingdom")
                                                      ),
                                     conditionalPanel("input.country_var == 'Cultural dimensions'",
                                                      selectInput("cult_dim",
                                                                  em("Select cultural dimension"),
                                                                  choices = unique(hofstede$cult_dim)),
                                                      htmlOutput("cult_dim_text")
                                                      ),
                                     conditionalPanel("input.country_var == 'Audio features'",
                                                      selectInput("audio_features",
                                                                  em("Select audio feature"),
                                                                  choices = c("Danceability" = "danceability",
                                                                              "Energy" = "energy",
                                                                              "Valence" = "valence",
                                                                              "Loudness" = "loudness",
                                                                              "Speechiness" = "speechiness",
                                                                              "Acousticness" = "acousticness",
                                                                              "Instrumentalness" = "instrumentalness",
                                                                              "Liveness" = "liveness",
                                                                              "Tempo" = "tempo",
                                                                              "Duration" = "duration_ms")
                                                                  ),
                                                      htmlOutput("audio_features_text")
                                                      )
                                     )
                       )
)
)
)