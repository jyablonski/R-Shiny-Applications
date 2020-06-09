library(shiny)
library(shinydashboard)
library(genius)
library(lubridate)
library(tidyverse)
library(spotifyr)
library(extrafont)
library(plotly)

source('setToken.R')
access_token <- get_spotify_access_token()
dateVariable <- Sys.Date()
mdyDate <-  format(dateVariable, format = "%B %d, %Y")

# custom theme
theme_jacob <- function () { 
  theme_minimal(base_size=9, base_family="Gill Sans MT") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite')
    )
}

# data retrieval function
get_artist_data <- function(artist) {
  artist_Databb <- get_artist_audio_features(artist)
  artist_Databb <- artist_Databb %>%
    filter(!str_detect(album_name, 'Live In'),
           !str_detect(album_name, 'Concert'),
           !str_detect(album_name, 'Live'),
           !str_detect(album_name, 'Commentary'),
           !str_detect(album_name, 'Instrumentals'))
}

get_artist_song_count <- function(dataset){
  data1 <- dataset %>%
    distinct(track_name) %>%
    count(name = 'Songs')}

get_artist_album_count <- function(dataset){
  data2 <- dataset %>%
    distinct(album_name) %>%
    count(name = 'Albums')}

get_artist_statistics <- function(dataset){
  data3 <- dataset %>%
    select(valence, energy) %>%
    mutate(avg_valence = mean(valence * 100),
           avg_energy = mean(energy * 100),
           avg_valence = round(avg_valence, 2),
           avg_energy = round(avg_energy, 2)) %>%
    select(-valence, -energy) %>%
    rename(Positivity = avg_valence, Energy = avg_energy) %>%
    head(1) %>%
    cbind(artist_Songs) %>%
    cbind(artist_Albums)}

get_album_timeline <- function(dataset){
  data4 <- dataset %>%
    distinct(album_name, album_release_date) %>%
    arrange(album_release_date)
}

# plot function
# plot function
myPlot <- function(df) {
  artist_Albums <- df %>%
    distinct(album_name) %>%
    count()
  artist_Songs <- df %>%
    distinct(track_name) %>%
    count()
  
  p <- df %>%
    ggplot(aes(valence, energy, color = album_name, text = paste(track_name, '<br>', album_name, '<br>',
                                                                 'Positivity ', round(valence, 2), '<br>',
                                                                 'Energy ', round(energy, 2)))) +
    geom_point(size = 2, alpha = 0.8) +
    geom_hline(yintercept = 0.5) +
    geom_vline(xintercept = 0.5) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .1)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1)) +
    annotate('text', x = 0.95, y = 1, label = 'Happy / Joyful') +
    annotate('text', x = 0.95, y = 0, label = 'Mellow / Peaceful') +
    annotate('text', x = 0.01, y = 0, label = 'Sad / Slow') +
    annotate('text', x = 0.09, y = 1, label = 'Aggressive / Fast Paced') +
    labs(x = "Positivity",
         y = "Energy",
         color = 'Album',
         title = paste(df$artist_name, ' Song Distribution', sep = ""),
         subtitle = paste(artist_Songs$n, ' Songs and ', artist_Albums$n, ' Total Albums', sep = ""),
         caption = paste('Data collected via spotifyr Package on ', mdyDate, sep = "")) +
    theme_jacob() +
    theme(legend.position = 'top')
  ggplotly(p, tooltip = c('text')) %>%
    layout(title = list(text = paste0(paste(df$artist_name, ' Song Distribution', sep = ""))))
                                      '<br>',
                                      '<sup>',
                                      subtitle = paste(artist_Songs$n, ' Songs and ', artist_Albums$n, ' Total Albums', sep = ""),
                                      '</sup>')))
}

  
# Define UI ----
ui <- fluidPage(
  titlePanel("Spotify App"),
  
  sidebarLayout(
    sidebarPanel(textInput("user_artist", h3("Select an Artist"), 
                           value = "Enter text...")),
    actionButton("simulate", "Search")
    ),
  fluidRow(
    column(3, valueBoxOutput('songsBox')),
    column(3, valueBoxOutput('albumsBox')),
    column(3, valueBoxOutput('positivityBox')),
    column(3, valueBoxOutput('energyBox'))
  ),
  fluidRow(
    column(12, plotlyOutput("spotify_plot"))
  )
)


# Define server logic ----
server <- function(input, output) {
  observeEvent(input$simulate, {
    my_artist <- input$user_artist
    
    artist_Data <- get_artist_data(artist = my_artist)
    artist_Songs <- get_artist_song_count(dataset = artist_Data)
    artist_Albums <- get_artist_album_count(dataset = artist_Data)
    artist_Statistics <- get_artist_statistics(dataset = artist_Data)
    artist_Album_Timeline <- get_album_timeline(dataset = artist_Data)

    output$spotify_plot <- renderPlotly({
      my_artist <- input$user_artist
      myPlot(artist_Data)
    })
    
    output$songsBox <- renderValueBox({
      valueBox(
        artist_Statistics$Songs, "Unique Songs", icon = icon("credit-card"), color = "red"
      )
    })
    output$albumsBox <- renderValueBox({
      valueBox(
        artist_Statistics$Albums, "Studio Albums", icon = icon("credit-card"), color = "red"
      )
    })
    output$positivityBox <- renderValueBox({
      valueBox(
        artist_Statistics$Positivity, "Positivity", icon = icon("credit-card"), color = "blue"
      )
    })
    output$energyBox <- renderValueBox({
      valueBox(
        artist_Statistics$Energy, "Energy", icon = icon("credit-card"), color = "orange"
      )
    })

})
  

}

# Run the app ----
shinyApp(ui = ui, server = server)


# functioning as of 5:15 pm 6-8-20
# to do list : create averages for energy + positivity.


# button to remove commentary  / bonus tracks / live music (basically any duplicate songs)
