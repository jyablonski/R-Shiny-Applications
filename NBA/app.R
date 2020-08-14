source('global.R')
source('content/body_standings.R')
source('content/body_recent.R')
source('content/body_team_plots.R')
source('content/body_about.R')
source('content/body_schedule.R')


ui <- fluidPage(
  navbarPage('2019-2020 NBA Season Dashboard',
                 tabPanel("Overview", dashboardPage(title = "Overview",
                                                     header = dashboardHeader(disable = TRUE),
                                                     sidebar = dashboardSidebar(disable = TRUE),
                                                     body = body_standings)),
                 tabPanel("Most Recent Games", dashboardPage(title = "Most Recent Games",
                                                     header = dashboardHeader(disable = TRUE),
                                                     sidebar = dashboardSidebar(disable = TRUE),
                                                     body = body_recent)),
                 tabPanel("Team Plots", dashboardPage(title = "Team Plots",
                                                     header = dashboardHeader(disable = TRUE),
                                                     sidebar = dashboardSidebar(disable = TRUE),
                                                     body = body_team_plots)),
                 tabPanel("Schedule", dashboardPage(title = "Schedule",
                                                     header = dashboardHeader(disable = TRUE),
                                                     sidebar = dashboardSidebar(disable = TRUE),
                                                     body = body_schedule)),
                 tabPanel("About", dashboardPage(title = "About",
                                                     header = dashboardHeader(disable = TRUE),
                                                     sidebar = dashboardSidebar(disable = TRUE),
                                                     body = body_about))
                 )
                )
# Server Logic
server <- function(input, output, session) {
  
  output$today_date <- renderText({
    paste0('Data Updated as of ', today)
    
  })
  
  output$east_standings_table <- DT::renderDataTable(east_standings, rownames = FALSE, 
                                                     options = list(searching = FALSE,
                                                                    pageLength = 15,
                                                                    lengthChange = FALSE, info = FALSE,
                                                                    paging = FALSE))
  output$west_standings_table <- DT::renderDataTable(west_standings, rownames = FALSE,
                                                     options = list(searching = FALSE,
                                                                    pageLength = 15, 
                                                                    lengthChange = FALSE, info = FALSE,
                                                                    paging = FALSE))
  
  output$schedule_table <- DT::renderDataTable(schedule_main, rownames = FALSE,
                                               options = list(pageLength = 20))

  output$recent_date <- renderValueBox({
    valueBox(
      value = format(recent_Bans$Date, format = "%B %d, %Y"), "Most Recent Game Date", icon = icon("music note list"), color = "red"
    )
  })
  
  output$recent_games_played <- renderValueBox({
    valueBox(
      value = recent_Bans$`Number of Games`, "Games Played", icon = icon("music note list"), color = "red"
    )
  })
  
  output$recent_home_wins <- renderValueBox({
    valueBox(
      value = recent_Bans$`Home Wins`, "Home Wins", icon = icon("music note list"), color = "red"
    )
  })
  
  output$recent_road_wins <- renderValueBox({
    valueBox(
      value = recent_Bans$`Road Wins`, "Road Wins", icon = icon("music note list"), color = "red"
    )
  })

  output$top_15 <- DT::renderDataTable(datatable(top_15_yesterday, rownames = FALSE,
                                       options = list(searching = FALSE, pageLength = 15, 
                                                      lengthChange = FALSE, info = FALSE, paging = FALSE)) %>%
                                         formatCurrency(6, currency = "$", interval = 3, mark = ",", digits = 0))
                                       
  output$recent_team_wins <- DT::renderDataTable(team_Wins_Yesterday2, rownames = FALSE,
                                                 options = list(searching = FALSE,
                                                                pageLength = 15, 
                                                                lengthChange = FALSE, info = FALSE,
                                                                paging = FALSE)) 
  output$top20_plot_output <- renderPlotly({
    
     ggplotly(top20_plot, tooltip = c('text')) %>%
       layout(legend = list(orientation = "h", x = 0.35))
  })
  
  output$team_plot_output <- renderPlot({

    team_plot
  })
  
  
  selected_team <- reactive({
    team_mov %>%
      filter(Team == input$select_team)
  })
  
  selected_team_bubble <- reactive({
    gameLogs_bubble %>%
      filter(Team == input$select_team) %>%
      select(-Team) %>%
      rename(Team = FullName, `Win Percentage` = WinPercentage) %>%
      select(Team, Wins, Losses, `Active Injuries`, `Win Streak`, `Win Percentage`)
  })
  
  selected_team_injury <- reactive({
    injury_data %>%
      filter(Team == input$select_team)
  })
  
  output$team_mov_output <- renderPlotly({
    df <- selected_team()
    mov_plot(df)
  })
  
  output$contract_plot_output <- renderPlotly({
    value_plot(gameLogs_Two)
  })
  
  output$injury_table <- DT::renderDataTable(selected_team_injury(), rownames = FALSE,
                                                 options = list(searching = FALSE,
                                                                pageLength = 15, 
                                                                lengthChange = FALSE, info = FALSE,
                                                                paging = FALSE))
  output$schedule_output <- renderPlotly({
    
    ggplotly(cc, tooltip = c('text'))
  })
  
  output$three_point_shooting_output <- renderPlotly({
    
    plot_bubble_shooting(threePointShooting_Bubble)
  })
  
}



# Run the App
shinyApp(ui, server)
