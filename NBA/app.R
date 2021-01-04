source('global.R')
source('content/body_standings.R')
source('content/body_recent.R')
source('content/body_team_plots.R')
source('content/body_about.R')
source('content/body_schedule.R')


ui <- fluidPage(
  tags$head(includeHTML(("google-analytics.html"))),
  navbarPage('2020-2021 NBA Season Dashboard',
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
    paste0('Data Updated as of ', updated_date)
    
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
  
  output$transactions_table <- DT::renderDataTable(transactions)
  
  output$schedule_table <- DT::renderDataTable(schedule_main, rownames = FALSE,
                                               options = list(pageLength = 20))

  output$recent_date <- renderValueBox({
    valueBox(
      value = format(recent_Bans$Date, format = "%B %d, %Y"), "Most Recent Game Date", icon = icon("calendar"), color = "purple"
    )
  })
  
  output$recent_games_played <- renderValueBox({
    valueBox(
      value = recent_Bans$`Number of Games`, "Games Played", icon = icon("list"), color = "purple"
    )
  })
  
  output$recent_home_wins <- renderValueBox({
    valueBox(
      value = recent_Bans$`Count_Home Wins`, "Home Wins", icon = icon("music note list"), color = "red"
    )
  })
  
  output$recent_road_wins <- renderValueBox({
    valueBox(
      value = recent_Bans$`Count_Road Wins`, "Road Wins", icon = icon("music note list"), color = "red"
    )
  })

  output$top_15 <- DT::renderDataTable(datatable(top_15_yesterday, rownames = FALSE,
                                                 options = list(searching = FALSE, pageLength = 15, 
                                                                lengthChange = FALSE, info = FALSE, paging = FALSE)) %>%
                                         formatCurrency(6, currency = "$", interval = 3, mark = ",", digits = 0) %>%
                                         formatStyle(c(1:dim(top_15_yesterday)[2]), border = '1px solid #ddd'))
  
  output$recent_team_wins <- DT::renderDataTable(datatable(team_Wins_Yesterday2, rownames = FALSE,
                                                           options = list(searching = FALSE,
                                                                          pageLength = 15, 
                                                                          lengthChange = FALSE, info = FALSE,
                                                                          paging = FALSE)) %>%
                                                   formatStyle(c(1:dim(team_Wins_Yesterday2)[2]), border = '1px solid #ddd'))
  output$top20_plot_output <- renderPlotly({
      top20_plot(top_20pt_scorers) 
  })
  
  selected_team_injury <- reactive({
    injury_data %>%
      filter(FullName == input$select_team)
  })
  
  output$team_plot_output <- renderPlot({

    team_plot
  })
  
  
  selected_team_ppg <- reactive({
    team_ppg_scorers %>%
      filter(FullName == input$select_team)
  })

  output$team_ppg_plot_output <- renderPlotly({
    team_ppg_plot(selected_team_ppg()) 
  })
  
  
  selected_team <- reactive({
    team_mov %>%
      filter(FullName == input$select_team)
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
      filter(FullName == input$select_team)
  })
  
  output$team_mov_output <- renderPlotly({
    df <- selected_team()
    mov_plot(df)
  })
  
  output$injury_table <- DT::renderDataTable(selected_team_injury(), rownames = FALSE,
                                                 options = list(searching = FALSE,
                                                                pageLength = 15, 
                                                                lengthChange = FALSE, info = FALSE,
                                                                paging = FALSE))
  
  selected_team_bans <- reactive({
    team_wins %>%
      filter(FullName == input$select_team)
  })
  
  selected_team_last_season <- reactive({
    last_season_wins %>%
      filter(FullName == input$select_team)
  })
  
  output$regular_wins <- renderValueBox({
    regular_valuebox_function(selected_team_bans())
  })
  
  output$last_season_wins <- renderValueBox({
    last_season_valuebox_function(selected_team_last_season())
  })
  
  output$game_types_output <- renderPlotly({
    game_types_plot(game_types)
  })
  
  output$contract_value_output <- renderPlotly({
    value_plot(contract_df)
  })
  
  output$team_contract_value_output <- renderPlotly({
    team_contract_value_plot(team_contract_value)
  })
  
  output$bans_avg_pts <- renderValueBox({
    valueBox(
      value = main_bans$league_avg, HTML(paste0("League Average Points Scored <br> <br> ",
                                                         main_bans$pct_change, "% difference from Last Season")),
      icon = icon("caret-up"), color = "blue"
    )
  })
  
  output$bans_date <- renderValueBox({
    gp_valuebox_function(main_bans)
  })
  
  output$bans_homeroad <- renderValueBox({
    valueBox(
      value = main_bans$record, HTML(paste0("League Wide Home - Road Win Record <br> <br> ",  
                                                       main_bans$`Win Percentage Home`, "% - ", main_bans$`Win Percentage Road`,
                          '% Win Percentage Splits')),
      icon = icon("chart-bar"), color = "blue"
    )
  })
  
}



# Run the App
shinyApp(ui, server)

