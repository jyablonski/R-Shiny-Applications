source('global.R')
# source('shot_loc_main.R')
source('content/body_standings.R')
source('content/body_recent.R')
source('content/body_team_plots.R')
source('content/body_about.R')
source('content/body_schedule.R')
# source('content/body_shot_location.R')


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
             # tabPanel("Shot Location App", dashboardPage(title = "Shot Location App",
             #                                    header = dashboardHeader(disable = TRUE),
             #                                    sidebar = dashboardSidebar(disable = TRUE),
             #                                    body = body_shot_location)),
             
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
      value = format(recent_Bans$Date, format = "%B %d, %Y"), "Most Recent Game Date", icon = icon("calendar"), color = "purple"
    )
  })
  
  output$recent_games_played <- renderValueBox({
    valueBox(
      value = recent_Bans$`Number of Games`, "Game Played", icon = icon("list"), color = "purple"
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
                                       
  output$recent_team_wins <- DT::renderDataTable(team_Wins_Yesterday2, rownames = FALSE,
                                                 options = list(searching = FALSE,
                                                                pageLength = 15, 
                                                                lengthChange = FALSE, info = FALSE,
                                                                paging = FALSE)) 
  output$top20_plot_output <- renderPlotly({
    if (input$type == 'Regular Season') {
      top20_plot(top_20pt_scorers) }
    else {
      top20_plot(top_20pt_scorers_p)
    }
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
  # output$schedule_output <- renderPlotly({
  #   
  #   ggplotly(cc, tooltip = c('text'))
  # })
  
  output$three_point_shooting_output <- renderPlotly({
    
    plot_bubble_shooting(threePointShooting_Bubble)
  })
  
  selected_team_differential <- reactive({
    playoffs_differentials %>%
      filter(Team == input$select_team)
  })
  
  output$differentials_output <- renderPlotly({
    plot_ppg_differentials(selected_team_differential())
  })
  
  selected_team_bans <- reactive({
    bubble_playoff_wins %>%
      filter(Team == input$select_team)
  })
  
  selected_team_bubble_bans <- reactive({
    bubble_regular_wins %>%
      filter(Team == input$select_team)
  })
  
  output$playoff_wins <- renderValueBox({
    playoffs_valuebox_function(selected_team_bans())
  })
  
  output$bubble_wins <- renderValueBox({
    bubble_valuebox_function(selected_team_bubble_bans())
  })
  
  # player_info <- reactive({
  #   req(input$player_search != '')
  #   available_players %>%
  #     filter(str_detect(display_first_last, regex(input$player_search, ignore_case = TRUE)))
  #   
  #   
  # })
  # 
  # observeEvent(input$player_search, {
  #   choices <- player_info()$display_first_last
  #   names(choices) <- choices
  #   
  #   updateSelectInput(session, 'select_player',
  #                     choices = choices,
  #                     selected = player_info()$name[1])
  #   
  # })
  # 
  # selected_player <- reactive({
  #   req(nrow(player_info()) > 0) # tells the server to wait until we actually have data in artist_info()
  #   player_info() %>% 
  #     filter(display_first_last == input$select_player)
  # })
  # 
  # observeEvent(input$select_player, {
  #   
  #   year_2 <- year_df %>%
  #     filter(bb >= selected_player()$from_year & bb <= selected_player()$to_year)
  #   
  #   ccc <- unique(year_2$year_choices)
  #   
  #   updateSelectInput(session, 'select_year',
  #                     choices = ccc,
  #                     selected = ccc[1])
  #   
  #   
  # })
  # 
  # observeEvent(input$generate_plot, {
  #   req(input$select_player != '')
  #   player_name <- input$select_player
  #   season_year <- input$select_year
  #   season_type_choice <- input$select_type
  #   player_code <- get_player_code(player_name)
  #   shot_data <- get_data(player_code, season_year, season_type_choice)
  #   
  #   output$hexagon_plot <- renderPlot({
  #     
  #     suppressWarnings(draw_hexagon(shot_data))
  #   })
  #   
  # })
}



# Run the App
shinyApp(ui, server)
