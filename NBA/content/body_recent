body_recent <- dashboardBody(
  fluidRow(
    valueBoxOutput("recent_date", width = 6),
    valueBoxOutput("recent_games_played", width = 6),
    ),
  fluidRow(
    column(width = 6, "Top 15 Performers by Points Scored", DT::dataTableOutput("top_15")),
    column(width = 6, "Team Stats", DT::dataTableOutput("recent_team_wins"))
  )
)

