body_standings <- dashboardBody(
  fluidRow(
    box(h4(paste0("Key Figures & Standings as of ", today)),
        fluidRow(
          column(
            valueBoxOutput("bans_homeroad", width = 4),
            valueBoxOutput("bans_avg_pts", width = 4),
            valueBoxOutput("bans_date", width = 4),
            width = 12,
            style = "margin-left: -20px"
          )
        ),
        div("Last updated: ", updated_date),
        width = 12)),
  fluidRow(
    column(width = 6, h4("Western Conference"), DT::dataTableOutput("west_standings_table")),
    column(width = 6, h4("Eastern Conference"), DT::dataTableOutput("east_standings_table"))
  ),
  fluidRow(),
  fluidRow(),
  fluidRow(
      box(
        title = "Player Scoring Efficiency",
        status = "success",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = 6,
        plotlyOutput("top20_plot_output", height = "600px")
        ),
      box(
        title = "Team Ratings",
        status = "success",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = 6,
        plotOutput("team_plot_output", height = "600px")
      )
  ),
  fluidRow(
    box(
      title = "Player Contract Value Analysis",
      status = "success",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 6,
      plotlyOutput("contract_value_output", height = "600px")
    ),
    box(
      title = "Team Contract Value Analysis",
      status = "success",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 6,
      plotlyOutput("team_contract_value_output", height = "600px")
    )
  )
)
