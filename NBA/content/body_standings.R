body_standings <- dashboardBody(
  fluidRow(textOutput('today_date')),
  fluidRow(
    column(width = 6, "Western Conference", DT::dataTableOutput("west_standings_table")),
    column(width = 6, "Eastern Conference", DT::dataTableOutput("east_standings_table"))
  ),
  fluidRow(
      box(
        title = "20+ PPG Scorers",
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
      )),
  fluidRow(
    box(
      title = "Contract Value Analysis",
      status = "success",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 6,
      plotlyOutput("contract_plot_output", height = "600px")
      ),
    box(
      title = "Three Point Shooting Changes after the Season Reboot",
      status = "success",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 6,
      plotlyOutput("three_point_shooting_output", height = "600px")
    )

  )
)
