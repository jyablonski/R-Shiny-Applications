body_team_plots <- dashboardBody(
  fluidRow(
    box(
      selectInput('select_team', h3("Select a Team"),
                  choices = team_choices,
  ))),
  fluidRow(
    box(
      title = "Team Margin of Victory Plot",
      status = "success",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 6,
      plotlyOutput("team_mov_output", height = "600px")
    ),
    column(width = 6, "Injury Table", DT::dataTableOutput("injury_table"))
  )
)

