body_schedule <- dashboardBody(
  fluidRow(tags$a(href="https://www.espn.com/nba/schedule", "Click here for National TV Schedule")),
  fluidRow(
    column(width = 12, DT::dataTableOutput("schedule_table")),
  ),
  fluidRow(
    box(
      title = "Updated Strength of Schedule Analysis",
      status = "success",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 6,
      plotlyOutput("schedule_output", height = "600px")
    ),
    box(
      column(
        "The Graph on the left shows the 22 Teams invited to the NBA Bubble, ordered by the Average Rank of their 
        opponents for the remaining games left in the Regular Season.",
        tags$br(),
        tags$br(),
        "Teams in Blue are facing lesser competition than their Team Rank, while Teams in Red are facing harder competition.  
        The Teams at the top have, on-paper, the easiest remaining schedule while the teams toward the bottom have a harder schedule.",
        
        width = 12
        )
    )
  )
)
