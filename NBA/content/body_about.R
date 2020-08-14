body_about <- dashboardBody(fluidRow(
  fluidRow(
    column(
      box(
        title = div("About this project", style = "padding-left: 20px", class = "h2"),
        column(
          "This dashboard shows up to date information about the NBA Restart this Summer at the Orlando Bubble.",
          tags$br(),
          h3("Data"),
          HTML("Gamelog data is collected via the nbastatR package in R, which scrapes from the NBA's Stats API. Injury & Schedule data
               is web scraped from basketball-reference.com."),
          tags$br(),
          tags$br(),
          HTML("The Dashboard's data is automatically updated whenever this application is loaded by a user."),
          h3("Developer"),
          "Jacob Yablonski | ",
          tags$a(href = "https://www.linkedin.com/in/jacobyablonski/", "LinkedIn"), "|",
          tags$a(href = "https://github.com/jyablonski", "Github"),
          width = 12,
          style = "padding-left: 20px; padding-right: 20px; padding-bottom: 40px; margin-top: -15px;"
        ),
        width = 6
      ),
      width = 12,
      style = "padding: 15px"
    )
  )
)
)
