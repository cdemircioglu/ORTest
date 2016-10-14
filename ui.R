dashboardPage(
  dashboardHeader(title = "Open ROADS Community",titleWidth = 350),
  dashboardSidebar(
    sliderInput("rateThreshold", "Number of computers",
                min = 0, max = 20, value = 3, step = 1
    )
  ),
  dashboardBody(
              fluidRow(
                valueBoxOutput("rate", width=3),
                valueBoxOutput("count", width=3)
              )
  )
)
