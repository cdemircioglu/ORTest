dashboardPage(
  dashboardHeader(title = "Open ROADS Community",titleWidth = 350),
  dashboardSidebar(
    sliderInput("rateThreshold", "Number of computers",
                min = 0, max = 20, value = 3, step = 1
    ),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                valueBoxOutput("rate", width=3),
                valueBoxOutput("count", width=3),
                valueBoxOutput("cost", width=3),
                valueBoxOutput("users", width=3)
              ),
              fluidRow(
                box(
                  width = 8, status = "info", solidHeader = TRUE,
                  title = "Revenue by group size (green better - red worse)",
                  bubblesOutput("packagePlot", width = "100%", height = 600)
                )
              )
      )
    )
  )
)
