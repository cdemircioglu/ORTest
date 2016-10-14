dashboardPage(
  dashboardHeader(title = "Open ROADS Community",titleWidth = 350),
  dashboardSidebar(
    sliderInput("rateThreshold", "Number of computers",
                min = 0, max = 20, value = 3, step = 1
    ),

    sliderInput("perceivedValue", "Perceived value",
                min = 0, max = 50, value = 40, step = 10
    ),
    sliderInput("costtoDeliver", "Cost to deliver",
                min = 0, max = 50, value = 10, step = 5
    ),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("Raw data", tabName = "rawdata")
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
                ),
                box(
                  width = 4, status = "info",
                  title = "Top web sites visited",
                  tableOutput("packageTable")
                )
              )
      ),
      tabItem("rawdata",
              numericInput("maxrows", "Rows to show", 25),
              verbatimTextOutput("rawtable"),
              downloadButton("downloadCsv", "Download as CSV")
      )
    )
  )
)
