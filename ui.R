library(shiny)
library(shinydashboard)
library(bubbles)

dashboardPage(
  dashboardHeader(title = "Open ROADS Community"),
  dashboardSidebar(
    sliderInput("servercnt", "Number of computers",min = 0, max = 10, value = 1, step = 1),
    selectInput("marketInterest","Market Interest",selected ="MOVIES", c("ACCESSORIES","ACCOUNTING","ARTS","ASTRONOMY","CHRISTIANITY","EDUCATION","ENTERTAINMENT","FINANCE","HEALTH","INVESTING","MOVIES","MUSIC","SPORTS","TECHNOLOGY","TELEVISION")),
    sliderInput("perceivedValue", "Revenue to carrier",min = 0, max = 50, value = 40, step = 10),
    sliderInput("costtoDeliver", "Cost to deliver",min = 0, max = 50, value = 10, step = 10),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard")
      #,menuItem("Raw data", tabName = "rawdata")
    )
  ),
  dashboardBody(
    
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    tabItems(
      tabItem("dashboard",
              # Boxes need to be put in a row (or column)
              fluidRow(
                valueBoxOutput("completePercent", width=2),
                valueBoxOutput("timetoComplete", width=2),
                valueBoxOutput("costPerHour", width=2),
                valueBoxOutput("customersScanned", width=2),
                valueBoxOutput("totalcustomersScanned", width=2)
              ),
              fluidRow(
                box(
                  width = 9, status = "info", solidHeader = TRUE,
                  title = "Profit by group size (green better - red worse)",
                  bubblesOutput("packagePlot", width = "100%", height = 600)
                ),
                box(
                  width = 3, status = "info",
                  title = "Revenue Distribution",
                  plotOutput("plot")
                ),
                box(
                  width = 3, status = "info",
                  title = "Top web sites visited",
                  tableOutput("packageTable")
                )
                
              )
      ) #tab item
    ) #tab items
  )
)