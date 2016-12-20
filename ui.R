library(shiny)
library(shinydashboard)
library(bubbles)


dashboardPage(
  dashboardHeader(title = "Open ROADS Community"),
  dashboardSidebar(
    sliderInput("servercnt", "Number of computers",min = 0, max = 10, value = 1, step = 1),
    selectInput("marketInterest","Market Interest",selected ="MOVIES", c("ACCESSORIES","ACCOUNTING","ARTS","ASTRONOMY","CHRISTIANITY","EDUCATION","ENTERTAINMENT","FINANCE","HEALTH","INVESTING","MOVIES","MUSIC","SPORTS","TECHNOLOGY","TELEVISION")),
    sliderInput("perceivedValue", "Monthly Price",min = 0, max = 50, value = 40, step = 10),
    sliderInput("costtoDeliver", "Monthly Cost",min = 0, max = 50, value = 10, step = 10),
    sliderInput("promotionalCost", "Promotional Cost",min = 0, max = 10, value = 1, step = 1),
    sliderInput("captiveMonths", "Captive Months",min = 0, max = 24, value = 12, step = 2),
    sliderInput("churnRate", "Monthly Churn",min = 0, max = 30, value = 2, step = 1),
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
                valueBoxOutput("completePercent", width=3),
                valueBoxOutput("timetoComplete", width=3),
                valueBoxOutput("costPerHour", width=2),
                valueBoxOutput("customersScanned", width=2),
                valueBoxOutput("totalcustomersScanned", width=2)
              ),
              fluidRow(
                box(
                  width = 8, height = 660, status = "info", solidHeader = TRUE,
                  title = "Profit by group size (green better - red worse)",
                  bubblesOutput("packagePlot", width = "100%")
                ),
                box(
                  width = 4, status = "info", height = 330, solidHeader = TRUE, align="center", style='padding:0px;',
                  title = "Economic benefit distribution",
                  plotOutput("plot")
                ),
                box(
                  width = 4, status = "info", height = 311, solidHeader = TRUE, align="center",
                  title = "Market interest distribution",
                  plotOutput("plotMarketInterest")
                )
              )
              
      ) #tab item
    ) #tab items
  )
)