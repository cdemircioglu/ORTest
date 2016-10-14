library(shiny)
library(shinydashboard)
library(bubbles)

dashboardPage(
  dashboardHeader(title = "Open ROADS Community"),
  dashboardSidebar(
    sliderInput("servercnt", "Number of computers",min = 0, max = 20, value = 3, step = 1),
    selectInput("marketInterest","Market Interest",selected ="INVESTING", c("ACCESSORIES","ACCOUNTING","ADD","ADULT_EDUCATION","ARTS_CRAFTS","BARBECUES_GRILLING","CHRISTIANITY","EDUCATIONAL_INSTITUTIONS","ENTERTAINMENT_NEWS_CELEBRITY_SITES","ENTERTAINMENT_OTHER","FINANCIAL_PLANNING","HEALTH_LOWFAT_COOKING","INVESTING","LITERATURE_BOOKS","MOVIES","MUSIC","PRIVATE_SCHOOL","PSYCHOLOGY_PSYCHIATRY","REFERENCE_MATERIALS_MAPS","SMOKING_CESSATION","SPACE_ASTRONOMY","SPECIAL_EDUCATION","STREAMING_DOWNLOADABLE_VIDEO","TAX_PLANNING","TELEVISION","TEXT_MESSAGING_SMS","WIKIS","YEAR_712_EDUCATION")),
    sliderInput("perceivedValue", "Perceived value",min = 0, max = 50, value = 40, step = 10),
    sliderInput("costtoDeliver", "Cost to deliver",min = 0, max = 50, value = 10, step = 5),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("Raw data", tabName = "rawdata")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              # Boxes need to be put in a row (or column)
              fluidRow(
                valueBoxOutput("completePercent", width=3),
                valueBoxOutput("timetoComplete", width=3),
                valueBoxOutput("costPerHour", width=3),
                valueBoxOutput("customersScanned", width=3)
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
      ) #tab item
    ) #tab items
  )
)