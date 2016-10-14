library(shiny)
library(shinySignals)
library(dplyr)
library(shinydashboard)
library(bubbles)

## Only run examples in interactive R sessions
if (interactive()) {
  

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sliderInput("servercnt", "Number of computers",
                min = 0, max = 20, value = 3, step = 1
    )
  ),
  dashboardBody(
    fluidRow(
      #valueBoxOutput("rate", width=3),
      valueBoxOutput("count", width=3)
    )
  )
)

server <- function(input, output, session) {
  
  output$count <- renderValueBox({
    valueBox(
      value = input$servercnt,
      subtitle = "Time to complete",icon = icon("clock-o")
    )
  })
  
  
}

shinyApp(ui, server)
}



server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs), col = 'darkgray', border = 'white')
  })
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs", "Number of observations:", min = 10, max = 500, value = 100)
    ),
    mainPanel(plotOutput("distPlot"))
  )
)

shinyApp(ui = ui, server = server)