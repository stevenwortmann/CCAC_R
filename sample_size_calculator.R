library(shiny)
library(quantmod)

ui <- fluidPage(
  pageWithSidebar(
    headerPanel(("Calculate Sample Size:")),
    
    sidebarPanel( # User enters desired value and clicks 'Submit'
      numericInput('stanDev','Estimated standard deviation:',10),
      submitButton('Submit'),
      numericInput('moe','Acceptable margin for error:',5),
      submitButton('Submit')
    ),
    mainPanel(
      h4('Sample size to use, given inputs:'),
      textOutput("sampleS") # Server output is displayed from UI-fed inputs
    )
  )
)

server <- function(input, output) { # Sample size reactive formula
    output$sampleS <- reactive({(1.96 * (input$stanDev)/(input$moe))^2})
  }

shinyApp(ui,server) # Run the app
