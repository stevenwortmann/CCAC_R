library(shiny)
library(quantmod)

ui <- fluidPage(
  pageWithSidebar(
    headerPanel(("Calculate Margin of Error:")),
    
    sidebarPanel( # User enters desired value and clicks 'Submit'
      numericInput('stanDev','Estimated standard deviation:',0.4),
      submitButton('Submit'),
      numericInput('sample','Largest possible sample size:',100),
      submitButton('Submit')
    ),
    mainPanel(
      h3('Expected Margin of Error:'),
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {

  moe <- function(n){sqrt((input$stanDev^2)/n)} # Margin of error function
  
  output$plot <- renderPlot({
    curve(moe,from=2,to=input$sample, # All values from 2 to user input
          xlab='Sample Size',ylab='Margin of Error',col='darkGreen')
  })
}

shinyApp(ui,server) # Run the app
