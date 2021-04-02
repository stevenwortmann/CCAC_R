library(shiny)
library(quantmod)

ui <- fluidPage(
  titlePanel("Stock Price: Historic Analysis"), # Top panel
  
  sidebarLayout( # Side panel
    position = ("right"),
    sidebarPanel(# Create sidebar panel on right side of screen
      h4("Please, don't mind him.", style = "color:brown"), # Differing header sizes, colors
      img(src = "monkey.gif", height = 117, width = 220), # Monkey image for user enjoyment
      h5("He's just happy it's Friday.", style = "color:green"),
      helpText("Enter ticker and timeframe for maximum and minimum prices, 
      or specific date for its closing price. Information from Yahoo Finance."), # Instructions
      
      textInput(inputId = "symb", label = "Ticker Symbol", "GME"), # input$symb feeds ticker to server
      
      dateRangeInput("dates", # input$dates feeds date range to server
                     "Date range to view",
                     start = "2002-02-13", # This is first public day for default stock (GME)
                     end = as.character(Sys.Date())), # Covers up to present day
      
      dateInput("specDate", # input$specDate feeds single date to server
                "Date for specific price",
                value = as.character((Sys.Date())-1)) # Previous day is latest available closing price
      
    ), # Close sidebar panel
    
    mainPanel( # Main panel
      textOutput("text1"), # Output for closing price of specific day
      textOutput("text2"), # Output for maximum price of date range
      textOutput("text3"), # Output for minimum price of date range
      plotOutput("plot") # Output for price history plot
    ) # Close main panel
    
  ) # End sidebarLayout
  
) # End fluidPage



server <- function(input,output) {
  
  stockPrices <- reactive({ 
    getSymbols(input$symb, src = "yahoo", # User input ticker is searched in Yahoo system
               from = input$dates[1], # Dates are used for price history plot
               to = input$dates[2],
               auto.assign = FALSE)
    
  })
  
  output$text1 <- renderText({
    price <-
      getSymbols(input$symb, src = 'yahoo', # Ticker is searched in Yahoo system
                 from = (as.Date(input$specDate)-1), # Specified date must fall within range on chart
                 to = as.character(Sys.Date()), 
                 auto.assign = FALSE)
    
    
    paste("Closing price on", input$specDate,"was $",(format(round(price[index(price) == input$specDate][,4],2),nsmall=2)))
  }) # Specified date, cents-rounded price on that date is displayed
  
  output$text2 <- renderText({
    price <-
      getSymbols(input$symb, src = 'yahoo', # Ticker is searched in Yahoo system
                 from = as.Date(input$dates[1]-1), # User-specified dates are fed into server
                 to = as.Date(input$dates[2]+1),
                 auto.assign = FALSE)
    
    
    paste("Maximum price in date range was $", format(round(max(price[,2]),2),nsmall=2), "on", index(price[price[,2] == max(price[,2])]))
  }) # Maximum of recorded prices is extracted from price dataframe, as well as its date index
  
  output$text3 <- renderText({
    price <-
      getSymbols(input$symb, src = 'yahoo', # Ticker is searched in Yahoo system
                 from = as.Date(input$dates[1]-1), # User-specified dates are fed into server
                 to = as.Date(input$dates[2]+1),
                 auto.assign = FALSE)
    
    
    paste("Minimum price in date range was $", format(round(min(price[,3]),2),nsmall=2), "on", index(price[price[,3] == min(price[,3])]))
  }) # Minimum of recorded prices is extracted from price dataframe, as well as its date index
  
  output$plot <- renderPlot({ # Reactive plot suitable for assigning to the output slot
    
    chartSeries(stockPrices(), theme = chartTheme("white"),
                type = "line", TA = NULL, name = paste(input$symb, "Prices"))
    # input$symb fed into function stockPrices to generate price history plot
  })
  
} # End server

shinyApp(ui, server) # Run the app