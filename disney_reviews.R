library(shiny)

disney <- read.csv('/users/steve_wortmann/desktop/dat204/wk7/DisneylandReviews.csv')
# Reviews of Hong Kong, California and Paris Disneyland parks

disney_ui <- fluidPage( # Dynamic UI with R Shiny
  titlePanel("Select Disneyland Location(s) to View Reviews"),
  sidebarLayout(
    sidebarPanel( # Sidebar panel for user selection of Disney branch
      checkboxGroupInput("checkGroup", # 
                         label = ("Disneyland:"), # Each branch has index value
                         choices = list('Hong Kong' = unique(disney$Branch)[1],
                                        'California' = unique(disney$Branch)[2],
                                        'Paris' = unique(disney$Branch)[3]))),
    mainPanel( 
      plotOutput(outputId = "distPlot")
      ) # 'distPlot' is used to feed data into server for correct display
    )
  )

disney_server <- function(input,output) {
  output$distPlot <- renderPlot({
    x <- disney$Rating[disney$Branch %in% input$checkGroup]
    # Histogram made for selected groups, fed in from 'input$checkGroup'
    hist(x, breaks = (0:max(disney$Rating)), col = "#75AADB", border = "white",
         xlab = "Rating Score",
         ylab = "Number of Ratings Published",
         main = "Disneyland Location(s) Ratings")
  })
}

shinyApp(ui = disney_ui, server = disney_server)
# Running the app with the UI and server as arguments
