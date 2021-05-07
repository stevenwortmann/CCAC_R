library(tidyverse)
library(plotly)
library(rsconnect)
library(ggthemes)

url <- 'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv'
data <- as_tibble(read.csv(url))
data$date <- as.Date(data$date, '%Y-%m-%d')

ui <- function(input, output) {# Fill in the spot we created for a plot
  fluidPage(    
    # Give the page a title
    titlePanel("Telephones by region"),
    # Generate a row with a sidebar
    sidebarLayout(      
      # Define the sidebar with one input
      sidebarPanel(
        selectInput("region", "Region:", 
                    choices=colnames(WorldPhones)),hr(),
        helpText("Data from AT&T (1961) The World's Telephones.")),
      # Create a spot for the barplot
      mainPanel(plotlyOutput("phonePlot"))
    )
  )}


server <-function(input, output) {
    
    # Fill in the spot we created for a plot
    output$phonePlot <- renderPlotly({
      ggplotly(ggplot(subset(data, location %in% top_20), aes(x=date, y=new_deaths_smoothed_per_million, color=location)) + 
        geom_line() + xlab('Time') + ylab('New Deaths/Million') + ylim(0,25) + 
        ggtitle('Daily Covid Deaths per Million, Worldwide') + theme(legend.position = "bottom") + 
        scale_x_date(date_breaks = '1 month',date_labels = "%b%y", limits = as.Date(c('2020-02-01',(Sys.Date()-1)))))
    })
  }

shinyApp(ui, server) # Run the app
