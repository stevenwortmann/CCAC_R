library(tidyverse)
library(plotly)
library(rsconnect)
library(ggthemes)

url <- 'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv'
data <- as_tibble(read.csv(url))
data$date <- as.Date(data$date, '%Y-%m-%d')

# Cleaning unnecessary coding from dataset and ordering most recent
data <- data %>% select(-iso_code,-continent) %>%  arrange(desc(date))

# Non-location, date columns for use in statistics inputs
stat_cols <- names(data)[!names(data) %in% c('location','date')]

# Compended version of what we want on app
ggplotly(ggplot(subset(data, location %in% c("Israel","Mongolia")), aes(x=date, y=new_deaths_smoothed, color=location)) + geom_line() +
  xlab('Time') + ylab('New Deaths/Million') + ylim(0,4000) + ggtitle('Daily Covid Deaths per Million, Worldwide') +
  theme(legend.position = "bottom") + scale_x_date(date_breaks = '1 month',date_labels = "%b%y", limits = as.Date(c('2020-02-01',(Sys.Date()-1)))))



ui <- function(input, output) {# Fill in the spot we created for a plot
  fluidPage(
    titlePanel("Covid Data by Country"),
    sidebarLayout(
      sidebarPanel(
        selectInput("country", "Region:", 
                    choices=data$location[data$location %in% c("Israel","Mongolia")],),hr(),
        selectInput("rates1", "Data 1:", 
                    choices=stat_cols,
                    selected = 'total_cases',),hr(),
        selectInput("rates2", "Data 2:", 
                    choices=stat_cols,
                    selected = 'total_deaths',),hr(),
        helpText("Data from www.ourworldindata.org")),
      
      mainPanel(plotlyOutput("phonePlot"))
    )
  )}

server <-function(input, output) {
  
  country <- reactive({subset(data, location== input$country)})
  
    # Fill in the spot we created for a plot
    output$phonePlot <- renderPlotly({
      ggplotly(ggplot(country(), aes(x=date)) + 
        geom_line(aes(y=input$rates1), color="darkred", na.rm=T)) + 
        geom_line(aes(y=input$rates2), color="steelblue", na.rm=T) + 
        xlab('Time') + ylab('New Deaths/Million') +
        ggtitle('Daily Covid Deaths per Million, Worldwide') + theme(legend.position = "bottom") + 
        scale_x_date(date_breaks = '1 month',date_labels = "%b%y", limits = as.Date(c('2020-02-01',(Sys.Date()-1))))
    })
  }

shinyApp(ui, server) # Run the app

?selectizeInput
