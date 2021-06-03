library(tidyverse)
library(plotly)
library(rsconnect)
library(ggthemes)
library(shiny)

# Data sourced from ourworldindata.org/coronavirus
url <- 'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv'
data <- as_tibble(read.csv(url))
data$date <- as.Date(data$date, '%Y-%m-%d')

# Cleaning unnecessary coding from data set, order most recent
data <- data %>% select(-iso_code,-continent) %>%  arrange(desc(date))

# Non-location, date columns for use in statistics inputs
# Regions are isolated for user selection in ui
stat_cols <- names(data)[!names(data) %in% c('location','date')]
regions <- unique(data$location)

ui <- fluidPage(
  titlePanel("Covid Data by Country"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "regions",label = NULL, # Place holder enabled if first choice is empty
        choices = c("Select geographic region(s)" = "", regions), 
        multiple = TRUE),
      selectInput("rates1", "Data set 1:",choices=stat_cols,
                  selected = 'new_cases_smoothed',),
      helpText("Data from www.ourworldindata.org")),
    mainPanel(plotlyOutput(outputId = "p")))
)

server <- function(input, output, session, ...) {
  output$p <- renderPlotly({
    req(input$regions)
    if (identical(input$regions, "")) return(NULL) # No graph displayed for empty user input
    p <- ggplot(data = filter(data, location %in% input$regions)) + 
      geom_line(aes(date, get(input$rates1), group = location, color = location)) + 
      theme(axis.title = element_blank(), legend.title = element_blank()) +
      scale_x_date(date_breaks = '3 month', date_labels = "%b%y",
                   limits = as.Date(c('2020-02-01',(Sys.Date()-1))))
    # Graph resolution is automatically generated from user data selection extents
    height <- session$clientData$output_p_height
    width <- session$clientData$output_p_width
    ggplotly(p, height = height, width = width) %>% # Adjusts legend underneath graph
      layout(legend = list(orientation = "h", x = 0.3, y = -0.2))
  })
}

shinyApp(ui, server)
               