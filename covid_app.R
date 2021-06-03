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
regions <- unique(data$location)
stat_cols

ui <- fluidPage(
  titlePanel("Covid Data by Country"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "regions", 
        label = NULL,
        # placeholder is enabled when 1st choice is an empty string
        choices = c("Select geographic region(s)" = "", regions), 
        multiple = TRUE),hr(),
      selectInput("rates1", "Data set 1:", 
                  choices=stat_cols,
                  selected = 'new_cases_smoothed',),hr(),
      helpText("Data from www.ourworldindata.org")),
    mainPanel(plotlyOutput(outputId = "p")))
)

server <- function(input, output, session, ...) {
  output$p <- renderPlotly({
    req(input$regions)
    if (identical(input$regions, "")) return(NULL)
    p <- ggplot(data = filter(data, location %in% input$regions)) + 
      geom_line(aes(date, get(input$rates1), group = location, color = location))
    height <- session$clientData$output_p_height
    width <- session$clientData$output_p_width
    ggplotly(p, height = height, width = width)
  })
}

shinyApp(ui, server)
