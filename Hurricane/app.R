#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load libraries
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)

#load data
data = read.csv("hurrican703.csv")

# Add "ALL" to the list of storm types
storm_types <- c("ALL", unique(data$Nature))
years <- c("ALL", unique(data$Season))


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Enhanced Hurricane Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("storm_type", "Select Storm Type:", 
                  choices = storm_types, selected = "ALL"),
      
      selectInput("selected_year", "Select Season (Year):", 
                  choices = years, selected = "ALL"),
      
      uiOutput("month_selector"),     # Dynamic month dropdown
      uiOutput("hurricane_selector"), # Dynamic hurricane dropdown
      
      sliderInput("wind_range", "Wind Speed (knots):", 
                  min = min(data$Wind.kt, na.rm = TRUE), 
                  max = max(data$Wind.kt, na.rm = TRUE), 
                  value = c(min(data$Wind.kt, na.rm = TRUE), max(data$Wind.kt, na.rm = TRUE)))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("hurricane_map")),
        tabPanel("Data Table", DTOutput("data_table")),
        tabPanel("Wind Speed Trend", plotOutput("wind_plot"))
      )
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Filter hurricanes by selected storm type
  filtered_type_data <- reactive({
    if (input$storm_type == "ALL") {
      data  # No filtering on storm type
    } else {
      data %>% filter(Nature == input$storm_type)
    }
  })
  
  # Filter hurricanes by selected year
  filtered_year_data <- reactive({
    if (input$selected_year == "ALL") {
      filtered_type_data()  # No filtering on year
    } else {
      filtered_type_data() %>% filter(Season == input$selected_year)
    }
  })
  
  # Update month dropdown based on selected year
  output$month_selector <- renderUI({
    available_months <- unique(filtered_year_data()$Month)
    selectInput("selected_month", "Select Month:", 
                choices = c("ALL", available_months), selected = "ALL")
  })
  
  # Filter hurricanes by selected month
  filtered_month_data <- reactive({
    if (input$selected_month == "ALL") {
      filtered_year_data()  # No filtering on month
    } else {
      filtered_year_data() %>% filter(Month == input$selected_month)
    }
  })
  
  # Update hurricane dropdown based on selected storm type, year, and month
  output$hurricane_selector <- renderUI({
    available_hurricanes <- unique(filtered_month_data()$ID)
    selectInput("selected_hurricane", "Select Hurricane:", 
                choices = available_hurricanes, 
                selected = available_hurricanes[1])
  })
  
  # Filter dataset based on storm type, year, month, hurricane, and wind speed
  filtered_data <- reactive({
    filtered_month_data() %>%
      filter(ID == input$selected_hurricane, 
             Wind.kt >= input$wind_range[1], 
             Wind.kt <= input$wind_range[2])
  })
  
  output$hurricane_map <- renderLeaflet({
    hurricane_data <- filtered_data()
    
    leaflet(hurricane_data) %>%
      addTiles() %>%
      addPolylines(lng = ~Longitude, lat = ~Latitude, color = "blue", weight = 2, opacity = 0.8) %>%
      addCircleMarkers(lng = ~Longitude, lat = ~Latitude, radius = 4, 
                       color = ~ifelse(Wind.kt >= 100, "red", "blue"),
                       popup = ~paste("Wind:", Wind.kt, "knots", "<br>", "Date:", time))
  })
  
  output$data_table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10))
  })
  
  output$wind_plot <- renderPlot({
    hurricane_data <- filtered_data()
    
    ggplot(hurricane_data, aes(x = as.POSIXct(time, format = "(%y-%m-%d %H:%M:%S)"), y = Wind.kt)) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(title = "Wind Speed Over Time", x = "Time", y = "Wind Speed (knots)") +
      theme_minimal()
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
