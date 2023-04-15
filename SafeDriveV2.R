#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# ui_page3.R

ui_page3 <-
  fluidPage(
    h2("Map Information"),
    selectInput("road_condition", "Road Condition", choices = c("Dry", "Wet", "Snowy/Icy")),
    selectInput("num_lines", "Number of Lines", choices = c("1", "2", "3")),
    selectInput("weather_condition", "Weather Condition", choices = c("Clear", "Cloudy", "Rainy")),
    numericInput("speed_limit", "Speed Limit", min = 0, max = 100, value = 40),
    actionButton("next", "Next")
  )

# server_page3.R

server_page3 <- function(input, output, session, data) {
  
  observe({
    if(!is.null(input$next)) {
      # Save map information to data
      data$map_info <- list(
        road_condition = input$road_condition,
        num_lines = input$num_lines,
        weather_condition = input$weather_condition,
        speed_limit = input$speed_limit
      )
      
      # Move to the next page
      updateTabsetPanel(session, "tabs", selected = "page4")
    }
  })
  
}


# Run the application 
shinyApp(ui = ui_page3, server = server_page3)
