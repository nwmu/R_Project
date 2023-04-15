library(shiny)
library(leaflet)
library(dplyr)
# Define UI for first page
library(shiny)

# Define UI for first page
ui_page1 <- fluidPage(
  # Page title
  titlePanel("Whose Driving"),
  
  # Input form to select or create a driver profile
  sidebarLayout(
    sidebarPanel(
      # Select an existing profile or create a new one
      selectInput("driver_name", "Select Driver Profile:", choices = c("Create New Profile", driver_profiles$Name)),
      
      # Input fields for new driver profile
      conditionalPanel(
        condition = "input.driver_name == 'Create New Profile'",
        textInput("new_name", "Name:"),
        numericInput("new_age", "Age:", min = 16, max = 99),
        selectInput("new_sex", "Sex:", choices = c("Male", "Female"))
      ),
      
      # Button to submit driver profile information
      actionButton("submit_profile", "Submit Profile")
    ),
    
    # Display panel for selected driver profile
    mainPanel(
      h3("Selected Driver Profile"),
      textOutput("selected_profile")
    )
  )
)
server_page1 <- function(input, output, session) {
  
  # Load existing driver profiles from CSV file
  driver_profiles <- read.csv("driver_profiles.csv", stringsAsFactors = FALSE)
  
  # Create reactive to store available driver names for dropdown
  driver_names <- reactive({
    driver_profiles$name
  })
  
  # Create reactive to store selected driver name
  selected_driver <- reactive({
    input$driver_select
  })
  
  # Function to add new driver profile to CSV file
  add_driver_profile <- function(name, age, sex) {
    new_profile <- data.frame(name = name, age = age, sex = sex, stringsAsFactors = FALSE)
    driver_profiles <<- rbind(driver_profiles, new_profile)
    write.csv(driver_profiles, "driver_profiles.csv", row.names = FALSE)
  }
  
  # Create submit button event
  observeEvent(input$submit_driver, {
    # Check if name, age, and sex are filled
    if (is.null(input$name) || is.null(input$age) || is.null(input$sex)) {
      return()
    }
    
    # Add new driver profile
    add_driver_profile(input$name, input$age, input$sex)
    
    # Reset input fields
    updateTextInput(session, "name", value = "")
    updateNumericInput(session, "age", value = "")
    updateRadioButtons(session, "sex", selected = "Male")
  })
  
  # Return reactive elements
  list(driver_names = driver_names,
       selected_driver = selected_driver)
}



library(shiny)

ui_page2 <- function(){
  
  page <- tagList(
    h2("Sensors"),
    checkboxInput("alcohol_check", "Alcohol"),
    checkboxInput("seatbelt_check", "Seatbelt"),
    checkboxInput("airbag_check", "Airbag Functionality"),
    checkboxInput("doors_check", "Doors Locked"),
    checkboxInput("tires_check", "Tires"),
    actionButton("next_btn", "Next")
  )
  
  return(page)
}

server_page2 <- function(input, output, session){
  
  observeEvent(input$next_btn, {
    if(!input$alcohol_check || !input$seatbelt_check || !input$airbag_check ||
       !input$doors_check || !input$tires_check){
      showModal(modalDialog(title = "Error",
                            "Please check all sensors before moving on!",
                            easyClose = TRUE))
    } else {
      updateTabsetPanel(session, "main_panel", "ui_page3")
    }
  })
  
}


# Define page 3

# Third page - Map
ui_page3 <- fluidPage(
  
  # Page header
  headerPanel("Map"),
  
  # Input fields
  sidebarPanel(
    
    # Road condition
    selectInput(inputId = "road_cond",
                label = "Road Condition:",
                choices = c("Dry", "Wet", "Snowy", "Icy")),
    
    # Number of lanes
    selectInput(inputId = "num_lanes",
                label = "Number of Lanes:",
                choices = c(1, 2, 3, 4, 5)),
    
    # Speed limit
    textInput(inputId = "speed_limit",
              label = "Speed Limit (mph):",
              value = 30),
    
    # Light condition
    selectInput(inputId = "light_cond",
                label = "Light Condition:",
                choices = c("Daylight", "Dusk/Dawn", "Dark, Street Lights On", "Dark, Street Lights Off", "Unknown")),
    
    # Weather condition
    selectInput(inputId = "weather_cond",
                label = "Weather Condition:",
                choices = c("Clear", "Rain", "Sleet/Hail/Freezing Rain", "Snow", "Fog/Smog/Smoke", "Severe Crosswinds", "Blowing Sand, Soil, Dirt", "Other", "Unknown")),
    
    # Submit button
    actionButton(inputId = "submit_btn", label = "Submit")
    
  )
  
  
)

server_page3 <- function(input, output, session) {
  # Convert input values to numeric
  df<- read.csv("/Users/saijyothibhumireddy/Desktop/ R-Final Project/df.csv", stringsAsFactors = FALSE)
  # Probability calculation based on selected inputs
  observe({
    req(input$gender, input$age, input$license_age, input$road_cond, input$weather_cond, input$light_cond)
    # Convert input values to numeric
    light_cond <- match(input$light_cond, c("Daylight", "Dusk/Dawn", "Dark, Street Lights On", "Dark, Street Lights Off", "Unknown"))
    weather_cond <- match(input$weather_cond, c("Clear", "Rain", "Sleet/Hail/Freezing Rain", "Snow", "Fog/Smog/Smoke", "Severe Crosswinds", "Blowing Sand, Soil, Dirt", "Other", "Unknown"))
    road_cond <- match(input$road_cond, c("Dry", "Wet", "Snowy", "Icy"))
    
    # Convert age and license_age to numeric
    age <- as.numeric(input$age)
    
    
    # Calculate probability
    age_group <- get_age_group(age)
    prob <- sum(df$age_group == age_group & 
                  df$year == 2017 & 
                  df$rd_cond_cd == get_road_cond_numeric &
                  df$wthr_cd==get_weather_cond_numeric &
                  df$num_lns == num_lines &
                  df$spd_limt == speed_limit) /
      sum(df$year == 2017 & 
            df$rd_cond_cd == get_road_cond_numeric &
            df$wthr_cd==get_weather_cond_numeric &
            df$num_lns == num_lines &
            df$spd_limt == speed_limit)
    
    # Update output
    output$probability <- renderText(paste0("Probability of crash: ", prob))
  })
  
}

        

# Fourth page - Probability of Accident

ui_page4 <- fluidPage(
  titlePanel("Probability of Accident"),
  sidebarLayout(
    sidebarPanel(
      h4("Driver's Information"),
      verbatimTextOutput("profile"),
      h4("Road Conditions"),
      verbatimTextOutput("road"),
      h4("Probability of Accident"),
      verbatimTextOutput("probability")
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Page 4 server
server_page4 <- function(input, output, session) {
  
  # Function to convert road condition to numeric
  get_road_cond_numeric <- function(road_cond) {
    if (road_cond == "Dry") {
      return(1)
    } else if (road_cond == "Wet") {
      return(2)
    } else if (road_cond == "Snowy") {
      return(3)
    } else if (road_cond == "Icy") {
      return(4)
    } else {
      return(NA)
    }
  }
  
  # Function to convert light condition to numeric
  get_light_cond_numeric <- function(light_cond) {
    if (light_cond == "Daylight") {
      return(1)
    } else if (light_cond == "Dusk/Dawn") {
      return(2)
    } else if (light_cond == "Dark, Street Lights On") {
      return(3)
    } else if (light_cond == "Dark, Street Lights Off") {
      return(4)
    } else if (light_cond == "Unknown") {
      return(5)
    } else {
      return(NA)
    }
  }
  
  # Function to convert weather condition to numeric
  get_weather_cond_numeric <- function(weather_cond) {
    if (weather_cond == "Clear") {
      return(1)
    } else if (weather_cond == "Rain") {
      return(2)
    } else if (weather_cond == "Sleet/Hail/Freezing Rain") {
      return(3)
    } else if (weather_cond == "Snow") {
      return(4)
    } else if (weather_cond == "Fog/Smog/Smoke") {
      return(5)
    } else if (weather_cond == "Severe Crosswinds") {
      return(6)
    } else if (weather_cond == "Blowing Sand, Soil, Dirt") {
      return(7)
    } else if (weather_cond == "Other") {
      return(8)
    } else if (weather_cond == "Unknown") {
      return(9)
    } else {
      return(NA)
    }
  }
  df<- read.csv("/Users/saijyothibhumireddy/Desktop/ R-Final Project/df.csv", stringsAsFactors = FALSE)
  # Create reactive to store predicted probability
  output$probability <- renderText({
    prob <- sum(df$age_group == age_group & 
                  df$year == 2017 & 
                  df$rd_cond_cd == get_road_cond_numeric &
                  df$wthr_cd==get_weather_cond_numeric &
                  df$num_lns == num_lines &
                  df$spd_limt == speed_limit) /
        sum(df$year == 2017 & 
            df$rd_cond_cd == get_road_cond_numeric &
            df$wthr_cd==get_weather_cond_numeric &
            df$num_lns == num_lines &
            df$spd_limt == speed_limit)
    percent(prob, accuracy = 0.1)
  })
  # Output predicted probability
  output$page3_prob <- renderText({
    if (!is.null(predicted_prob())) {
      paste("The predicted probability of a fatal outcome for this collision is:", round(predicted_prob(), 3))
    } else {
      ""
    }
  })

  # Plot histogram of accident severity
  output$distPlot <- renderPlot({
      ggplot(df %>% filter(year == 2017 & 
                                    rd_cond_cd == get_road_cond_numeric &
                                    wthr_cd==get_weather_cond_numeric &
                                    num_lns == num_lines &
                                    spd_limt == speed_limit),
           aes(x = injy_svty_cd, fill = factor(injy_svty_cd))) +
      geom_bar(position = "dodge") +
      scale_fill_discrete(name = "Injury Severity Code") +
      labs(title = "Distribution of Injury Severity",
           x = "Injury Severity Code",
           y = "Number of Accidents")
  })
}


# Define UI for fifth page
ui_page5 <- fluidPage(
  # Page title
  titlePanel("Post Crash"),
  
  # Main panel
  mainPanel(
    # Crash type dropdown
    selectInput("crash_type", "Select Crash Type",
                choices = c("Frontal Collision", "Rear Collision", "Side Collision",
                            "Rollover", "Other")),
    
    # Defective part dropdown
    selectInput("defective_part", "Select Defective Part",
                choices = c("None", "Tires", "Brakes", "Steering",
                            "Transmission", "Other")),
    
    # Hazardous action dropdown
    selectInput("hazardous_action", "Select Hazardous Action",
                choices = c("None", "Speeding", "Distracted Driving", "Drunk Driving",
                            "Driving under Influence of Drugs", "Other")),
    
    # Prediction of injury severity
    h3("Prediction of Injury Severity:"),
    verbatimTextOutput("prediction")
  )
)

# Define server for fifth page
server_page5 <- function(input, output, session) {
  # Predict injury severity based on the data entered in previous pages
  output$prediction <- renderPrint({
    # Load the crash data
    crash_data <- read.csv("crash_data.csv")
    
    # Filter the data based on the input
    df <- filter(crash_data, crsh_type_cd == input$crash_type &
                   vehc_dfct_cd == input$defective_part &
                   hzrd_actn_cd == input$hazardous_action &
                   num_lns == as.numeric(input$num_lanes) &
                   spd_limt == as.numeric(input$speed_limit) &
                   rd_cond_cd == input$road_condition)
    
    # Check if there are any matching records
    if (nrow(df) == 0) {
      return("No matching records found.")
    }
    
    # Create a data frame with the required columns
    df_selected <- df %>% select(injy_svty_cd, age_group, gndr_cd, drug_alcohol_suspicion)
    
    # Convert the required columns to factors
    df_selected$injy_svty_cd <- as.factor(df_selected$injy_svty_cd)
    df_selected$age_group <- as.factor(df_selected$age_group)
    df_selected$gndr_cd <- as.factor(df_selected$gndr_cd)
    df_selected$drug_alcohol_suspicion <- as.factor(df_selected$drug_alcohol_suspicion)
    
    # Load the Naive Bayes model
    load("nb_model.RData")
    
    # Predict the injury severity using the Naive Bayes model
    prediction <- predict(nb_model, newdata = df_selected, type = "class")
    
    # Return the prediction
    return(prediction)
  })
}

# Combine all pages in the app


shinyApp(
  ui = navbarPage(
    "Safe Driver",
    tabPanel("Create/Select Profile", ui_page1),
    tabPanel("Sensors Check", ui_page2),
    tabPanel("Road Conditions", ui_page3),
    tabPanel("Probability", ui_page4),
    tabPanel("Post Crash Prediction", ui_page5)
  ),
  server = function(input, output, session) {
    # Call the server functions for each page
    callModule(server_page1, "page1")
    callModule(server_page2, "page2")
    callModule(server_page3, "page3")
    callModule(server_page4, "page4")
    callModule(server_page5, "page5")
  }
  )
