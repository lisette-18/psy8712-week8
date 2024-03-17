#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(shiny)
library(tidyverse)
library(rsconnect)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title for the scatterplot display
  titlePanel("Scatterplot Displaying Relationship between Question Means"),
  #sidebar layout that allows us to tell the app where to include the information
  sidebarLayout(
    sidebarPanel(
      #select gender input with all as the default display based on the instructions
      selectInput("gender", "Select Gender?", choices = c("Male", "Female", "All"),
                  selected = "All"),
      #select error band display with display as the default based on the instructions
      selectInput("errorband", " Error Band", choices = c("Display Error Band", "Suppress Error Band"),
                  selected = "Display Error Band"),
      #select inclusion criteria with include as the default display based on the instructions
      selectInput("date", "Include Data before July 1, 2017?", choices = c("Include", "Exclude"),
                  selected = "Include")),
    #plot output to show the scatterplot created 
    mainPanel(
      plotOutput("scatterplot")
    )
  )
)


# Define server logic required to draw the scatterplot
server <- function(input, output) {
  #render scatterplot
  output$scatterplot <- renderPlot({
    data <- readRDS('shiny_data.rds')
    filtered_data <- data
    #code to display gender, all by default, using it to display the default of all, but if the user switches from "All" to another gender choice, the plot is updated 
    if(input$gender != "All") {
      filtered_data <- filtered_data %>%
        filter(gender == input$gender)
    }
    #code to display date, "include" by default, using it to display the default of those who completed the assessment before July 1, 2017, but if the user switches to "Exclude", the plot is updated 
    if(input$date == "Exclude") {
      filtered_data <- filtered_data %>%
        filter(timeEnd >= ymd("2017-07-01"))
    }
    #code to display shaded error band, display by default, using it to display the default, but if the user switches from "Display" to "Suppress", the plot is updated 
    if(input$errorband == "Display Error Band"){
      ggplot(filtered_data, aes(mean1_6, mean8_10)) +
        geom_point() +
        geom_smooth(method = "lm", color = "purple") +
        labs(x = "Mean Scores of Q1 - Q6", y = "Mean Scores of Q8 - Q10", title = "Fig 1.")
    } else {
      ggplot(filtered_data, aes(mean1_6, mean8_10)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, color = "purple") +
        labs(x = "Mean Scores of Q1 - Q6", y = "Mean Scores of Q8 - Q10", title = "Fig 1.")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)