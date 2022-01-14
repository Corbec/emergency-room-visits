
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  theme = shinytheme("superhero"),
  
  # Application title
  titlePanel("Emergency Department Healthcare Spending in US 2006 - 2016"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("year",
                  "Select a range of years:",
                  min = min_year,
                  max = max_year,
                  value = c(min_year, max_year)),
      
      
      selectInput("condition",
                  "Select condition:",
                  choices = condition_items
      ),
      
      
      selectInput("spending",
                  "Select spending type:",
                  choices = spending_choices
      ),
      
      selectInput("age",
                  "Select age group:",
                  choices = age_items,
                  selected = "All Ages"
      ),
      
      radioButtons("sex",
                   "Select sex:",
                   choices = sex_items,
                   selected = "Both"),
      width = 3
    ),
    
    
    mainPanel(
      fluidRow(
        plotOutput("column"),
        width = 12 
      ),
      
      fluidRow(
        plotlyOutput("line"),
        width = 12 
      )
    )
  )
))
