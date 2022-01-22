
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  theme = shinytheme("superhero"),
  
  # Application title
  titlePanel("Emergency Department Healthcare Spending in US 2006 - 2016"),
  
  # Sidebar 
  tabsetPanel(
    tabPanel("Yearly Analysis",
             sidebarLayout(
               sidebarPanel(
                 
                 sliderInput("year",
                             "Select a range of years:",
                             min = min_year,
                             max = max_year,
                             value = c(min_year, max_year),
                             sep = NULL,
                             ticks = FALSE
                ),
                 
                 
                 selectInput("condition",
                             "Select condition:",
                             choices = condition_items,
                             selected = "All Conditions"
                 ),
                 
                 
                 selectInput("spending",
                             "Select spending type:",
                             choices = c("All Payers" = "mean_all",
                                         "Public Insurance" = "mean_pub",
                                         "Private Insurance" = "mean_pri",
                                         "Out of Pocket" = "mean_oop"),
                             selected = "All Payers"
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
                   plotlyOutput("total"),
                   width = 12 
                 ),
                 
                 br(),
                 
                 fluidRow(
                   plotlyOutput("line"),
                   width = 12 
                 )
               )
             )
    ),
    
    tabPanel("Side By Side Analysis",
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput("year1",
                             "Select first year for comparison:",
                             choices = year_items
                 ),
                 
                 selectInput("year2",
                             "Select second year for comparison:",
                             choices = year_items
                 ),
                 
                 selectInput("condition1",
                             "Select first condition for comparison:",
                             choices = condition_items,
                             selected = "All Conditions"
                 ),
                 
                 selectInput("condition2",
                             "Select second condition for comparison:",
                             choices = condition_items,
                             selected = "All Conditions"
                 ),
                 
                 
                 selectInput("spending1",
                             "Select first spending type for comparison:",
                             choices = c("All Payers" = "mean_all",
                                         "Public Insurance" = "mean_pub",
                                         "Private Insurance" = "mean_pri",
                                         "Out of Pocket" = "mean_oop"),
                             selected = "All Payers"
                 ),
                 
                 selectInput("spending2",
                             "Select second spending type for comparison:",
                             choices = c("All Payers" = "mean_all",
                                         "Public Insurance" = "mean_pub",
                                         "Private Insurance" = "mean_pri",
                                         "Out of Pocket" = "mean_oop"),
                             selected = "All Payers"
                 ),
                 
                 
                 radioButtons("sex1",
                              "Select first sex for comparison:",
                              choices = sex_items,
                              selected = "Both"),
                 
                 
                 radioButtons("sex2",
                              "Select second sex for comparison:",
                              choices = sex_items,
                              selected = "Both"),
                 width = 3
               ),
               
               mainPanel(
                 fluidRow(
                   plotlyOutput("year1ages"),
                   width = 12
                 ),
                 
                 br(),
                 
                 fluidRow(
                   plotlyOutput("year1conditions"),
                   width = 12
                 )
                 
               )
             )
    )
  )))