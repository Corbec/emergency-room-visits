
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
                             value = c(min_year, max_year)),
                 
                 
                 selectInput("condition",
                             "Select condition:",
                             choices = condition_items
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
                   plotlyOutput("column"),
                   width = 12 
                 ),
                 
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
                             "Select year for left plot:",
                             choices = year_items
                 ),
                 
                 selectInput("year2",
                             "Select year for right plot:",
                             choices = year_items
                 ),
                 
                 selectInput("condition1",
                             "Select condition for left plot:",
                             choices = condition_items
                 ),
                 
                 selectInput("condition2",
                             "Select condition for right plot:",
                             choices = condition_items
                 ),
                 
                 
                 selectInput("spending1",
                             "Select spending type for left plot:",
                             choices = c("All Payers" = "mean_all",
                                         "Public Insurance" = "mean_pub",
                                         "Private Insurance" = "mean_pri",
                                         "Out of Pocket" = "mean_oop"),
                             selected = "All Payers"
                 ),
                 
                 selectInput("spending2",
                             "Select spending type for left plot:",
                             choices = c("All Payers" = "mean_all",
                                         "Public Insurance" = "mean_pub",
                                         "Private Insurance" = "mean_pri",
                                         "Out of Pocket" = "mean_oop"),
                             selected = "All Payers"
                 ),
                 
                 
                 radioButtons("sex1",
                              "Select sex for left plot:",
                              choices = sex_items,
                              selected = "Both"),
                 
                 
                 radioButtons("sex2",
                              "Select sex for right plot:",
                              choices = sex_items,
                              selected = "Both"),
                 width = 3
               ),
               
               mainPanel(
                 column(
                   plotlyOutput("year1ages"),
                   plotlyOutput("year1conditions"),
                   width = 6
                 ),
                 
                 column(
                   plotlyOutput("year2ages"),
                   plotlyOutput("year2conditions"),
                   width = 6
                 )
                 
               )
             )
    )
  )))