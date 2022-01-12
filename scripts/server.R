

library(shiny)

# Define server logic 
shinyServer(function(input, output) {
  
  slider_visits <- reactive({
    
    ed_visits %>%
      filter(between(ed_visits$year_id, input$year[1], input$year[2]))
  })
  
  condition_visits <- reactive({
    slider_visits() %>% 
      filter(agg_cause == input$condition)
  })
  
  age_visits <- reactive({
    condition_visits() %>%
      filter(age_group_name == input$age)
  })
  
  sex_visits <- reactive({
    age_visits() %>% 
      filter(sex == input$sex)
  })
  
  # building line plot to show over year spending
  
  output$line <- renderPlotly({
    p <- sex_visits() %>%
      ggplot(aes_string(x = "year_id", y = input$spending))+
      geom_line()
    ggplotly(p)
  })
})
  