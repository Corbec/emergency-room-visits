

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
  
  year1_visits <- reactive({
    ed_visits %>% 
      filter(year_id == input$year1)
  })
  
  year2_visits <- reactive({
    ed_visits %>% 
      filter(year_id == input$year2)
  })
  
  year1_condition <- reactive({
    year1_sex() %>% 
      filter(agg_cause == input$condition1)
  })
  
  year2_condition <- reactive({
    year2_sex() %>% 
      filter(agg_cause == input$condition2)
  })
  
  year1_sex <- reactive({
    year1_visits() %>% 
      filter(sex == input$sex1)
  })
  
  year2_sex <- reactive({
    year2_visits() %>% 
      filter(sex == input$sex2)
  })
  
  # building line plot for first tab to show over year spending
  
  output$line <- renderPlotly({
    p <- sex_visits() %>%
      ggplot(aes(x = year_id)) +
      geom_line(aes_string(y = input$spending), color = "red") +
      geom_line(aes_string(y = str_replace(input$spending, "mean", "upper")), color = "blue") +
      geom_line(aes_string(y = str_replace(input$spending, "mean", "lower")), color = "orange")
    ggplotly(p)
  })
  
  # building a bar plot for first tab to show mean spending by sex
  
  output$column <- renderPlotly({
    p <- age_visits() %>%
      ggplot(aes_string(x = "sex", y = input$spending, fill = "sex")) +
      geom_bar(stat = "summary",
               fun = "mean") +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  # building left age group plot for tab 2
  
  output$year1ages <- renderPlotly({
    p <- year1_condition() %>%
      ggplot(aes_string(x = "age_group_name", y = input$spending1, fill = "age_group_name")) +
      geom_col() +
      coord_flip() +
      theme(legend.position = "none") +
      scale_fill_hue(c=60, l=40)
    ggplotly(p)
  })
  
  # building right age group plot for tab 2
  
  output$year2ages <- renderPlotly({
   p <- year2_condition() %>%
      ggplot(aes_string(x = "age_group_name", y = input$spending2, fill = "age_group_name")) +
      geom_col() +
      coord_flip() +
      theme(legend.position = "none") +
      scale_fill_hue(c=60, l=40)
   ggplotly(p)
  })
  
  # building left condition plot for tab 2
  
  output$year1conditions <- renderPlotly({
    p <- year1_sex() %>%
      ggplot(aes_string(x = "agg_cause", y = input$spending1, fill = "agg_cause")) +
      geom_bar(stat = "summary",
               fun = "mean") +
      coord_flip() +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  # building right condition plot for tab 2
  
  output$year2conditions <- renderPlotly({
    p <- year2_sex() %>%
      ggplot(aes_string(x = "agg_cause", y = input$spending2, fill = "agg_cause")) +
      geom_bar(stat = "summary",
               fun = "mean") +
      coord_flip() +
      theme(legend.position = "none")
    ggplotly(p)
  })
})
