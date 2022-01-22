

library(shiny)

# Define server logic 
shinyServer(function(input, output) {
  
  slider_visits <- reactive({
    
    ed_visits %>%
      filter(between(ed_visits$year_id, input$year[1], input$year[2]))
  })
  
  slider_total <- reactive({
    total_spending %>% 
      filter(between(total_spending$year, input$year[1], input$year[2]))
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
  
  # page 2 filters
  
  year1_visits <- reactive({
    ed_visits %>% 
      filter(year_id == input$year1)
  })
  
  year2_visits <- reactive({
    ed_visits %>% 
      filter(year_id == input$year2)
  })
  
  year1_spending <- reactive({
    year1_visits() %>% 
      select(year_id, age_group_name, sex, agg_cause, input$spending1) %>% 
      rename(spending = input$spending1)
  })
  
  year2_spending <- reactive({
    year2_visits() %>% 
      select(year_id, age_group_name, sex, agg_cause, input$spending2) %>% 
      rename(spending = input$spending2)
  })
  
  year1_sex <- reactive({
    year1_spending() %>% 
      filter(sex == input$sex1)%>% 
      mutate(variables = paste(input$year1,
                               input$sex1,
                               input$spending1))
  })
  
  year2_sex <- reactive({
    year2_spending() %>% 
      filter(sex == input$sex2)%>% 
      mutate(variables = paste(input$year2,
                               input$sex2,
                               input$spending2))
  })
  
  year1_condition <- reactive({
    year1_sex() %>% 
      filter(agg_cause == input$condition1) %>%  
      mutate(variables = paste(input$year1,
                               input$sex1,
                               input$spending1,
                               input$condition1))
  })
  
  year2_condition <- reactive({
    year2_sex() %>% 
      filter(agg_cause == input$condition2) %>% 
      mutate(variables = paste(input$year2,
                               input$sex2,
                               input$spending2,
                               input$condition2))
  })
  
  
  # building total spending line plot
  
  output$total <- renderPlotly({
    p <- slider_total() %>%
      ggplot(aes(x = year)) +
      geom_line(aes_string(y = input$spending), color = "blue") +
      geom_ribbon(aes_string(ymin = str_replace(input$spending, "mean", "lower"),
                             ymax = str_replace(input$spending, "mean", "upper")),
                  alpha=0.3,
                  fill = "dodgerblue") +
      ggtitle(paste("Per Capita Total Spending: ", input$year[1], " to ", input$year[2])) +
      xlab(FALSE) +
      ylab("USD") +
      theme(plot.title = element_text(hjust = 0.5))
    ggplotly(p)
  })
  
  # building line plot for first tab to show over year spending
  
  output$line <- renderPlotly({
    p <- sex_visits() %>%
      ggplot(aes(x = year_id)) +
      geom_line(aes_string(y = input$spending), color = "red") +
      geom_ribbon(aes_string(ymin = str_replace(input$spending, "mean", "lower"),
                  ymax = str_replace(input$spending, "mean", "upper")),
                  alpha=0.3,
                  fill = "darkorange") +
      ggtitle(paste("Per Capita ER Spending: ", input$year[1], " to ", input$year[2])) +
      xlab(FALSE) +
      ylab("USD") +
      theme(plot.title = element_text(hjust = 0.5))
    ggplotly(p)
  })
  
  # building a bar plot for first tab to show mean spending by sex - decided not to use
  
  output$column <- renderPlotly({
    p <- age_visits() %>%
      ggplot(aes_string(x = "sex", y = input$spending, fill = "sex")) +
      geom_bar(stat = "summary",
               fun = "mean") +
      theme(legend.position = "none") +
      ggtitle(paste("Per Capita ER Spending By Sex: ", input$year[1], " to ", input$year[2])) +
      xlab(FALSE) +
      ylab("USD") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_manual(values = wes_palette("GrandBudapest1"))
    ggplotly(p)
  })
  
  # building age group plot for tab 2
  
  age_plot <- reactive({
    
    year1_condition() %>% 
      rbind(year2_condition())
      
  })
  
  output$year1ages <- renderPlotly({
    p <- age_plot() %>%
      #filter(age_group_name != "All Ages") %>% 
      ggplot(aes_string(x = "age_group_name", y = "spending", fill = "variables")) +
      geom_col(position = position_dodge2()) +
      coord_flip() +
      ylab(paste("USD Spent Per Capita")) +
      xlab(FALSE) +
      ggtitle(paste(input$condition1," ",
                    input$year1, " ",
                    input$sex1, " ",
                    input$spending1,
                    " vs ",
                    input$condition2, " ",
                    input$year2, " ",
                    input$sex2, " ",
                    input$spending2
                    )) +
      scale_fill_manual(values = wes_palette("Royal1")) +
      scale_x_discrete(limits = rev(c("Under 5", "5 to 9", "10 to 14", "15 to 19", "20 to 24",
                                      "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
                                      "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74",
                                      "75 to 79", "80 to 84", "85 and over", "All Ages")))
    ggplotly(p)
  })
  
  # building condition plot for tab 2
  
  sex_plot <- reactive({
    year1_sex() %>% 
      rbind(year2_sex())
  })
  
  output$year1conditions <- renderPlotly({
    p <- sex_plot() %>%
      filter(agg_cause != "All Conditions") %>% 
      ggplot(aes_string(x = "agg_cause", y = "spending", fill = "variables")) +
      geom_bar(stat = "summary",
               fun = "mean",
               position = position_dodge()) +
      coord_flip() +
      ylab(paste("USD Spent Per Capita")) +
      xlab(FALSE) +
      ggtitle(paste("Spending By Condition: ",
                    input$year1, " ",
                    input$sex1, " ",
                    input$spending1,
                    " vs ",
                    input$year2, " ",
                    input$sex2, " ",
                    input$spending2
                    )) +
      scale_fill_manual(values = wes_palette("FantasticFox1"))
    ggplotly(p)
  })
})
