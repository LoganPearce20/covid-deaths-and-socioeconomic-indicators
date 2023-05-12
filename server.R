library(shiny)
library(ggplot2)

df_joined <- read.csv("data/df_joined.csv")
df_income <- read.csv("data/df_income.csv")

df_distinct_deaths <- df_joined %>%
  distinct(State, total_deaths)

df_distinct_conditions <- df_joined %>%
  group_by(State, Condition) %>%
  mutate(condition_deaths = sum(COVID.19.Deaths, na.rm = T)) %>%
  distinct(State, Condition, condition_deaths)

df_distinct_income_deaths <- df_joined %>%
  group_by(State, Income) %>%
  mutate(income_deaths = sum(COVID.19.Deaths, na.rm = T)) %>%
  distinct(State, Income, income_deaths)

#Set plot theme
plot_theme <- ggdark::dark_theme_gray(base_size = 14) + 
  theme(plot.title = element_text(),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank())

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  df_filter_condition_group <- reactive({
    subset(df_joined, Condition.Group == input$condition_group)
  })
  df_filter_condition <- reactive({
    subset(df_joined, Condition == input$condition)
  })
  df_filter_age <- reactive({
    subset(df_joined, Age.Group == input$age_group)
  })
  df_filter_state <- reactive({
    subset(df_distinct_conditions, State == input$state)
  })
  
  output$comment1 <- renderText({
    "The project aims to investigate the relationship between the socioeconomic factors of each state 
    and their impact on COVID-19 related deaths in the year 2020. 
    The study will gather data on various socioeconomic indicators such as income, mariage rates, race, political alignment, and age in each state. 
    This data will then be analyzed to identify any correlations between these factors and the number and cause of COVID-19 deaths in each state. 
    The results of this study could potentially provide valuable insights into the social determinants of health and how they influence the spread and impact of infectious diseases like COVID-19. 
    Additionally, the findings could inform public health policies aimed at mitigating the impact of such diseases on vulnerable populations in the future."
  })
  
#-----------------------covid deaths by income---------------------------------
  output$plot1 <- renderPlot({
    num_states <- length(unique(df_distinct_deaths$State))
    text_angle <- ifelse(num_states >= 50, 0 , 90)
    
    ggplot(df_joined, aes(x = State, y = Income, fill = State)) +
      geom_col(show.legend = F, position = "dodge", stat = "identity") +
      labs(title = "Income per State",
           x = "State",
           y = "Income") +
      plot_theme +

      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) + coord_flip()

  })
  output$plot2 <- renderPlot({
    num_states <- length(unique(df_distinct_deaths$State))
    text_angle <- ifelse(num_states >= 50, 0 , 90)
    
    ggplot(df_distinct_deaths, aes(x = State, y = total_deaths, fill = State)) +
      geom_col(show.legend = F, position = "dodge", stat = "identity") +
      labs(title = "Deaths in Each State") +
      plot_theme +coord_flip()

      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) + coord_flip()

})
    output$plot3 <- renderPlot({
      metric_string <- str_to_title(gsub("\\.", " ", input$condition))
      num_conditions <- length(unique(df_filter_condition()$Condition))
      text_angle <- ifelse(num_conditions >= 10, 0 , 90)
      
      ggplot(df_filter_state(), aes(x = Condition, y = condition_deaths, fill = Condition)) +
        geom_col(show.legend = F, position = "dodge", stat = "identity") +
        labs(title = "Deaths in Each State") +
        ggtitle(paste("Deaths by condition in",input$state)) +
        plot_theme +

        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank()) + coord_flip()

      
    })
    output$plot4 <- renderPlot({
      ggplot(df_distinct_income_deaths, aes(x = Income, y = income_deaths)) +
        geom_smooth() +
        xlab("Income") +
        ylab("Deaths") +
        ggtitle("Covid 19 deaths by Income") + 
        plot_theme
    })
    output$comment2 <- renderText({
      "Based on this data there is no evidence that income plays a role into how likely you are to 
      die from covid or what covid related conditions you are susceptible to."
    })
 
    #----------------- covid deaths by marriage rate-------------------------------------   
    output$plot5 <- renderPlot({
      num_states <- length(unique(df_distinct_deaths$State))
      text_angle <- ifelse(num_states >= 50, 0 , 90)
      
      ggplot(df_joined, aes(x = marriageRate, y = total_deaths, fill = State)) +
        geom_col(show.legend = F, position = "dodge", stat = "identity") +
        labs(title = "Covid Death by Marriage Rate") +
        plot_theme +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank()) 
})


})



