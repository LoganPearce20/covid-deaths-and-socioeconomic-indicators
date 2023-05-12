library(shiny)
library(ggplot2)
library(viridis)

df_joined <- read.csv("data/df_joined.csv")
df_income <- read.csv("data/df_income.csv")

df_distinct_deaths <- df_joined %>%
  distinct(State, total_deaths)

df_distinct_conditions <- df_joined %>%
  group_by(State, Condition) %>%
  mutate(condition_deaths = sum(COVID.19.Deaths, na.rm = T)) %>%
  distinct(State, Condition, condition_deaths, Income)

df_distinct_income_deaths <- df_joined %>%
  group_by(State, Income) %>%
  mutate(income_deaths = sum(COVID.19.Deaths, na.rm = T)) %>%
  distinct(State, Income, income_deaths)

df_distinct_democrat <- df_joined %>%
  group_by(State, total_democrat) %>%
  mutate(democrat_deaths = sum(COVID.19.Deaths, na.rm = T)) %>%
  distinct(State, total_democrat, democrat_deaths) %>%
  filter(total_democrat >= .5)

df_distinct_republican <- df_joined %>%
  group_by(State, total_republican) %>%
  mutate(republican_deaths = sum(COVID.19.Deaths, na.rm = T)) %>%
  distinct(State, total_republican, republican_deaths) %>%
  filter(total_republican >= .5)

#df_distinct_race <- df_joined %>%
  #group_by(State, Hispanic) %>%
  #mutate(death_by_race = sum(COVID.19.Deaths, na.rm = T)) %>%
  #select(State, Hispanic, COVID.19.Deaths)

df_distinct_age <- df_joined %>%
  group_by(State, Age.Group) %>%
  mutate(death_by_age = sum(COVID.19.Deaths, na.rm = T)) %>%
  distinct(State, Age.Group, death_by_age)

df_distinct_marriages <- df_joined %>%
  group_by(State, marriageRate) %>%
  mutate(marriage_deaths = sum(COVID.19.Deaths, na.rm = T)) %>%
  distinct(State, marriageRate, marriage_deaths)

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
    subset(df_distinct_conditions, Condition == input$condition)
  })
  df_filter_age <- reactive({
    subset(df_distinct_age, Age.Group == input$age_group)
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
    ggplot(df_joined, aes(x = State, y = Income, fill = State)) +
      geom_col(show.legend = F, position = "dodge", stat = "identity") +
      labs(title = "Income per State",
           x = "State",
           y = "Income") +
      plot_theme +
      coord_flip()
  })
  output$plot2 <- renderPlot({
    ggplot(df_distinct_deaths, aes(x = State, y = total_deaths, fill = State)) +
      geom_col(show.legend = F, position = "dodge", stat = "identity") +
      labs(title = "Deaths in Each State") +
      plot_theme +
      coord_flip()
})
    output$plot3 <- renderPlot({
      ggplot(df_filter_state(), aes(x = Condition, y = condition_deaths, fill = Condition)) +
        geom_col(show.legend = F, position = "dodge", stat = "identity") +
        labs(title = "Deaths in Each State") +
        ggtitle(paste("Deaths by condition in",input$state)) +
        plot_theme +
        coord_flip()
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
    
    output$plot5 <- renderPlot({
      metric_string <- str_to_title(gsub("\\.", " ", input$condition))
      
      ggplot(df_filter_condition(),aes(x = Income, y = condition_deaths))+
        geom_smooth() + 
        ggtitle(input$condition,"Deaths Based on Income") +
        ylab(input$condition) +
        plot_theme
    })
    #----------------- covid deaths by marriage rate-------------------------------------   
    output$plot6 <- renderPlot({
      ggplot(df_distinct_marriages, aes(x = State, y = marriageRate, fill = State)) +
        geom_col(show.legend = F, position = "dodge", stat = "identity") +
        labs(title = "Marriage Rate in Each State") +
        plot_theme +
        coord_flip()
    })
    output$plot7 <- renderPlot({
      ggplot(df_distinct_deaths, aes(x = marriageRate, y = marriage_deaths)) +
        geom_smooth() +
        xlab("Marriage Rate") +
        ggtitle("Deaths by Marriage Rate") +
        plot_theme
    })

    #------------------ covid deaths by political Affiliation ----------------------------
    output$plot7 <- renderPlot({
      metric_string <- str_to_title(gsub("\\.", " ", input$political_party))
      
      ggplot(df_distinct_democrat, aes(x = State, y = democrat_deaths, fill = State)) +
        geom_col() +
        xlab("democrat") +
        ylab("Deaths") +
        ggtitle("Covid 19 deaths by vote") + 
        plot_theme
      
    })
    output$plot8 <- renderPlot({
      metric_string <- str_to_title(gsub("\\.", " ", input$political_party))
      
      ggplot(df_distinct_republican, aes(x = State, y = republican_deaths, fill = State)) +
        geom_col() +
        xlab("republican") +
        ylab("Deaths") +
        ggtitle("Covid 19 deaths by vote") + 
        plot_theme
      
    })
    output$plot9 <- renderPlot({
      
      ggplot(df_distinct_democrat,aes(x = total_democrat, y = democrat_deaths))+
        geom_smooth() + 
        ggtitle("Proportion of Democrats") +
        ylab("Deaths") +
        plot_theme
    })   
    output$plot10 <- renderPlot({
      
      ggplot(df_distinct_republican,aes(x = total_republican, y = republican_deaths))+
        geom_smooth() + 
        ggtitle("Proportion of Republican") +
        ylab("Deaths") +
        plot_theme
    })   
    
    output$comment3 <- renderText({
      "Based on this data there is evidence that the greater amount of rebublicans by state the less deaths there were. 
      Compared to the states with more Demorats that have more deaths."
    })
    
    
    #---------------------------------- deaths by race ------------------------------
    
    # output$plot11 <- renderPlot({
    #   metric_string <- str_to_title(gsub("\\.", " ", input$Race))
    #   ggplot(df_distinct_race, aes(x = Race, y = death_by_race, fill = Race)) +
    #     geom_col() +
    #     xlab("Race") +
    #     ylab("Deaths") +
    #     ggtitle("Covid 19 deaths by race") + 
    #     plot_theme
    # })   
    # 
    
    #----------------------------------- deaths by age ------------------------------
    output$plot11 <- renderPlot({
      #metric_string <- str_to_title(gsub("\\.", " ", input$political_party))
      
      ggplot(df_distinct_age, aes(x = Age.Group, y = death_by_age, fill = Age.Group)) +
        geom_col() +
        xlab("Age") +
        ylab("Deaths") +
        ggtitle("Covid 19 deaths by age") + 
        plot_theme
      
    })
    output$plot12 <- renderPlot({
      metric_string <- str_to_title(gsub("\\.", " ", input$age_group))
      
      ggplot(df_distinct_age,aes(x = Age.Group, y = death_by_age))+
        geom_smooth() + 
        ggtitle(input$age_group,"Deaths Based on age") +
        ylab(input$age_group) +
        plot_theme
    }) 
})




