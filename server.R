library(shiny)
library(ggplot2)
library(viridis)
library(lme4)
library(dplyr)
library(DT)
library(tidyverse)

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
  filter(Age.Group %in% c("0-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+")) %>%
  group_by(State, Age.Group, TotalPop) %>%
  mutate(death_by_age = sum(COVID.19.Deaths, na.rm = T)) %>%
  distinct(State, Age.Group,death_by_age, TotalPop)

df_distinct_marriages <- df_joined %>%
  group_by(State, marriageRate) %>%
  mutate(marriage_deaths = sum(COVID.19.Deaths, na.rm = T)) %>%
  distinct(State, marriageRate, marriage_deaths)

df_distinct_marriage_condition <- df_joined %>%
  group_by(State, Condition) %>%
  mutate(marriage_deaths = sum(COVID.19.Deaths, na.rm = T)) %>%
  distinct(marriageRate, marriage_deaths, Condition)

df_distinct_age_condition <- df_joined %>%
  filter(Age.Group %in% c("0-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+")) %>%
  group_by(State, Condition) %>%
  distinct(State, Condition, COVID.19.Deaths, Age.Group)

df_distinct_pop <- df_joined %>%
  group_by(State, TotalPop) %>%
  mutate(pop_deaths = sum(COVID.19.Deaths, na.rm = T)) %>%
  distinct(State,TotalPop,pop_deaths)

#------------------------------------linear regression model----------------------
model <- lm(death_by_age ~  Age.Group + State + TotalPop,  data = df_distinct_age)

df_distinct_age$casualty_predictions <-  predict(model, newdata = df_distinct_age)

df_distinct_age$casualty_predictions <- ceiling(df_distinct_age$casualty_predictions)

df_distinct_age$casualty_predictions[df_distinct_age$casualty_predictions < 0] <- 0
  


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
  df_filter_age2 <- reactive({
    subset(df_distinct_age2, State == input$state)
  })
  df_filter_state <- reactive({
    subset(df_distinct_conditions, State == input$state)
  })
  df_filter_condition2 <- reactive({
    subset(df_distinct_age_condition, Condition == input$condition)
    })
  df_filter_condition3 <- reactive({
    subset(df_distinct_marriage_condition, Condition == input$condition) 
  })
  output$comment1 <- renderText({
    "<p>The project aims to investigate the relationship between the socioeconomic factors of each state and their impact on COVID-19 related deaths in the year 2020. The study will gather data on various socioeconomic indicators such as income, mariage rates, race, political alignment, and age in each state. This data will then be analyzed to identify any correlations between these factors and the number and cause of COVID-19 deaths in each state. The results of this study could potentially provide valuable insights into the social determinants of health and how they influence the spread and impact of infectious diseases like COVID-19. Additionally, the findings could inform public health policies aimed at mitigating the impact of such diseases on vulnerable populations in the future.<br> <p> The scope of this project is limited to the socioeconomic factors of income, marriage rate, density of republican vs. democratic voters, population density, and age and how each one of those interacts with ones liklihood of dying after contracting covid-19. <br> <p> The requirements of this project are as follows: <br> <p> 1. Covid-19 DATA <br> <p> 2. Socio-economic Data <br> <p> 3. Data Cleaning and Integration <br> <p> 4. Data Analysis and Visualization <br> <p> 5. Create a linear mixed affects Model <br> <p> 6. Create a prediction model"
  })
#-----------------------covid deaths by income---------------------------------
  output$plot1 <- renderPlot({
    ggplot(df_joined, aes(x = State, y = Income, fill = State)) +
      geom_col(show.legend = F, position = "dodge", stat = "identity") +
      ggtitle('Income per state', "This graph shows you the average income for every state in 2020") +
      xlab('State') +
      ylab('Income') +
      plot_theme +
      coord_flip() 


  })
  output$plot2 <- renderPlot({
    ggplot(df_distinct_deaths, aes(x = State, y = total_deaths, fill = State)) +
      geom_col(show.legend = F, position = "dodge", stat = "identity") +
      ggtitle("Deaths in Each State", 'This graph shows the total number of deaths in each state') +
      plot_theme +
      coord_flip()


})
    output$plot3 <- renderPlot({
      ggplot(df_filter_condition(), aes(x = State, y = condition_deaths, fill = State)) +
        geom_col(show.legend = F, position = "dodge", stat = "identity") +
        ggtitle(input$condition, 'This reactive graph shows the total number of deaths by condition in each state') +
        plot_theme +
        coord_flip()
    })
    output$plot16 <- renderPlot({
      ggplot(df_filter_state(), aes(x = Condition, y = condition_deaths, fill = Condition)) +
        geom_col(show.legend = F, position = "dodge", stat = "identity") +
        labs(title = "Deaths in Each State") +
        ggtitle(input$state,'This graph shows how each condition is affecting each state differently') +
        plot_theme +
        coord_flip()
    })
    output$plot4 <- renderPlot({
      ggplot(df_distinct_income_deaths, aes(x = Income, y = income_deaths)) +
        geom_smooth() +
        xlab("Income") +
        ylab("Deaths") +
        ggtitle("Covid 19 deaths by Income", "This graph shows the relationship between income and the total number of deaths due to covid-19") + 
        plot_theme
    })
    output$comment2 <- renderText({
      "Based on this data and a t-test of 1.044 from our linear mixed-effects model there is no evidence that income is an effective indicator in regards
      to someones risk of death if they contract covid-19."
    })
    
    output$plot5 <- renderPlot({
      metric_string <- str_to_title(gsub("\\.", " ", input$condition))
      
      ggplot(df_filter_condition(),aes(x = Income, y = condition_deaths))+
        geom_smooth() + 
        ggtitle(input$condition,"This graph shows the relationship between income and the reported covid related condition that lead to the persons death") +
        ylab(input$condition) +
        plot_theme
    })
    #----------------- covid deaths by marriage rate-------------------------------------   
    output$plot6 <- renderPlot({
      ggplot(df_distinct_marriages, aes(x = State, y = marriageRate, fill = State)) +
        geom_col(show.legend = F, position = "dodge", stat = "identity") +
        ggtitle('Marriage rate in each state', 'This graph shows the marriage rate for every state') +
        plot_theme +
        coord_flip()
    })
    output$plot7 <- renderPlot({
      ggplot(df_distinct_marriages, aes(x = marriageRate, y = marriage_deaths)) +
        geom_smooth() +
        xlab("Marriage Rate") +
        ggtitle("Deaths by Marriage Rate",'This graph shows the relationship between a states marriage rate and covid-19 deaths') +
        plot_theme
    })
    output$plot15 <- renderPlot({
      metric_string <- str_to_title(gsub("\\.", " ", input$condition))
      
      ggplot(df_filter_condition3(), aes(x = marriageRate, y = marriage_deaths)) +
        geom_smooth() + 
        ggtitle(input$condition,"This reactive graph shows you the relationship between marriage rate and the conditions that lead to covid-19 deaths") +
        ylab(input$condition) +
        plot_theme
    })
    output$comment5 <- renderText({
      "Based on this data and a t-test value of -1.529 there does not seem to be a strong link between a states marriage rate and covid-19 deaths"
    })

    #------------------ covid deaths by political Affiliation ----------------------------
    output$plot8 <- renderPlot({
      metric_string <- str_to_title(gsub("\\.", " ", input$political_party))
      
      ggplot(df_distinct_democrat, aes(x = State, y = democrat_deaths, fill = State)) +
        geom_col(show.legend = F) +
        xlab("State") +
        ylab("Deaths") +
        ggtitle("Covid 19 deaths by democratic states", 'This graph shows the total number of covid-19 deaths in democratic states') + 
        plot_theme +
        coord_flip()
      
    })
    output$plot9 <- renderPlot({
      metric_string <- str_to_title(gsub("\\.", " ", input$political_party))
      
      ggplot(df_distinct_republican, aes(x = State, y = republican_deaths, fill = State)) +
        geom_col(show.legend = F) +
        xlab("State") +
        ylab("Deaths") +
        ggtitle("Covid 19 deaths by republican states",'This graph shows the total number of covid-19 deaths in republican states') + 
        plot_theme +
        coord_flip()
      
    })
    output$plot10 <- renderPlot({
      
      ggplot(df_distinct_democrat,aes(x = total_democrat, y = democrat_deaths))+
        geom_smooth(show.legend = F) + 
        ggtitle("Proportion of Democrats",'This graph shows the relationship between covid-19 deaths and how many residents in the state voted democrat') +
        ylab("Deaths") +
        plot_theme
    })   
    output$plot11 <- renderPlot({
      
      ggplot(df_distinct_republican,aes(x = total_republican, y = republican_deaths))+
        geom_smooth(show.legend = F) + 
        ggtitle("Proportion of Republican",'This graph shows the relationship between covid-19 deaths and how many residents in the state voted republican') +
        ylab("Deaths") +
        plot_theme
    })   
    
    output$comment3 <- renderText({
      "Based on this data and a t-test value of -.866 from our linear mixed-effects model there is no evidence that ones political affiliation plays any role
      into how likely they are to die from COVID-19."
    })
    
    
    #---------------------------------- deaths by race ------------------------------
    
      #output$plot11 <- renderPlot({
       #metric_string <- str_to_title(gsub("\\.", " ", input$Race))
       #ggplot(df_distinct_state_race, aes(x = Race, y = race_deaths, fill = Race)) +
         #geom_col() +
         #xlab("Race") +
         #ylab("Deaths") +
         #ggtitle("Covid 19 deaths by race") + 
         #plot_theme
     #})   
     
    
    #----------------------------------- deaths by age ------------------------------
    output$plot12 <- renderPlot({
      #metric_string <- str_to_title(gsub("\\.", " ", input$political_party))
      
      ggplot(df_distinct_age, aes(x = Age.Group, y = death_by_age, fill = Age.Group)) +
        geom_col(show.legend = F) +
        xlab("Age") +
        ylab("Deaths") +
        ggtitle("Covid 19 deaths by age", 'This graph shows the total number of covid deaths within each age group') + 
        plot_theme
      
    })
    output$plot13 <- renderPlot({
      ggplot(df_filter_age(),aes(x = State, y = death_by_age, fill = State))+
        geom_col() + 
        ggtitle(input$age_group, "This reactive graph shows the number of covid deaths in every state for a selected age group") +
        plot_theme +
        coord_flip()
    }) 
    output$plot14 <- renderPlot({
      ggplot(df_filter_condition2(),aes(x = Age.Group, y = COVID.19.Deaths, fill = Age.Group))+
        geom_col(show.legend = F) + 
        ggtitle(input$condition,'This reactive graph shows the number of deaths related to a specific condition for each age group in each state') +
        plot_theme
    }) 
    output$comment4 <- renderText({
      "Based on this data and a t-test value of 5.081 from our linear mixed-effects model there is evidence that age is only an effective indicator of 
      death for someone suffering from Covid-19 if they are over the age of 55."
    })
    #----------------------------------- deaths by population ------------------------------
    output$plot21 <- renderPlot({
      ggplot(df_distinct_pop, aes(x = State, y = TotalPop, fill = State)) +
        geom_col(show.legend = F) +
        xlab("State") +
        ylab("Population") +
        ggtitle("Total population of each state", 'This graph shows the total population for each state') + 
        plot_theme +
        coord_flip()
    })
    output$plot22 <- renderPlot({
      ggplot(df_distinct_pop,aes(x = TotalPop, y = pop_deaths))+
        geom_smooth(show.legend = F) + 
        ggtitle("Deaths by Population Density",'This graph shows the total number of deaths by population density') +
        ylab("Deaths") +
        plot_theme
    })   
    output$comment6 <- renderText({
      "Based on our graphs and a t-test value of 21.733 found with our linear mixed-effects model there is a connection between a states population density and the observed number of covid-19 related deaths"
    })
    #----------------------------------- models ------------------------------
    hierarchical_model_age <- lmer( COVID.19.Deaths ~ Age.Group + (1|State), data = df_distinct_age_condition)
    hierarchical_model_republican <- lmer( COVID.19.Deaths ~ total_republican + (1|State), data = df_joined)
    hierarchical_model_income <- lmer( COVID.19.Deaths ~ Income + (1|State), data = df_joined)
    hierarchical_model_marriage <- lmer( COVID.19.Deaths ~ marriageRate + (1|State), data = df_joined)
    hierarchical_model_pop <- lmer( COVID.19.Deaths ~ TotalPop + (1|State), data = df_joined)
    
    output$plot17 <- renderPrint({
      summary(hierarchical_model_age)
    })
    output$plot18 <- renderPrint({
      summary(hierarchical_model_republican)
    })
    output$plot19 <- renderPrint({
      summary(hierarchical_model_income)
    })
    output$plot20 <- renderPrint({
      summary(hierarchical_model_marriage)
    })
    output$plot23 <- renderPrint({
      summary(hierarchical_model_pop)
    })
    
    output$predictionModel<-DT::renderDataTable(df_distinct_age[,c("State","Age.Group","TotalPop","death_by_age","casualty_predictions")],options = list(pageLength = 10),
                                              callback = JS(
                                                "table.on( 'search.dt', function () {",
                                                "Shiny.setInputValue( 'search', table.search() );",
                                                "} );"))
    
    output$comment7 <- renderText({
      "This model uses both the population density of the state and Age to help predict the number of casualties that will occur from covid-19.  We only use population density and age as our predictors as they were the only socioeconomic factors found to be statistically significant by our linear mixed-effects model. As we can see before the age of 55 our predictions are not accurate, which is exactly what we expect.  After the age of 55 our predictions are much more accurate and can be used to help allocate resources to better handle any covid-19, or similar, outbreaks in the future."
    })

    output$citation1 <- renderPrint({
      "<p>“Linear Mixed-Effects Models in Medical Research” by Kwon et al. (2021) 1 discusses the use of linear mixed-effects models (LMMs) in medical research. The authors provide an overview of LMMs and their use in medical research. They also provide examples of how LMMs can be used to analyze data from medical research studies. LMMs are an extension of linear regression models that describe the relationship between a response variable and independent variables, with coefficients that can vary with respect to one or more grouping variables. They are used when there is non-independence in the data, such as arises from a hierarchical structure. Linear mixed-effects modeling allows a researcher to examine the condition of interest while also taking into account variability within and across participants and items simultaneously. It also handles missing data and unbalanced designs quite well. Linear mixed models have two components: fixed effects and random effects. They accommodate separate variance components modeled with a set of random effects. <br> <p> Kwon, J. M., Kim, J. H., & Kim, K. (2021). Linear Mixed-Effects Models in Medical Research. Anesthesia & Analgesia, 132(6), 1669-1677. https://doi.org/10.1213/ANE.0000000000005415 <br> <p> Pearce, L. J. (2023, May 16). Linear Mixed Effects Models for Medical Data. Bing Chatbot. https://github.com/microsoft/bing-chatbot"
    })
    output$citation2 <- renderPrint({
      "<p>Linear regression is a statistical method that is used to establish a relationship between two variables. In this case, the number of deaths in each state is being predicted based on their age and the population of that state. Linear regression is used because it is a simple and effective way to model the relationship between two variables. The model is based on the assumption that there is a linear relationship between the two variables. The linear mixed-effects model shows that there is a correlation between age and population of a state with someone dying of covid-19. Therefore, linear regression is used to predict the number of deaths in each state based on these variables<sup>[2]</sup>.</p>"
    })

 #-----------------------Idea Backlog--------------------------------------------------   
    output$comment8 <- renderText({
      "1.We planned on creating a leaflet map displaying the covid deaths by zipcode, however we only had lat & lon data 
      for voting by county and making a map for that could not make sense <br>
      2. Tried using Race as a socioeconomic factor, however, we did not have data on the ethinicity of the person who died 
      due to covid.Therefore, we could not use race."
    })
 }) 


