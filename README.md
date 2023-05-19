
# Covid Deaths 🦠 and Socioeconomic Indicators 💵
In this project we have collected data on the number of deaths due to covid 19 throughout all 50 states and various Socioeconomic factors such as average income, marital status, political voting preferance, and deaths due to other diseases in every state. We aim to see the trends and correlation between each Socioeconomic factor and how it affects the number of covid deaths. 

## ShinyApp URL
A working version of this app has been published and hosted through Shinyapps.io, and is accessible via the link below.
https://loganpearce20.shinyapps.io/covid-deaths-and-socioeconomic-indicators/

## R script Directory
* Data cleaning and Dataframe creation ~ covidQuery.R
* creation of UI and models for predictions ~ ui.R
* creation of server and visualizations ~ server.R

## Library 📚
```r
    library(tidyverse)
    library(dplyr)
    library(magrittr)
    library(ggplot2)
    library(shiny)
    library(viridis)
    library(lme4)
    library(shinythemes)
 ```
## Data Cleaning 🧼
1. Filter the year to be '2020'
```r
   df_covid_data <- df_covid_data %>%
        filter(Year == "2020") %>%
        group_by(State) %>%
        mutate(total_deaths = sum(COVID.19.Deaths, na.rm = T))
  ```
  2. Find the total proportion of each political party in each State
  ```r    
    df_voting_tendancy <- df_voting_tendancy %>%
            select(state, percentage20_Donald_Trump, percentage20_Joe_Biden, total_votes20, votes20_Donald_Trump, 
                votes20_Joe_Biden, TotalPop, Hispanic, White, Black, Asian, Native, Pacific) %>%
            arrange(state) %>%
            group_by(state) %>%
            mutate(total_votes_by_state = sum(total_votes20, na.rm = T)) %>%
            mutate(total_republican = sum(votes20_Donald_Trump, na.rm = T) / total_votes_by_state) %>%
            mutate(total_democrat = sum(votes20_Joe_Biden, na.rm = T) / total_votes_by_state) %>%
            mutate(White = round((White / 100) * TotalPop)) %>%
            mutate(Black = round((Black / 100) * TotalPop)) %>%
            mutate(Hispanic = round((Hispanic / 100) * TotalPop)) %>%
            mutate(Asian = round((Asian / 100) * TotalPop)) %>%
            mutate(Native = round((Native / 100) * TotalPop)) %>%
            mutate(Pacific = round((Pacific / 100) * TotalPop)) %>%
            mutate(TotalPop = sum(TotalPop, na.rm = T)) %>%
            mutate(White = sum(White, na.rm = T)) %>%
            mutate(Black = sum(Black, na.rm = T)) %>%
            mutate(Hispanic = sum(Hispanic, na.rm = T)) %>%
            mutate(Asian = sum(Asian, na.rm = T)) %>%
            mutate(Native = sum(Native, na.rm = T)) %>%
            mutate(Pacific = sum(Pacific, na.rm = T)) %>%
            distinct(total_republican, total_democrat, White, Black, Hispanic, Asian, Native, Pacific)
  ```      
  3. Changed the names of variables 
  ```r          
            names(df_marital_status)[names(df_marital_status) == "STATE"] <- "State"
            names(df_marital_status)[names(df_marital_status) == "RATE"] <- "marriageRate"  
```
## Graphs 📈
All our graphs were made using the ggplot function.

- Set theme for all graphs
```r
plot_theme <- ggdark::dark_theme_gray(base_size = 14) + 
  theme(plot.title = element_text(),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank())
```
- Create reactives for our graphs to use
```r
 df_filter_condition_group <- reactive({
    subset(df_joined, Condition.Group == input$condition_group)
  })
 ```

- Bar Graph 
```r
  output$plot1 <- renderPlot({
    ggplot(df_joined, aes(x = State, y = Income, fill = State)) +
      geom_col(show.legend = F, position = "dodge", stat = "identity") +
      ggtitle('Income per state', "This graph shows you the average income for every state in 2020") +
      xlab('State') +
      ylab('Income') +
      plot_theme +
      coord_flip() 
```
   - Line Graph
```r
    output$plot4 <- renderPlot({
      ggplot(df_distinct_income_deaths, aes(x = Income, y = income_deaths)) +
        geom_smooth() +
        xlab("Income") +
        ylab("Deaths") +
        ggtitle("Covid 19 deaths by Income", "This graph shows the relationship between income and the total number of deaths due to covid-19") + 
        plot_theme
    })
```
## Modeling🔋
1. Linear mixed-effects model
* Used to find what socioeconomic factors are statistically significant in predicting a covid-19 death
```r
hierarchical_model_age <- lmer( COVID.19.Deaths ~ Age.Group + (1|State), data = df_distinct_age_condition)

output$plot17 <- renderPrint({
      summary(hierarchical_model_age)
    })
```
2. Linear regression prediction model
* Based on the findings of our linear mixed-effects model we use state, age, and population density to predict covid-19 deaths 
```r
model <- lm(death_by_age ~  Age.Group + State + TotalPop,  data = df_distinct_age)

df_distinct_age$casualty_predictions <-  predict(model, newdata = df_distinct_age)

df_distinct_age$casualty_predictions <- ceiling(df_distinct_age$casualty_predictions)

df_distinct_age$casualty_predictions[df_distinct_age$casualty_predictions < 0] <- 0

output$predictionModel<-         DT::renderDataTable(df_distinct_age[,c("State","Age.Group","TotalPop","death_by_age","casualty_predictions")],options = list(pageLength = 10),
                                              callback = JS(
                                                "table.on( 'search.dt', function () {",
                                                "Shiny.setInputValue( 'search', table.search() );",
                                                "} );"))
```
