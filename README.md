
# Covid Deaths 🦠 and Socioeconomic Indicators 💵
In this project we have collected data on the number of deaths due to covid 19 throughout all 50 states and various Socioeconomic factors such as average income, marital status, political voting preferance, and deaths due to other diseases in every state. We aim to see the trends and correlation between each Socioeconomic factor and how it affects the number of covid deaths. 


## Library 📚
    library(tidyverse)
    library(dplyr)
    library(magrittr)
    library(ggplot2)
    library(shiny)
    library(viridis)
    library(lme4)
    library(shinythemes)
## Data Cleaning 🧼
1. Filter the year to be '2020' 

        df_covid_data <- df_covid_data %>%
        filter(Year == "2020") %>%
        group_by(State) %>%
        mutate(total_deaths = sum(COVID.19.Deaths, na.rm = T))
    
## Graphs 📈
All our graphs were made using the ggplot function. 

    output$plot1 <- renderPlot({
        ggplot(df_joined, aes(x = State, y = Income, fill = State)) +
        geom_col(show.legend = F, position = "dodge", stat = "identity") +
        labs(title = "Income per State",
            x = "State",
            y = "Income") +
        plot_theme +
        coord_flip() 
## Modelling🔋
