
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
  2. Find the total proportion of each political party in each State
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
  3. Changed the names of variables 
    names(df_marital_status)[names(df_marital_status) == "STATE"] <- "State"
    names(df_marital_status)[names(df_marital_status) == "RATE"] <- "marriageRate"  
## Graphs 📈
All our graphs were made using the ggplot function.

- Income per State Graph 

        output$plot1 <- renderPlot({
            ggplot(df_joined, aes(x = State, y = Income, fill = State)) +
            geom_col(show.legend = F, position = "dodge", stat = "identity") +
            labs(title = "Income per State",
                x = "State",
                y = "Income") +
            plot_theme +
            coord_flip() 

    - Covid Death by Age

            gplot(df_distinct_age, aes(x = Age.Group, y = death_by_age, fill = Age.Group)) +
            geom_col(show.legend = F) +
            xlab("Age") +
            ylab("Deaths") +
            ggtitle("Covid 19 deaths by age") + 
            plot_theme
## Modelling🔋
