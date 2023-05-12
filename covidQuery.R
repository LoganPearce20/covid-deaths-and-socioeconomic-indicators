library(tidyverse)
library(dplyr)
library(magrittr)
library(ggplot2)
library(shiny)
 
df_covid_data <- read.csv("data/Conditions_Contributing_to_COVID-19_Deaths__by_State_and_Age__Provisional_2020-2023.csv")
df_income <- read.csv("data/income_by_state.csv")
df_marital_status <- read.csv("data/marital_status.csv")
df_voting_tendancy <- read.csv("data/county_statistics.csv")

df_covid_data <- df_covid_data %>%
  filter(Year == "2020") %>%
  group_by(State) %>%
  mutate(total_deaths = sum(COVID.19.Deaths, na.rm = T)) %>%
  select(Year, State, Condition.Group, Condition, Age.Group, COVID.19.Deaths, Number.of.Mentions, total_deaths, End.Date)
  
df_income <- df_income %>%
  filter(DATE == "1/1/20") %>%
  pivot_longer(cols = -DATE, names_to = "State", values_to = "Income")

df_marital_status <- df_marital_status %>%
  filter(YEAR == "2020") 

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

names(df_marital_status)[names(df_marital_status) == "STATE"] <- "State"
names(df_marital_status)[names(df_marital_status) == "RATE"] <- "marriageRate"
names(df_voting_tendancy)[names(df_voting_tendancy) == "state"] <- "State"

# df_voting <- df_voting_tendancy %>%
#   pivot_longer(cols = -c(State, total_republican, total_democrat, Pacific), names_to = "Race", values_to = "total")

df_joined <- inner_join(df_covid_data, df_income, by = "State") %>%
  inner_join(df_marital_status, by = "State") %>%
  inner_join(df_voting_tendancy, by = "State") %>%
  select(-DATE, -YEAR, -URL, -Year)

write_csv(df_joined, "data/df_joined.csv")
write_csv(df_income, "data/df_income.csv")





