library(tidyverse)
library(dplyr)
library(magrittr)
 
df_covid_data <- read.csv("data/Conditions_Contributing_to_COVID-19_Deaths__by_State_and_Age__Provisional_2020-2023.csv")
df_income <- read.csv("data/income_by_state.csv")
df_marital_status <- read.csv("data/marital_status.csv")

df_covid_data <- df_covid_data %>%
  filter(Year == "2020") %>%
  select(Year, State, Condition.Group, Condition, Age.Group, COVID.19.Deaths, Number.of.Mentions, )
  
df_income <- df_income %>%
  filter(DATE == "1/1/20")

df_marital_status <- df_marital_status %>%
  filter(YEAR == "2020")
