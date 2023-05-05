library(tidyverse)
library(dplyr)
library(magrittr)
 
df_covid_data <- read.csv("data/Conditions_Contributing_to_COVID-19_Deaths__by_State_and_Age__Provisional_2020-2023.csv")
df_income <- read.csv("data/income_by_state.csv")
df_marital_status <- read.csv("data/marital_status.csv")

