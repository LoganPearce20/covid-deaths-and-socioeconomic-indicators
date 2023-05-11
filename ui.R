library(shiny)
library(ggplot2)
library(shinythemes)
library(shinyjs)
library(viridis)

df_joined <- read.csv("data/df_joined.csv")
df_income <- read.csv("data/df_income.csv")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = shinytheme("cyborg"),
    # Application title
    titlePanel("Effects of Socioeconomic Factors on Covid Related Deaths by State"),

    # Sidebar with inputs
  sidebarLayout(
    sidebarPanel(
      selectizeInput("state",
                     'Choose a State:',
                     choices = distinct(df_joined, df_joined$State)),
      selectizeInput("condition_group",
                     'Choose a Condition Group:',
                     choices = distinct(df_joined, df_joined$Condition.Group)),
      selectizeInput("condition",
                     'Choose a Condition:',
                     choices = distinct(df_joined, df_joined$Condition)),
      selectizeInput("age_group",
                     'Choose an Age Group:',
                     choices = distinct(df_joined, df_joined$Age.Group)),
      selectizeInput("political_party",
                     "Choose a political party:",
                     choices = colnames(select(df_joined, total_republican, total_democrat)),
                     selected = "total_democrat")),

        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(
            # Sidebar with a slider input for number of bins
            tabsetPanel(
              tabPanel("Introduction", 
                       htmlOutput("comment1")),
              tabPanel("Covid Deaths by Income",
                       htmlOutput("comment2"),
                       plotOutput("plot1"), 
                       plotOutput("plot2"),
                       plotOutput('plot3'),
                       plotOutput('plot4'),
                       plotOutput('plot5')),
              tabPanel("Covid Deaths by Marriage Rate"),
              tabPanel("Covid Deaths by Political Affiliation", 
              tabPanel("Covid Deaths by Race",
                       column(8,plotOutput('plot_4', width = '1000px'))),
              tabPanel("Covid Deaths by Age"),
            )
        )
    )
))))

