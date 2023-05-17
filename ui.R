library(shiny)
library(ggplot2)
library(shinythemes)
library(shinyjs)
library(viridis)
library(dplyr)

df_joined <- read.csv("data/df_joined.csv")
df_income <- read.csv("data/df_income.csv")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = shinytheme("darkly"),
    # Application title
    titlePanel("Effects of Socioeconomic Factors on Covid Related Deaths"),

    # Sidebar with inputs
  sidebarLayout(
    sidebarPanel(
      selectizeInput("state",
                     'Choose a State:',
                     choices = distinct(df_joined, df_joined$State)),
      selectizeInput("condition",
                     'Choose a Condition:',
                     choices = distinct(df_joined, df_joined$Condition)),
      selectizeInput("age_group",
                     'Choose an Age Group:',
                     choices = distinct(df_joined, df_joined$Age.Group))),

        mainPanel(
          fluidRow(
            # Sidebar with a slider input for number of bins
            tabsetPanel(
              tabPanel("Introduction", 
                       htmlOutput("comment1"),
                       htmlOutput('comment6'),
                       htmlOutput('comment7')),
              tabPanel("Overview of Each States Covid Deaths", 
                       plotOutput("plot2"),
                       plotOutput('plot16'),
                       plotOutput('plot3')),
              tabPanel("Covid Deaths by Income",
                       htmlOutput("comment2"),
                       plotOutput("plot1"), 
                       plotOutput('plot4'),
                       plotOutput('plot5'),
                       verbatimTextOutput('plot19')),
              tabPanel("Covid Deaths by Marriage Rate",
                       htmlOutput("comment5"),
                       plotOutput('plot6'),
                       plotOutput('plot7'),
                       plotOutput('plot15'),
                       verbatimTextOutput('plot20')),
              tabPanel("Covid Deaths by Political Affiliation", 
                       htmlOutput("comment3"),
                       plotOutput('plot8'),
                       plotOutput('plot9'),
                       plotOutput('plot10'),
                       plotOutput('plot11'),
                       verbatimTextOutput('plot18')),
              tabPanel("Covid Deaths by Age",
                       htmlOutput("comment4"),
                       plotOutput('plot12'),
                       plotOutput('plot13'),
                       plotOutput('plot14'),
                       verbatimTextOutput('plot17')),
              tabPanel("BingAi Model Citations",
                       htmlOutput('citation1'),
                       htmlOutput('citation2'),
                       htmlOutput('citation3')),
              tabPanel("Idea Backlog")
                       
            )
        )
    )
)))

