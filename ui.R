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
  theme = shinytheme("cyborg"),
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
                     choices = distinct(df_joined, df_joined$Age.Group)),
      checkboxInput("show_output1", "Income Conclusion"),
      conditionalPanel(
        condition = "input.show_output1 == true",
        htmlOutput('comment2')),
      checkboxInput("show_output2", "Marriage Rate Conclusion"),
      conditionalPanel(
        condition = "input.show_output2 == true",
        htmlOutput("comment5")),
      checkboxInput("show_output3", "Political Affiliation Conclusion"),
      conditionalPanel(
        condition = "input.show_output3 == true",
        htmlOutput("comment3")),
      checkboxInput("show_output4", "Age Conclusion"),
      conditionalPanel(
        condition = "input.show_output4 == true",
        htmlOutput("comment4")),
      checkboxInput("show_output5", "Population Conclusion"),
      conditionalPanel(
        condition = "input.show_output5 == true",
        htmlOutput("comment6")),
      checkboxInput("show_output6", "Prediction Model Explanation"),
      conditionalPanel(
        condition = "input.show_output6 == true",
        htmlOutput("comment7"))),

        mainPanel(
          fluidRow(
            # Sidebar with a slider input for number of bins
            tabsetPanel(
              tabPanel("Introduction", 
                       htmlOutput("comment1")),
              tabPanel("Overview of Each States Covid Deaths", 
                       plotOutput("plot2"),
                       plotOutput('plot16'),
                       plotOutput('plot3')),
              tabPanel("Covid Deaths by Income",
                       plotOutput("plot1"), 
                       plotOutput('plot4'),
                       plotOutput('plot5'),
                       verbatimTextOutput('plot19')),
              tabPanel("Covid Deaths by Marriage Rate",
                       plotOutput('plot6'),
                       plotOutput('plot7'),
                       plotOutput('plot15'),
                       verbatimTextOutput('plot20')),
              tabPanel("Covid Deaths by Political Affiliation", 
                       plotOutput('plot8'),
                       plotOutput('plot9'),
                       plotOutput('plot10'),
                       plotOutput('plot11'),
                       verbatimTextOutput('plot18')),
              tabPanel("Covid Deaths by Age",
                       plotOutput('plot12'),
                       plotOutput('plot13'),
                       plotOutput('plot14'),
                       verbatimTextOutput('plot17')),
              tabPanel("Covid Deaths by Population",
                       plotOutput('plot21'),
                       plotOutput('plot22'),
                       verbatimTextOutput('plot23')),
              tabPanel('Prediction Model',
                       dataTableOutput('predictionModel'),
                       htmlOutput('citation2')),
              tabPanel("Linear mixed-effects model citation",
                       htmlOutput('citation1')),
              tabPanel("Idea Backlog",
                       htmlOutput("comment8"))

                       
            )
        )
    )
)))

