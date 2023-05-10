library(shiny)
library(ggplot2)

df_joined <- read.csv("data/df_joined.csv")

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
    subset(df_joined, Condition == input$condition)
  })
  df_filter_age <- reactive({
    subset(df_joined, Age.Group == input$age_group)
  })
  df_filter_state <- reactive({
    subset(df_joined, State == input$state)
  })
  
  output$comment1 <- renderText({
    "The project aims to investigate the relationship between the socioeconomic factors of each state 
    and their impact on COVID-19 related deaths in the year 2020. 
    The study will gather data on various socioeconomic indicators such as income, mariage rates, race, political alignment, and age in each state. 
    This data will then be analyzed to identify any correlations between these factors and the number and cause of COVID-19 deaths in each state. 
    The results of this study could potentially provide valuable insights into the social determinants of health and how they influence the spread and impact of infectious diseases like COVID-19. 
    Additionally, the findings could inform public health policies aimed at mitigating the impact of such diseases on vulnerable populations in the future."
  })
    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')

    })
    output$plot_3 <- renderPlot({
      metric_string <- str_to_title(gsub("\\.", " ", input$political_party))
      
      ggplot(df_joined, aes(x = input$political_party, y = COVID.19.Deaths, fill = State)) +
        geom_col() +
        xlab("Voters") +
        ylab("Deaths") +
        ggtitle("Covid 19 deaths by vote") + 
        plot_theme
      
    })
    
    output$plot_4 <- renderPlot({
      #metric_string <- str_to_title(gsub("\\.", " ", input$political_party))
      
      ggplot(df_filter_state, aes(x = total, y = COVID.19.Deaths, fill = State)) +
        geom_col() +
        xlab("Voters") +
        ylab("Deaths") +
        ggtitle("Covid 19 deaths by vote") + 
        plot_theme
    })
      
})

