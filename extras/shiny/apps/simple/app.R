library(shiny) ; library(tidyverse) ; library(gapminder) ; library(ggplot2)
df <- gapminder %>% group_by(continent,year) %>% summarise(mean_lifeExp = mean(lifeExp))
ui <- fluidPage(
  titlePanel("Gapminder Shiny app"),
  br(),
  sidebarLayout(
    sidebarPanel(width = 3,
                 selectInput(inputId = "continent", label = "Select a continent",
                             choices = levels(df$continent), selected = "Europe")),
                  mainPanel(plotOutput('plot'))
    )
  )
server <- function(input, output){
  data_points <- reactive({filter(df, continent == input$continent)})
  
  output$plot <- renderPlot({
    ggplot(data_points(), aes(x = year, y = mean_lifeExp)) +
      geom_line(color = "deeppink2", lwd = 1.2) +
      scale_x_continuous(breaks = seq(1952, 2007, 5)) +
      scale_y_continuous(breaks = seq(0, 80, 20), limits = c(20, 90)) +
      labs(title = paste("Mean life expectancy in", input$continent),
           x = "\nYear", y = "\nMean life expectancy (years)", caption = "Source: Gapminder") +
      theme_classic() + theme(legend.position = "none")
  })
  }
shinyApp(ui, server)