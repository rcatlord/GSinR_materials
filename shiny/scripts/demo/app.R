library(shiny) ; library(tidyverse) ; library(forcats); library(gapminder) ; library(ggplot2)

df <- gapminder %>% 
  gather(variable, value, pop, lifeExp, gdpPercap) %>% 
  mutate(variable = factor(variable),
         variable = fct_recode(variable, "GDP per capita ($)" = "gdpPercap",
                               "Life expectancy" = "lifeExp",
                               "Population" = "pop"))
  
ui <- fluidPage(
  titlePanel("Gapminder Shiny app"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 selectInput(inputId = "country",
                             label = "Select a country",
                             choices = levels(df$country),
                             selected = "Sweden"),
                 br(),
                 radioButtons(inputId = "variable", 
                              label = "Select a variable",
                              choices = levels(df$variable),
                              selected = "Life expectancy")
                 ),
                  mainPanel(
                    plotOutput('plot')
                    )
    )
  )

server <- function(input, output){
  data_points <- reactive({filter(df, country == input$country & variable == input$variable)})
  
  output$plot <- renderPlot({
    if(is.null(input$country)) {
      return(NULL)
      }
    ggplot(data_points(), aes(x = year, y = value)) +
      geom_line(color = "deeppink2", lwd = 1.2) +
      scale_x_continuous(breaks = seq(1952, 2007, 5)) +
      labs(title = paste(input$variable, "in", input$country),
           x = "Year", y = input$variable, caption = "Source: Gapminder") +
      theme_classic() + 
      theme(legend.position = "none")
  })
  }

shinyApp(ui, server)