library(shiny) ; library(tidyverse) ; library(gapminder) ; library(ggplot2)

ui <- fluidPage(
  titlePanel("Gapminder Shiny app"),
  br(),
  sidebarLayout(
    sidebarPanel(
      uiOutput("choose_country"),
      checkboxInput("multiple", label = "or multiple countries", value = FALSE),
      selectInput("response", 
                  label = "Select a variable",
                  choices = list("GDP per Capita" = "gdpPercap", "Life Expectancy" = "lifeExp",  "Population" = "pop"), 
                  selected = "gdpPercap"),
      hr(),
      h4("Output options"),
      checkboxInput("facet", label = "Facet by country", value = FALSE)),
    mainPanel(h4(textOutput("output_country"), align = "center"),
              tabsetPanel(
                tabPanel("Plot",
                         plotOutput("ggplot_gdppc_vs_country")),
                tabPanel("Data", tableOutput("gapminder_table")))))
)

  
server <- function(input, output){
  
  output$choose_country <- renderUI({
    if(input$multiple) {
      title = "Select countries"
      options = list(maxItems = 4)
    } else {
      title = "Select a country"
      options = NULL
    }
    selectizeInput(inputId = "country_from_gapminder", 
                title, 
                multiple = input$multiple, options = options,
                as.list(levels(gapminder$country), selected = levels(gapminder$country)))
  })

  country_data  <- reactive({
    if(is.null(input$country_from_gapminder)) {
      return(NULL)
    }
    subset(gapminder, country %in% input$country_from_gapminder)
  })
  
  output$output_country <- renderText({
    c("Countries selected:", paste(input$country_from_gapminder, collapse = ", "))
  })
  
  output$gapminder_table <- renderTable({ 
    country_data()
  })
  
  output$ggplot_gdppc_vs_country <- renderPlot({
    if(is.null(country_data())) {
      return(NULL)
    }
    
    y <- input$response
    p <- ggplot(country_data(), aes_string(x = "year", y = y)) + 
      scale_x_continuous(breaks = seq(1952, 2007, 5), limits = c(1952, 2007)) +
      theme_classic() + 
      theme(legend.position = "right",
            axis.title.x = element_text(margin = margin(t = 15)),
            axis.title.y = element_text(margin = margin(r = 15)))
    
    if(input$multiple) {
      if(input$facet) {
        p +  geom_line(aes(color = country)) +
          geom_point(aes(color = country)) +
          labs(title = paste(y, "by Year")) +
          facet_wrap(~country)
      } else {
       p + labs(title =paste(y, "by Year"), colour = "") + geom_point(aes(color = country)) + geom_line(aes(color = country))
      }
    } else {
      p + labs(title =paste(y, "by Year for", country_data()$country)) + geom_point() + geom_line()
    }

  })
  
}

shinyApp(ui, server)