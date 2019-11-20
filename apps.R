library(shiny)
library(ggplot2)
data1 <- read.csv("data/drug_induced_deaths_1999-2015.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("Deaths by Drugs in 1999-2015"),
    sidebarLayout(
      sidebarPanel(
        selectizeInput("?..State", 
                       label = "State:", 
                       choices = data1$State)),
      
    mainPanel(
      plotOutput("trendPlot"))
    )
)

server <- function(input, output) {
  output$trendPlot <- renderPlot({
    #df <- data1[data1$State == input$State, ]
    trendPlot <- ggplot(data1) +
      geom_point(mapping = aes(x = Year, y = Deaths)) +
    labs(x = "Year", y = "Ideology")
  })
}

shinyApp(ui = ui, server = server)