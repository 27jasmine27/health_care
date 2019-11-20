library(shiny)
library(ggplot2)
data <- read.csv("data/drug_induced_deaths_1999-2015.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("Deaths by Drugs in 1999-2015"),
    sidebarLayout(
      sidebarPanel(
        selectizeInput("ï..State", 
                       label = "State:", 
                       choices = data$State))
      
    mainPanel(
      plotOutput("trendPlot"))
    )
)

server <- function(input, output) {
  output$trendPlot <- renderPlot({
    df <- data[data$ï..State == input$State,]
    ggplot(df) +
      geom_point(mapping = aes(x = Year, y = Deaths)),
    xlab = "Year"
    ylab = "Deaths"
  })
}

shinyApp(ui = ui, server = server)