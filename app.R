library(shiny)
library(shinythemes)

source("mapcode.R")

page_one <- tabPanel(
    "First Page", # label for the tab in the navbar
    titlePanel("Page 1"), # show with a displayed title
)

page_two <- tabPanel(
    "Second Page"
)

page_three <- tabPanel(
    "Third Page",
    titlePanel("Heroin-Related Overdose Deaths"),
    selectInput("category", "Category:",
                c("decrease" = "decrease",
                  "did not meet inclusion criteria" = "did not meet inclusion criteria",
                  "increase" = "increase",
                  "stable - not significant" = "stable - not significant"
                )),
    leafletOutput("mymap")
)

page_four <- navbarMenu("More",
                        tabPanel("Q&A"),
                        tabPanel("Contact Us")
)

ui <- navbarPage(
    theme = shinytheme("yeti"),
    "Drug Usage", 
    page_one,
    page_two,
    page_three,
    page_four
)

server <- function(input, output) {
    output$mymap <- renderLeaflet({my_map <- leaflet(states) %>%
        setView(-100, 40, 4) %>%
        addProviderTiles("MapBox", options = providerTileOptions(
            id = "mapbox.light",
            accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
        addPolygons(
            fillColor = ~pal(category), 
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
            label = labels) %>%
        addLegend(
            pal = pal,
            values = ~category, opacity = 0.7, title = NULL,
            position = "bottomright")})
    
    proxy <- leafletProxy("mymap")
    
    observe({
        if(input$category != ""){
            selected_polygon <- subset(states, states$category==input$category)
            proxy %>% clearGroup("highlighted_polygon")
            proxy %>% addPolylines(stroke=TRUE, weight = 4, color="red",
                                   data=selected_polygon, group="highlighted_polygon")
        }
    })
}


# Run the application 
shinyApp(ui = ui, server = server)