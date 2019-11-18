library(shiny)
library(shinythemes)

source("20191117_D.R")

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
        setView(-96, 37.8, 4) %>%
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
            label = labels,
            labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
        addLegend(
            pal = pal,
            values = ~category, opacity = 0.7, title = NULL,
            position = "bottomright")})
    
}


# Run the application 
shinyApp(ui = ui, server = server)