library(shiny)
library(shinythemes)
library(ggplot2)

source("overdose_map.R")
source("death_mapcode.R")

homepage <- tabPanel(
    "Overview"
)

page_one <- tabPanel(
    "First Page"
)

page_two <- tabPanel(
    "Second Page",
    titlePanel("Overdose Map"),
    sidebarLayout(
        sidebarPanel(
            radioButtons(inputId = "age", label = h3("Age Range"),
                         choices = list("Ages 0-24",
                                        "Ages 25-34",
                                        "Ages 35-44",
                                        "Ages 45-54",
                                        "Ages 55+",
                                        "Total"), 
                         selected = "Total")
        ),
        mainPanel(
            plotlyOutput("overdose_map")
        )
    )
)

page_three <- tabPanel(
    "Third Page",
    titlePanel("Heroin-Related Overdose Deaths"),
    sidebarLayout(
        sidebarPanel(
        selectInput("category", "Category:",
                c("stable - not significant" = "stable - not significant",
                  "increase" = "increase",
                  "did not meet inclusion criteria" = "did not meet inclusion criteria",
                  "decrease" = "decrease")
                ),
        hr(),
        h5("This is a static text")
        ),
    mainPanel(
    leafletOutput("mymap"))
    )
)

page_four <- navbarMenu("More",
                        tabPanel("Q&A"),
                        tabPanel("Contact Us",
                        br(),
                        p("INFO 201 | Autumn 2019"),
                        hr(),
                        p("Adriane Phi, 
                          Christian Diangco, 
                          Jasmine Kennedy, 
                          Jiaxian Xiang", 
                          align = "center"),
                        p("Link to ", a(strong(code("INFO201-Final-Project")), 
                                        href = "https://github.com/Jessjx6/health_care"), 
                          align = "center")
                        )
)

ui <- navbarPage(
    theme = shinytheme("yeti"),
    "Drug Usage",
    homepage,
    page_one,
    page_two,
    page_three,
    page_four
)

server <- function(input, output) {
    
    output$overdose_map <- renderPlotly({
        plot_geo(data = overdose_age_groups) %>%
            add_trace(
                z = ~overdose_age_groups[[input$age]],
                locations = ~Location,
                locationmode = "USA-states",
                color = ~Total
            ) %>%
            colorbar(title = "Overdoses") %>%
            layout(
                geo = list(scope = "usa"),
                title = paste0("Opiod Overdoses in 2017 by State (", input$age, ")"),
                annotations = list(
                    text = "*White states do not have sufficient data for this age group",
                    x = 1.1,
                    y = -0.1,
                    showarrow = FALSE
                )
            )
    })
    
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
            label = labels,
            labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
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