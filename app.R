library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
source("overdose_map.R")
source("death_mapcode.R")
source("overdose_ages.R")

deaths_vs_time <- read.csv(
    "data/drug_induced_deaths_1999-2015.csv", 
    stringsAsFactors = FALSE
)
colnames(deaths_vs_time)[1] = "state"

filtered_data <- filter(deaths_vs_time, state == "Washington")
plot_ly(filtered_data,
        x = ~Year,
        y = ~Deaths,
        type = "scatter")

graph_state <- function(state_name = "Washington", data) {
    filtered_data <- filter(data, state == state_name)
    plot_graph <- plot_ly(
        filtered_data,
        x = ~Year, y = ~Deaths,
        type = "scatter",
        mode = "lines+markers"
    )
    return(plot_graph)
}

homepage <- tabPanel(
    "Overview",
    mainPanel(
        h3(class = "title", "Overview"),
        p("Our Shiny application was built in R Studio and contains 
        the major components used to create the variety of visualizations 
        used to help us answer our research questions."),
        p("To begin, we used a heat map to show the extent of heroin overdoses 
        in different states, with color to show the amount of people, frequency, 
        and age. This is particularly useful because it gives a visual aid as 
        to where and what groups of people are in most need of help."),
        p("Secondly, the next visualization we used was a bar chart. 
        This varied from the heat map because it highlights the various ages 
        of heroin users across the nation. Being able to change the view based 
        on specific age groups allows the user to see the true and unobscured data,
        whereas the heat map shows the depth and severity of the problem. "),
        p("Lastly, the other map that we used highlights the change in deaths from 
          2016 until now. This is useful because it highlights the severity and 
          growing issue that needs attention and changes to legislation. ")
    )
)


page_one <- tabPanel(
    "Overdose Map",
    sidebarLayout(
        sidebarPanel(
            radioButtons(inputId = "age", label = h3("Age Range"),
                         choices = list("Ages 0-24",
                                        "Ages 25-34",
                                        "Ages 35-44",
                                        "Ages 45-54",
                                        "Ages 55+",
                                        "Total"), 
                         selected = "Total"),
            "*Gray states have no sufficient data for the specified age group"
        ),
        mainPanel(
            plotlyOutput("overdose_map")
        )
    )
)

page_four <- tabPanel(
    "Deaths Over Time",
    titlePanel("Deaths by Drugs in 1999-2015"),
    sidebarLayout(
        sidebarPanel(
            selectInput("state", 
                        label = "State:", 
                        choices = state.name
            )
        ),
        mainPanel(
            plotlyOutput("trendPlot"))
    )
)

page_three <- tabPanel(
    "Death Rate Change Map",
    titlePanel("Change in Heroin-Related Death Rates 2016-2017"),
    sidebarLayout(
        sidebarPanel(
            selectInput("category", "Category:",
                        c("stable - not significant" = "stable - not significant",
                          "increase" = "increase",
                          "did not meet inclusion criteria" = "did not meet inclusion criteria",
                          "decrease" = "decrease")
            ),
            hr(),
            p("By clicking on the map, we can see the death rate in 2016 and 2017, and the change 
           rate. "), 
            p("By chosing different catagory, the color of certain regions will change and 
           display the seriousness of the heroin overdose accorss the US. "),
            hr(),
            p("From this map, we are able to know the areas are most in need of help.")
        ),
        mainPanel(
            leafletOutput("mymap"))
    )
)

page_two <- tabPanel(
    "Overdoses by Age Group",
    sidebarLayout(
        sidebarPanel(
            radioButtons("year", "Year", choices = c(2017, 2016, 2015)),
            "*2016 has no sufficient data for certain age groups"
        ),
        mainPanel(
            plotlyOutput("ages_bar")
        )
    )
)

page_five <- tabPanel(
    "Conclusion"
)

page_six <- tabPanel(
    "Contact Us",
    br(),
    p("INFO 201 | Autumn 2019"),
    hr(),
    p(
        "Adriane Phi,
        Christian Diangco,
        Jasmine Kennedy,
         Jiaxian Xiang",
        align = "center"
    ),
     p("Link to ", a(strong(code("INFO201-Final-Project")),
        href = "https://github.com/Jessjx6/health_care"),
        align = "center"
     )
)

ui <- navbarPage(
    theme = shinytheme("yeti"),
    "Heroin Usage",
    homepage,
    page_one,
    page_two,
    page_three,
    page_four,
    page_five,
    page_six
)

server <- function(input, output) {
    output$trendPlot <- renderPlotly(graph_state(input$state, deaths_vs_time))
    
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
                title = paste0("Opiod Overdoses in 2017 by State (", input$age, ")")
            )
    })
    
    # death rate change map
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
    
    # overdose by age group bar chart
    output$ages_bar <- renderPlotly({
        ages_df_year <- paste0("overdose_age_groups", input$year)
        ages_df <- eval(parse(text = ages_df_year)) %>%
            plot_ages()
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
