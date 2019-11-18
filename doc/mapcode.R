library(dplyr)
library(ggplot2)
library(leaflet)

data <- read.csv(
  'Statistically significant changes in drug overdose death rates involving heroin by select states, United States, 2016 to 2017.csv', 
  stringsAsFactors = FALSE, na.strings = 'n/a'
)

data <- data[, 1:8]

states <- geojsonio::geojson_read("us-states.geojson", what = "sp")

detail <- merge(states@data, data, by.x = 'name', by.y = 'state', all.x = TRUE)

detail[is.na(detail$category), ]$category <- 'did not meet inclusion criteria'
detail <- detail[order(detail$id), ]

states@data <- detail

pal <- colorFactor(
  palette = "RdGy", domain = states$category, 
  levels = c(
    "increase", "stable - not significant", "decrease", 
    "did not meet inclusion criteria"
  )
)

labels <- sprintf(
  "<strong>%s</strong><br/>
  Category: %s<br/>
  Percent Change from 2016-2017: %s<br/>
  Statistically Significant: %s<br/>
  2016 Number: %s<br/>
  2016 Rate: %s<br/>
  2017 Number: %s<br/>
  2017 Rate: %s",
  states$name, states$category, states$change, states$significant, 
  states$X2016number, states$X2016rate, states$X2017number, states$X2017rate
) %>% lapply(htmltools::HTML)

my_map <- leaflet(states) %>%
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
    position = "bottomright")
