library(dplyr)
library(plotly)
library(openintro)

overdose_age_groups <- read.csv(
  "data/overdose_age_groups_state.csv",
  stringsAsFactors = FALSE
)
colnames(overdose_age_groups) <- c(
  "Location",
  "Ages 0-24",
  "Ages 25-34",
  "Ages 35-44",
  "Ages 45-54",
  "Ages 55+",
  "Total"
)
overdose_age_groups[overdose_age_groups == "NSD"] <- "No sufficient data"
overdose_age_groups <- filter(overdose_age_groups, Location != "United States")
state_codes <- lapply(overdose_age_groups$Location, state2abbr)
overdose_age_groups$Location <- state_codes

# map code test
# plot_geo(data = overdose_age_groups) %>%
#   add_trace(
#     z = ~overdose_age_groups[["Ages 55+"]],
#     locations = ~Location,
#     locationmode = "USA-states",
#     color = ~Total
#   ) %>%
#   colorbar(title = "Overdoses") %>%
#   layout(
#     geo = list(scope = "usa"),
#     title = paste0("Opiod Overdoses in 2017 by State (", "Ages 55+", ")")
#   )
