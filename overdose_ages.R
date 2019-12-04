library(dplyr)
library(plotly)

overdose_age_groups2017 <- read.csv(
  "data/overdose_age_groups_state2017.csv",
  stringsAsFactors = FALSE
)
overdose_age_groups2016 <- read.csv(
  "data/overdose_age_groups_state2016.csv",
  stringsAsFactors = FALSE
)
overdose_age_groups2015 <- read.csv(
  "data/overdose_age_groups_state2015.csv",
  stringsAsFactors = FALSE
)

# formats the data table given by changing column names and looking
# at all states
format_df <- function(df) {
  colnames(df) <- c(
    "Location",
    "Ages 0-24",
    "Ages 25-34",
    "Ages 35-44",
    "Ages 45-54",
    "Ages 55+",
    "Total"
  )
  filter(df, Location == "United States") %>%
    select(-Total, -Location)
}

overdose_age_groups2017 <- format_df(overdose_age_groups2017)
overdose_age_groups2016 <- format_df(overdose_age_groups2016)
overdose_age_groups2015 <- format_df(overdose_age_groups2015)

plot_ages <- function(df) {
  plot_ly(
    df,
    x = ~colnames(df),
    y = ~as.numeric(as.vector(df[1,])),
    type = "bar"
  ) %>%
     layout(
       xaxis = list(title = "Age Groups"),
       yaxis = list(title = "Number of Opiod Overdoses"),
       title = "Opiod Overdoses in the US by age group"
      )
}

