### Code to analyze the october data -----

octData <- read.csv(file = "octData.csv")

## add IV and Black Scholes prices ----

library(RQuantLib)

