library(data.table)

### Code to analyze the october data -----

octData <- read.csv(file = "octData.csv")
octData <- octData[,-1]

## only rows (seconds) with all data (options and futures)
octData.c <- octData[complete.cases(octData), ]
## set type column to character ----
octData.c <- transform(octData.c, typeInd_ezOct = as.character(typeInd_ezOct))

## convert to data.table for easy mutation -----
octData.c <- data.table(octData.c)

## replace "C" with "call and "P" with "put" (for IV function) ----
octData.c[octData.c == "C"] <- "call"
octData.c[octData.c == "P"] <- "put"

## Add implied vol column ----
IV <- 0
for (i in 1:dim(octData.c)[1]){
    
    IV[i] <- tryCatch(EuropeanOptionImpliedVolatility(octData.c$typeInd_ezOct[i], octData.c$avgTradePrice_ezOct[i], octData.c$avgTradePrice_esOct[i], octData.c$strikePrice_ezOct[i], .02, .01, octData.c$timeTillExp[i], .1),
                      error = function(e) return(NA)
                      )
}

## there are 376 instances where the IV calc failed; extract those into a dataframe

octData.c.IV.failed <- octData.c[is.na(IV), ]
## tried varying the initial vol buy that didn't work ----
## they look like they are failing becasue the numbers are wrong (would need neg IV).  Probabily because the market was moving sat -- so when we averaged over a second to get the ES price this took the average of a wide number.  The option trade may have been at one end of the second.

octData.c.IV <- octData.c[!is.na(IV), ]




## add IV and Black Scholes prices ----

library(RQuantLib)
