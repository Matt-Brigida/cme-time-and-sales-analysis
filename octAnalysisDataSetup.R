library(data.table)
library(RQuantLib)

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

octData.c.IV <- cbind(octData.c[!is.na(IV), ][-1,], IV[!is.na(IV)][-length(IV[!is.na(IV)])])
names(octData.c.IV)[10] <- "IVlagged"

## for each row, calculate the BS option value, then ready for neural network.
BS <- 0
Delta <- 0
Gamma <- 0
Vega <- 0
Theta <- 0
Rho <- 0
## DivRho <- 0
for (i in 1:dim(octData.c.IV)[1]){
    ## the function EuropeanOption will return the value, but also the greeks; we should store the greeks here
    tmp <- EuropeanOption(octData.c.IV$typeInd_ezOct[i], octData.c.IV$avgTradePrice_esOct[i], octData.c.IV$strikePrice_ezOct[i], 0.02, 0.01, octData.c.IV$timeTillExp[i], octData.c.IV$IVlagged[i])
    BS[i] <- tmp$value
    Delta[i] <- tmp$delta
    Gamma[i] <- tmp$gamma
    Vega[i] <- tmp$vega
    Theta[i] <- tmp$theta
    Rho[i] <- tmp$rho
    ## DivRho[i] <- tmp$dividendRho
}

## add IV and Black Scholes prices ----

octData.c.IV.BS <- cbind(octData.c.IV, BS, Delta, Gamma, Vega, Theta, Rho)

