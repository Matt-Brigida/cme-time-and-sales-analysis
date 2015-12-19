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

## Now calculate each of these via NNs --------
## but we cannot tell if the BS of NN Greeks are better! --------

library(nnet)

## We'll use a rolling training periods ----

## training period length
tp <- 100

## shouldn't use the term 'Train' below.  The training set is handled in the loop.  Should just be input variables and response.
## set up inputs
## same answer is add constant rf and div yield
inputTrain <- data.frame(cbind(as.numeric(octData.c.IV.BS$strikePrice_ezOct), as.character(octData.c.IV.BS$typeInd_ezOct), as.numeric(octData.c.IV.BS$avgTradePrice_esOct), as.numeric(octData.c.IV.BS$timeTillExp), as.numeric(octData.c.IV.BS$IVlagged)), stringsAsFactors = FALSE)
names(inputTrain) <- c("Strike", "Type", "ES_Price", "Time", "IVlagged")
inputTrain$Type[inputTrain$Type == "call"] <- 0
inputTrain$Type[inputTrain$Type == "put"] <- 1
## change all columns to numeric
inputTrain <- data.frame(apply(inputTrain, 2, function(x) as.numeric(x)))

## set up response
respTrain <- octData.c.IV.BS$avgTradePrice_ezOct

prediction <- 0
nnPredError <- 0
BSError <- 0
## rolling training and prediction -----
for (i in (tp + 1):(dim(octData.c.IV.BS)[1] - 1)){
    ## train
    inputTrainMod <- inputTrain[(i - tp):i, ]
    respTrainMod <- respTrain[(i - tp):i]
    trainMod <- nnet(inputTrainMod, respTrainMod, size=10, linout=T)
    ## predict
    prediction[i + 1] <- predict(trainMod, inputTrain[i+1, ]) 

    nnPredError[i + 1] <- abs(prediction - respTrain[i+1])
    BSError[i + 1] <- abs(octData.c.IV.BS$BS[i + 1] - respTrain[i + 1])

    ## results <- cbind(prediction, nnPredError, BSError)
    ## return(results)
}

results <- data.frame(cbind(prediction[-c(1:(tp+1))], nnPredError[-c(1:(tp+1))], BSError[-c(1:(tp+1))]))
names(results) <- c("nnPredValue", "nnPredError", "BSError")

## with tp = 200
mean(results$nnPredError)
## 15.54689
mean(results$BSError)
## 13.88409
### BS wins

## with tp = 100
mean(results$nnPredError)
## 15.54989
sd(results$nnPredError)
## 18.78325
mean(results$BSError)
## 13.88242
sd(results$BSError)
## 36.10133
### BS wins on mean, but has a higher variability of its error (is is way off sometimes)

plot(results$nnPredError, results$BSError)
## interesting result seen in above plot.  If one is off the other is accurate for that same option, i.e. if the error is 500 for NN it is 2 for BSM, and vice versa

plot(density(results$nnPredValue))
plot(density(results$BSError))


plot(results$nnPredValue, type = 'l')
plot(results$BSError, type = 'l')


#### Next:  Separate into different strike prices and repeast the analysis for each strike.  This takes into account (for BS) the vol smirk.  Note, the NN can already - above - control for the smirk.

