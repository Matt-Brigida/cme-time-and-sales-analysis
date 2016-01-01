## load data ----
octData.c.IV.BS <- read.csv("octDataForNNanalysis.csv", header = TRUE, stringsAsFactors = FALSE)

## Now calculate each of these via NNs --------
## but we cannot tell if the BS of NN Greeks are better! --------

library(nnet)

## We'll use a rolling training periods ----

## training period length
tp <- 500

## shouldn't use the term 'Train' below.  The training set is handled in the loop.  Should just be input variables and response.
## set up inputs
## same answer is add constant rf and div yield
modelInputs <- data.frame(cbind(as.numeric(octData.c.IV.BS$strikePrice_ezOct), as.character(octData.c.IV.BS$typeInd_ezOct), as.numeric(octData.c.IV.BS$avgTradePrice_esOct), as.numeric(octData.c.IV.BS$timeTillExp), as.numeric(octData.c.IV.BS$IVlagged)), stringsAsFactors = FALSE)
names(modelInputs) <- c("Strike", "Type", "ES_Price", "Time", "IVlagged")
modelInputs$Type[modelInputs$Type == "call"] <- 0
modelInputs$Type[modelInputs$Type == "put"] <- 1
## change all columns to numeric
modelInputs <- data.frame(apply(modelInputs, 2, function(x) as.numeric(x)))

## set up response
modelResp <- octData.c.IV.BS$avgTradePrice_ezOct

prediction <- 0
nnPredError <- 0
BSError <- 0
## rolling training and prediction -----
for (i in (tp + 1):(dim(octData.c.IV.BS)[1] - 1)) {
    ## train
    modelInputsMod <- modelInputs[(i - tp):i, ]
    modelRespMod <- modelResp[(i - tp):i]
    trainMod <- nnet(modelInputsMod, modelRespMod, size = 8, linout=T)
    ## predict
    prediction[i + 1] <- predict(trainMod, modelInputs[i + 1, ])[1]

    nnPredError[i + 1] <- abs(prediction - modelResp[i + 1])
    BSError[i + 1] <- abs(octData.c.IV.BS$BS[i + 1] - modelResp[i + 1])

    ## results <- cbind(prediction, nnPredError, BSError)
    ## return(results)
}

## some results
#{{{
results <- data.frame(cbind(prediction[-c(1:(tp + 1))], nnPredError[-c(1:(tp + 1))], BSError[-c(1:(tp + 1))]))
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

## with tp = 500 and 8 nodes
mean(results$nnPredError)
## 15.53731
mean(results$BSError)
## 13.89232


plot(results$nnPredError, results$BSError)
## interesting result seen in above plot.  If one is off the other is accurate for that same option, i.e. if the error is 500 for NN it is 2 for BSM, and vice versa

plot(density(results$nnPredValue))
plot(density(results$BSError))


plot(results$nnPredValue, type = 'l')
plot(results$BSError, type = 'l')
#}}}



###
###  I think I have to separate into two dataframes for NN; one with only calls and one with puts.
###

allData <- cbind(modelInputs, modelResp)
dataCalls <- allData[allData$Type == 0, ]
dataPuts <- allData[allData$Type == 1, ]

inputCalls <- dataCalls[, -6]
inputPuts <- dataPuts[, -6]
respCalls <- dataCalls[, 6]
respPuts <- dataPuts[, 6]

## Calls
tp <- 200
prediction <- 0
nnPredError <- 0
BSError <- 0
## rolling training and prediction -----
for (i in (tp + 1):(dim(octData.c.IV.BS)[1] - 1)){
    ## train
    modelInputsMod <- inputCalls[(i - tp):i, ]
    modelRespMod <- respCalls[(i - tp):i]
    trainMod <- nnet(modelInputsMod, modelRespMod, size=8, linout=T)
    ## predict
    prediction[i + 1] <- predict(trainMod, inputCalls[i+1, ])[1]

    nnPredError[i + 1] <- abs(prediction - respCalls[i+1])
    BSError[i + 1] <- abs(octData.c.IV.BS$BS[i + 1] - respCalls[i + 1])

    ## results <- cbind(prediction, nnPredError, BSError)
    ## return(results)
}

## results: NN wins
mean(nnPredError[205:length(respCalls)])
## 17.98131
mean(BSError[205:length(respCalls)])
## 22.358



### puts

tp <- 200
prediction <- 0
nnPredError <- 0
BSError <- 0
## rolling training and prediction -----
for (i in (tp + 1):(dim(octData.c.IV.BS)[1] - 1)) {
    ## train
    modelInputsMod <- inputPuts[(i - tp):i, ]
    modelRespMod <- respPuts[(i - tp):i]
    trainMod <- nnet(modelInputsMod, modelRespMod, size = 8, linout = T)
    ## predict
    prediction[i + 1] <- predict(trainMod, inputPuts[i + 1, ])[1]

    nnPredError[i + 1] <- abs(prediction - respPuts[i + 1])
    BSError[i + 1] <- abs(octData.c.IV.BS$BS[i + 1] - respPuts[i + 1])

    ## results <- cbind(prediction, nnPredError, BSError)
    ## return(results)
}

## Puts results -- NN wins 
mean(BSError[205:114211])
## 19.39558
mean(nnPredError[205:114211])
## 13.95602


###-------------------------
### using neuralnet from neuralnet package
###-------------------------
library(neuralnet)

tp2 <- 100
prediction2 <- 0
nnPredError2 <- 0
BSError2 <- 0
## rolling training and prediction -----
for (i in (tp2 + 1):(dim(octData.c.IV.BS)[1] - 1)) {
    ## train
    modelInputsMod <- modelInputs[(i - tp2):i, ]
    modelRespMod <- modelResp[(i - tp2):i]

## mofify here, to 
    form.in <- as.formula('optionPrices ~ TWTR + Strike + Time + IVlagged + rf')
    
    mod2 <- neuralnet(form.in, data = datTrain, hidden = 7, algorithm = "rprop+")

    predictionsNeuNet <- compute(mod2, covariate = inputEval)$net.result

    predErrorNeuNet <- abs(predictionsNeuNet - respEval)
## here
    
    trainMod <- nnet(modelInputsMod, modelRespMod, size=8, linout=T)
    ## predict
    prediction[i + 1] <- predict(trainMod, modelInputs[i+1, ])[1]

    nnPredError[i + 1] <- abs(prediction - modelResp[i+1])
    BSError[i + 1] <- abs(octData.c.IV.BS$BS[i + 1] - modelResp[i + 1])

    ## results <- cbind(prediction, nnPredError, BSError)
    ## return(results)
}


#### Next:  Separate into different strike prices and repeast the analysis for each strike.  This takes into account (for BS) the vol smirk.  Note, the NN can already - above - control for the smirk.

