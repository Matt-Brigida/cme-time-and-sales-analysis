
library(data.table)

## Start looking at data ----

## data layout guide: http://www.cmegroup.com/confluence/display/EPICSANDBOX/Time+and+Sales+CSV+Layout

names <- c("tradeDate", "tradeTime", "tradeSeqNum", "sessionInd", "tickerSymb", "typeInd", "deliveryDate", "tradeQuantity", "strikePrice", "tradePrice", "askBidType", "indicativeQuoteType", "marketQuote", "closeOpenType", "validOpenException", "postClose", "cancelCodeType", "insertCodeType", "fastLateInd", "cabinetInd" , "bookInd", "entryDate", "exchangeCode")

## October Futures ----

esOct <- read.csv('CME_TICK_ES_201510.csv', stringsAsFactors = FALSE, header = FALSE)
names(esOct) <- names

## create data.table
esOctDT <- data.table(esOct)
## has 13015323 rows

setkey(esOctDT, tradeDate, tradeTime, deliveryDate)

esGroupedbySecond <- esOctDT[,mean(tradePrice), by = key(esOctDT)]
## has 759288 rows
names(esGroupedbySecond)[4] <- "avgTradePrice"

## remove 'esOct' and 'esOctDT' now to free RAM?

## October options ----

ezOct <- read.csv('CME_TICK_EZ_201510.csv', stringsAsFactors = FALSE, header = FALSE)

names(ezOct) <- names
