
library(data.table)

## Start looking at data ----
### a note on the underlying futures contract for each option delivery date ----
## Each option's underlying is the next expiring futures contract.  ES Futures expire March/June/Sept/Dec.  So options expiring in Jan/Feb/Mar have the March futures contract as the underlying.

## data layout guide: http://www.cmegroup.com/confluence/display/EPICSANDBOX/Time+and+Sales+CSV+Layout

## October Futures ----

### use fread below instead ----
## esOct <- read.csv('CME_TICK_ES_201510.csv', stringsAsFactors = FALSE, header = FALSE)
## names(esOct) <- c("tradeDate", "tradeTime", "tradeSeqNum_esOct", "sessionInd_esOct", "tickerSymb_esOct", "typeInd_esOct", "deliveryDate_esOct", "tradeQuantity_esOct", "strikePrice_esOct", "tradePrice_esOct", "askBidType_esOct", "indicativeQuoteType_esOct", "marketQuote_esOct", "closeOpenType_esOct", "validOpenException_esOct", "postClose_esOct", "cancelCodeType_esOct", "insertCodeType_esOct", "fastLateInd_esOct", "cabinetInd" , "bookInd_esOct", "entryDate_esOct", "exchangeCode_esOct")

## create data.table
esOctDT <- fread('CME_TICK_ES_201510.csv', stringsAsFactors = FALSE, header = FALSE, sep = ",")
    #data.table(esOct)
## has 13015323 rows
names(esOctDT) <- c("tradeDate", "tradeTime", "tradeSeqNum_esOct", "sessionInd_esOct", "tickerSymb_esOct", "typeInd_esOct", "deliveryDate_esOct", "tradeQuantity_esOct", "strikePrice_esOct", "tradePrice_esOct", "askBidType_esOct", "indicativeQuoteType_esOct", "marketQuote_esOct", "closeOpenType_esOct", "validOpenException_esOct", "postClose_esOct", "cancelCodeType_esOct", "insertCodeType_esOct", "fastLateInd_esOct", "cabinetInd" , "bookInd_esOct", "entryDate_esOct", "exchangeCode_esOct")


setkey(esOctDT, tradeDate, tradeTime, deliveryDate_esOct)

esGroupedbySecond <- esOctDT[,mean(tradePrice_esOct), by = key(esOctDT)]
## has 759288 rows
names(esGroupedbySecond)[4] <- "avgTradePrice_esOct"
setkey(esGroupedbySecond, tradeDate, tradeTime)

## remove 'esOctDT' now to free RAM?

### October options ----

## ezOct <- read.csv('CME_TICK_EZ_201510.csv', stringsAsFactors = FALSE, header = FALSE)
## names(ezOct) <- c("tradeDate", "tradeTime", "tradeSeqNum_ezOct", "sessionInd_ezOct", "tickerSymb_ezOct", "typeInd_ezOct", "deliveryDate_ezOct", "tradeQuantity_ezOct", "strikePrice_ezOct", "tradePrice_ezOct", "askBidType_ezOct", "indicativeQuoteType_ezOct", "marketQuote_ezOct", "closeOpenType_ezOct", "validOpenException_ezOct", "postClose_ezOct", "cancelCodeType_ezOct", "insertCodeType_ezOct", "fastLateInd_ezOct", "cabinetInd" , "bookInd_ezOct", "entryDate_ezOct", "exchangeCode_ezOct")

## create data.table
ezOctDT <- fread('CME_TICK_EZ_201510.csv', stringsAsFactors = FALSE, header = FALSE, sep = ",")
    # data.table(ezOct)
## has 299591 rows
names(ezOctDT) <- c("tradeDate", "tradeTime", "tradeSeqNum_ezOct", "sessionInd_ezOct", "tickerSymb_ezOct", "typeInd_ezOct", "deliveryDate_ezOct", "tradeQuantity_ezOct", "strikePrice_ezOct", "tradePrice_ezOct", "askBidType_ezOct", "indicativeQuoteType_ezOct", "marketQuote_ezOct", "closeOpenType_ezOct", "validOpenException_ezOct", "postClose_ezOct", "cancelCodeType_ezOct", "insertCodeType_ezOct", "fastLateInd_ezOct", "cabinetInd" , "bookInd_ezOct", "entryDate_ezOct", "exchangeCode_ezOct")

## set key
setkey(ezOctDT, tradeDate, tradeTime, strikePrice_ezOct, typeInd_ezOct, deliveryDate_ezOct)

ezGroupedbySecond <- ezOctDT[,mean(tradePrice_ezOct), by = key(ezOctDT)]
## has 196323 rows
names(ezGroupedbySecond)[6] <- "avgTradePrice_ezOct"
setkey(ezGroupedbySecond, tradeDate, tradeTime)


## testing joining the data ----

## left outer join (all rows in es and any matching rows in ez)
octMerged <- ezGroupedbySecond[esGroupedbySecond] 

## This returns more rows because one row in es can match two (or more) rows in ez (because there can be trades in more than one option in each second -- different strikes, maturities, etc).  

## do I want a full outer join??? What can I do if there are options prices and no underlying.  I think I want a left outer join.
## merge(esGroupedbySecond, ezGroupedbySecond, by = key(esGroupedbySecond), all = TRUE)


### Now need to create timeTillExp column ----

## For both ES and EZ: Trading can occur up to 8:30 a.m. on the 3rd Friday of the contract month
# Nov 15s: 
# Dec 15s: 12/18/2015 (20151218)

# since this is Oct only, we can do 18 + 30 + (31 - day + frac_day)
