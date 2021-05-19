#### -------------------------------------------------------------------------------------------------
#### -------------------------------------------------------------------------------------------------
#### ---------------------------------------
#### ---------------------------------------
#### ---- Loading dataset and libraries ----
#### ---------------------------------------
#### ---------------------------------------
#### -------------------------------------------------------------------------------------------------
#### -------------------------------------------------------------------------------------------------
library(jsonlite)
library(dplyr)
library(zoo)
library(rworldmap)
library(missForest)
library(tensorflow)
library(keras)
library(reticulate)
library(tfestimators)
library(tfdatasets)
library(ggplot2)
library(xts)
library(plotrix)
library(glmnet)
library(randomForest)

use_condaenv("r-tensorflow")
sys <- import("sys")
np <- import("numpy")

data <- read.csv("path_to_file/rstudiodata.csv") #### Not Publicly available
data.map <- read.csv("path_to_file/countrygoogle.csv", header = TRUE)
dependent <- read.csv("path_to_file/efficiency.csv")
network <- read.csv("path_to_file/nonmissing.csv")
usa <- read.csv("path_to_file/usaprice.csv") #### Not Publicly available
china <- read.csv("path_to_file/chinaprice.csv") #### Not Publicly available
russia <- read.csv("path_to_file/russiaprice.csv") #### Not Publicly available
price <- fromJSON("https://api.blockchain.info/charts/market-price?timespan=3years&start=2017-01-01&format=json")

bottom.usa <- read.csv("path_to_file/hashrate_USA.csv", header = TRUE)
bottom.asia <- read.csv("path_to_file/hashrate_Asia.csv", header = TRUE)
bottom.europe <- read.csv("path_to_file/hashrate_Europe.csv", header = TRUE)

data$UKSVRPQKIndex <- NULL
data$EUHNUKIIndex <- NULL

network <- network[-1096,-1]
bitprice <- price$values$y[-c(1,2,1096)]

#### -------------------------------------------------------------------------------------------------
#### -------------------------------------------------------------------------------------------------
#### ---------------------------------------
#### ---------------------------------------
#### ---------    Data Analysis    ---------
#### ---------------------------------------
#### ---------------------------------------
#### -------------------------------------------------------------------------------------------------
#### -------------------------------------------------------------------------------------------------

#### Creating the weighted average of the stock returns of the electricity companies:
data.final <- data[,1:41]
data.stock <- data[,42:221]

#### USA:
index.cap <- seq(from = 2, to = 50, by = 2)
index.price <- seq(from = 1, to = 49, by = 2)

weghts.stock <- data.stock[,index.cap]
tot.stocks <- t(apply(weghts.stock, 1, sum, na.rm = TRUE))
weghts.stock.final <- weghts.stock/tot.stocks 
prices <- data.stock[, index.price] 
weighted.average <- weghts.stock.final*prices 

final.wa <- apply(weighted.average, 1, sum, na.rm = TRUE)

data.final$wausa <- final.wa

#### China:
index.cap <- seq(from = 52, to = 180, by = 2)
index.price <- seq(from = 51, to = 179, by = 2)

weghts.stock <- data.stock[,index.cap]
tot.stocks <- t(apply(weghts.stock, 1, sum, na.rm = TRUE))
weghts.stock.final <- weghts.stock/tot.stocks 
prices <- data.stock[, index.price] 
weighted.average <- weghts.stock.final*prices 

final.wa <- apply(weighted.average, 1, sum, na.rm = TRUE)

data.final$wachina <- final.wa

#### -------------------------------------------------------------------------------------------------

#### FSI has weekly (Friday) data:
adj.fsi <- na.locf(data.final$FSI, fromLast = TRUE)
data.final$FSI <- append(adj.fsi, rep(adj.fsi[780], times = 2))

#### Summary Statistics:
str(data.final)

data.missing <- apply(is.na(data.final[,-1]), 2, sum)

#### Miss Forest Algorithm 
data.complete <- missForest(data.final[,-1], maxiter = 20, ntree = 500, maxnodes = 500)
data.complete$OOBerror
data.nonmissing <- data.complete$ximp

#### -------------------------------------------------------------------------------------------------

### Computing the Returns for price data: 
final.data <- data.nonmissing

final.data[,1:14] <- log(final.data[,1:14])
final.data[,27:34] <- log(final.data[,27:34])
final.data[,36:39] <- log(final.data[,36:39])
final.data[,41:42] <- log(final.data[,41:42])

final.data[2:782, 1:14] <- apply(final.data[,1:14], 2, diff, lag = 1)
final.data[2:782, 27:34] <- apply(final.data[,27:34], 2, diff, lag = 1)
final.data[2:782, 36:39] <- apply(final.data[,36:39], 2, diff, lag = 1)
final.data[2:782, 41:42] <- apply(final.data[,41:42], 2, diff, lag = 1)

final.data <- final.data[-1,]

#### -------------------------------------------------------------------------------------------------

#### Summary statistics
data.mean <- as.data.frame(sapply(final.data, mean, na.rm=TRUE))
data.median <- as.data.frame(sapply(final.data, median, na.rm=TRUE))
data.std <- as.data.frame(sapply(final.data, sd, na.rm=TRUE))
data.min <- as.data.frame(sapply(final.data, min, na.rm = TRUE))
data.max <- as.data.frame(sapply(final.data, max, na.rm = TRUE))
data.normal <- as.data.frame(lapply(final.data, shapiro.test))
data.lower.quartile <- as.data.frame(sapply(final.data, quantile, probs = 0.25, na.rm=TRUE))
data.upper.quartile <- as.data.frame(sapply(final.data, quantile, probs = 0.75, na.rm=TRUE))

data.normal <- matrix(0, nrow = 42, ncol = 1)

for(i in 1:42){
  data.normal[i] <- shapiro.test(final.data[,i])[[2]]
}

#### -------------------------------------------------------------------------------------------------

#### Normalising the data and generating train and test dataset. CO2 emissions continue also during the weekend, so we 
#### will introduce also the weekend days. In order to do so, the Friday observation is used also for Saturday and 
#### Sunday.

datedataset <- as.Date(data$Date, "%d/%m/%Y")
datatset.xts <- xts(final.data, order.by = datedataset[-1])

#### The friday observation is used for saturday and sunday
compDataset <- na.locf(merge(datatset.xts, foo=zoo(NA, order.by=seq(start(datatset.xts), 
                      end(datatset.xts),"day",drop=F)))[, 1:42])

valuesregressor <- coredata(compDataset)

data.train <- as.matrix(valuesregressor[1:1034,])
data.test <- as.matrix(valuesregressor[1035:1093,])

#### Normalizing the data
mean.train <- apply(data.train, 2, mean)
std.train <- apply(data.train, 2, sd)

train.data <- scale(data.train, center = mean.train, scale = std.train)
test.data <- scale(data.test, center = mean.train, scale = std.train)

data.together <- rbind(train.data, test.data)

#### -------------------------------------------------------------------------------------------------

#### Graphs for Data Investigatory Analysis:  
data.map$Country <- as.character(as.factor(data.map$Country))
sPDF <- joinCountryData2Map(data.map, joinCode = "ISO3" , nameJoinColumn = "Country")

mapDevice() 
mapCountryData(sPDF, nameColumnToPlot = 'number', catMethod = "fixedWidth",
               colourPalette = c("blue", "lightgray", "lightgreen", "black", "yellow", "orange", "red"), 
               mapTitle = "Google Search 'Bitcoin'")

#### -------------------------------------------------------------------------------------------------
#### -------------------------------------------------------------------------------------------------
#### ---------------------------------------
#### ---------------------------------------
#### --------  Daily CO2 emission  ---------
#### ---------------------------------------
#### ---------------------------------------
#### -------------------------------------------------------------------------------------------------
#### -------------------------------------------------------------------------------------------------

#### Adjusting the dependent variable
dep <- dependent[,2:27]
dep$Bitfury2 <- as.numeric(as.character(dep$Bitfury2))
dep$Canaan3 <- as.numeric(as.character(dep$Canaan3))

#### The code here is to compute the sample mean of the different hash-rates (Not to Run): 
computing.average <- apply(dep, 1, mean, na.rm = TRUE)

#### Under we have the code to compute the weighted mean of the different hashrates, using the top down approach 
bitmain.sub <- dep[, c(5, 6, 7, 8)]
ebang.sub <- dep[, c(9, 10)]
canaan.sub <- dep[, c(14, 15, 16)]

bitmain.avg <- apply(bitmain.sub, 1, mean, na.rm = TRUE)
ebang.avg <- apply(ebang.sub, 1, mean, na.rm = TRUE)
canaan.avg <- apply(canaan.sub, 1, mean, na.rm = TRUE)

dep.new <- cbind(dep, bitmain.avg, ebang.avg, canaan.avg)
dep.new <- dep.new[,-c(5,6,7,8,9,10,14,15,16)]

others.sub <- dep.new[,seq(1,17)]
others.avg <- apply(others.sub, 1, mean, na.rm = TRUE)

dep.new <- cbind(dep.new, others.avg)
dep.new <- dep.new[,-seq(1,17)]

weight.avg.2017 <- apply(dep.new[1:12,], 1, weighted.mean, w = c(74.5, 5.35, 5.35, 14.8), na.rm = TRUE)
weight.avg.2018 <- apply(dep.new[13:24,], 1, weighted.mean, w = c(76, 6, 6, 12), na.rm = TRUE)
weight.avg.2019 <- apply(dep.new[25:36,], 1, weighted.mean, w = c(65.2, 7.90, 21.9, 5), na.rm = TRUE)

computing.average <- c(weight.avg.2017, weight.avg.2018, weight.avg.2019)

#### The missing values occurr when there is no new release, so it is correct to assume that for that particular 
#### month the energy efficiency is the one from the previous month
average <- na.locf(computing.average, fromLast = FALSE)

#### Now we need to move from monthly data to daily data: 
date <- as.character(dependent$Date)
together <- cbind(date, average)

dateformat <- as.Date(together[,1], "%d/%m/%Y")
df.xts <- xts(together[,2], order.by = dateformat)

dependentVar <- na.locf(merge(df.xts, foo=zoo(NA, order.by=seq(start(df.xts), end(df.xts)+30,"day",drop=F)))[, 1])

plot.xts(dependentVar, type = "l", main = "J/Gh")

#### Now we should convert the J/Gh to CO2 emissions to construct the realistic level of CO2 emission
valuesdependent <- coredata(dependentVar)

#### We need to multiply it by the networkhashrate
dependent.newmeasure <- valuesdependent/(1000000000)
hashes <- network$hashrate*1000000000000

final.dependent <- (((hashes*dependent.newmeasure)*1.05)*1e-6)[-c(1,2)]

plot(y = final.dependent, x = seq(as.Date("2017/01/01"), by = "day", length.out = 1093), type = "l", 
     ylab = "MW", xlab = "Year", main = "Dependent Variable")

#### Now we need to convert from MW to CO2 emission. 
tot <- (162 + 158 + 122 + 91 + 75)
china.mining <- 162/tot
russia.mining <- 158/tot
iran.mining <- 122/tot
venezuela.mining <- 91/tot
usa.mining <- 75/tot

#### First conversion factor (no green energy top-down)
conversion.factor <- (china.mining*0.974624913) + (iran.mining*0.631113877) + (russia.mining*0.513180381) + 
  (usa.mining*0.547096737) + (venezuela.mining*0.208069719)

#### Second conversion factor (green energy top-down)
conversion.factor <- (china.mining*0.60651) + (iran.mining*0.631113877) + (russia.mining*0.513180381) + 
  (usa.mining*0.20790) + (venezuela.mining*0.208069719)

daily.kwh <- ((final.dependent*1000)*24)*conversion.factor

plot(daily.kwh/1000000, x = seq(as.Date("2017/01/01"), by = "day", length.out = 1093), type = "l", 
      main = "Daily CO2 Emission - No Green Energy", ylab = "ktCO2", xlab = "Year")

#### Distingushing between dependent train and test (No disposal): 
co2dep <- daily.kwh/1000000

#### Adjusted Co2 with co2 emission from disposal: 
co2dep <- co2dep + 0.0087

dependent.train <- as.matrix(co2dep[1:1034])
dependent.test <- as.matrix(co2dep[1035:1093])

#### -------------------------------------------------------------------------------------------------
#### -------------------------------------------------------------------------------------------------
#### ---------------------------------------
#### ---------------------------------------
#### --------  Bottom-up Approach  ---------
#### ---------------------------------------
#### ---------------------------------------
#### -------------------------------------------------------------------------------------------------
#### -------------------------------------------------------------------------------------------------

bitmain.avg[1] <- (bitmain.avg[which(!is.na(bitmain.avg))])[1]
canaan.avg[1] <- (canaan.avg[which(!is.na(canaan.avg))])[1]
ebang.avg[1] <- (ebang.avg[which(!is.na(ebang.avg))])[1]

#### Replacing missing values assuming rational miners:
bitmain.bottom.up <- na.locf(bitmain.avg, fromLast = FALSE)
canaan.bottom.up <- na.locf(canaan.avg, fromLast = FALSE)
ebang.bottom.up <- na.locf(ebang.avg, fromLast = FALSE)
others.bottom.up <- na.locf(others.avg, fromLast = FALSE)

#### Converting it from monthly to daily data: 
bitmain.time <- cbind(date, bitmain.bottom.up)
canaan.time <- cbind(date, canaan.bottom.up)
ebang.time <- cbind(date, ebang.bottom.up)
others.time <- cbind(date, others.bottom.up)

bitmain.xts <- xts(bitmain.time[,2], order.by = dateformat)
canaan.xts <- xts(canaan.time[,2], order.by = dateformat)
ebang.xts <- xts(ebang.time[,2], order.by = dateformat)
others.xts <- xts(others.time[,2], order.by = dateformat)

bitmain.dependentVar <- na.locf(merge(bitmain.xts, foo=zoo(NA, order.by=seq(start(bitmain.xts), end(bitmain.xts)+30,
                                                                       "day",drop=F)))[, 1])

canaan.dependentVar <- na.locf(merge(canaan.xts, foo=zoo(NA, order.by=seq(start(canaan.xts), end(canaan.xts)+30,
                                                                            "day",drop=F)))[, 1])

ebang.dependentVar <- na.locf(merge(ebang.xts, foo=zoo(NA, order.by=seq(start(ebang.xts), end(ebang.xts)+30,
                                                                            "day",drop=F)))[, 1])

others.dependentVar <- na.locf(merge(others.xts, foo=zoo(NA, order.by=seq(start(others.xts), end(others.xts)+30,
                                                                            "day",drop=F)))[, 1])

values.bitmain <- coredata(bitmain.dependentVar)
values.canaan <- coredata(canaan.dependentVar)
values.ebang <- coredata(ebang.dependentVar)
values.others <- coredata(others.dependentVar)

dependent.new.bitmain <- values.bitmain/(1000000000)
dependent.new.canaan <- values.canaan/(1000000000)
dependent.new.ebang <- values.ebang/(1000000000)
dependent.new.others <- values.others/(1000000000)

#### Plotting the energy efficiency of the different mining producers:
plot.xts(bitmain.dependentVar, type = "l", main = "J/Gh - Bitmain")
plot.xts(canaan.dependentVar, type = "l", main = "J/Gh - Canaan")
plot.xts(ebang.dependentVar, type = "l", main = "J/Gh - Ebang")
plot.xts(others.dependentVar, type = "l", main = "J/Gh - Others")

#### Computing the first-part (green energy): 
bottom.usa$Weight <- bottom.usa$Weight/100
bottom.china.weight <- (71.70+0.08+0.09)/100 ## It includes Hong Kong and Taiwan
bottom.asia$Weight <- bottom.asia$Weight/100
bottom.europe$Weight <- bottom.europe$Weight/100 ## It includes Libia

sum(bottom.usa$Weight) + bottom.china.weight + sum(bottom.asia$Weight) + sum(bottom.europe$Weight)

#### No we start with the implementation of the novel bottom up approach
hashes.bycountry.America <- cbind(hashes*bottom.usa$Emission[1], hashes*bottom.usa$Emission[2], 
                              hashes*bottom.usa$Emission[3], hashes*bottom.usa$Emission[4],
                              hashes*bottom.usa$Emission[5], hashes*bottom.usa$Emission[6],
                              hashes*bottom.usa$Emission[7], hashes*bottom.usa$Emission[8],
                              hashes*bottom.usa$Emission[9], hashes*bottom.usa$Emission[10],
                              hashes*bottom.usa$Emission[11], hashes*bottom.usa$Emission[12])

hashes.America <- apply(hashes.bycountry.America, 1, weighted.mean, w = bottom.usa$Weight)

hashes.America.2017 <- hashes.America[1:365]
hashes.America.2018 <- hashes.America[366:730]
hashes.America.2019 <- hashes.America[731:1095]

bitmain.America.2017 <- (((((0.138*hashes.America.2017)*dependent.new.bitmain[1:365,])*1.05)*1e-06)*1000*24)/1000000
canaan.America.2017 <- (((((0.01125*hashes.America.2017)*dependent.new.canaan[1:365,])*1.05)*1e-06)*1000*24)/1000000
ebang.America.2017 <- (((((0.01550*hashes.America.2017)*dependent.new.ebang[1:365,])*1.05)*1e-06)*1000*24)/1000000
others.America.2017 <- (((((0.037*hashes.America.2017)*dependent.new.others[1:365,])*1.05)*1e-06)*1000*24)/1000000
America.emission.2017 <- (bitmain.America.2017 + canaan.America.2017 + ebang.America.2017 + others.America.2017)

bitmain.America.2018 <- (((((0.14*hashes.America.2018)*dependent.new.bitmain[366:730,])*1.05)*1e-06)*1000*24)/1000000
canaan.America.2018 <- (((((0.005*hashes.America.2018)*dependent.new.canaan[366:730,])*1.05)*1e-06)*1000*24)/1000000
ebang.America.2018 <- (((((0.012*hashes.America.2018)*dependent.new.ebang[366:730,])*1.05)*1e-06)*1000*24)/1000000
others.America.2018 <- (((((0*hashes.America.2018)*dependent.new.others[366:730,])*1.05)*1e-06)*1000*24)/1000000
America.emission.2018 <- (bitmain.America.2018 + canaan.America.2018 + ebang.America.2018 + others.America.2018)

bitmain.America.2019 <- (((((0.12*hashes.America.2019)*dependent.new.bitmain[731:1095,])*1.05)*1e-06)*1000*24)/1000000
canaan.America.2019 <- (((((0.003*hashes.America.2019)*dependent.new.canaan[731:1095,])*1.05)*1e-06)*1000*24)/1000000
ebang.America.2019 <- (((((0.024*hashes.America.2019)*dependent.new.ebang[731:1095,])*1.05)*1e-06)*1000*24)/1000000
others.America.2019 <- (((((0.0125*hashes.America.2019)*dependent.new.others[731:1095,])*1.05)*1e-06)*1000*24)/1000000
America.emission.2019 <- (bitmain.America.2019 + canaan.America.2019 + ebang.America.2019 + others.America.2019)

America.emission <- c(America.emission.2017, America.emission.2018, America.emission.2019)

hashes.China.2017 <- hashes[1:365]*bottom.china.weight*0.60651
hashes.China.2018 <- hashes[366:730]*bottom.china.weight*0.60651
hashes.China.2019 <- hashes[731:1095]*bottom.china.weight*0.60651

bitmain.China.2017 <- (((((0.36*hashes.China.2017)*dependent.new.bitmain[1:365,])*1.05)*1e-06)*1000*24)/1000000
canaan.China.2017 <- (((((0.01125*hashes.China.2017)*dependent.new.canaan[1:365,])*1.05)*1e-06)*1000*24)/1000000
ebang.China.2017 <- (((((0.0155*hashes.China.2017)*dependent.new.ebang[1:365,])*1.05)*1e-06)*1000*24)/1000000
others.China.2017 <- (((((0.037*hashes.China.2017)*dependent.new.others[1:365,])*1.05)*1e-06)*1000*24)/1000000
China.emission.2017 <- (bitmain.China.2017 + canaan.China.2017 + ebang.China.2017 + others.China.2017)

bitmain.China.2018 <- (((((0.369*hashes.China.2018)*dependent.new.bitmain[366:730,])*1.05)*1e-06)*1000*24)/1000000
canaan.China.2018 <- (((((0.11*hashes.China.2018)*dependent.new.canaan[366:730,])*1.05)*1e-06)*1000*24)/1000000
ebang.China.2018 <- (((((0.036*hashes.China.2018)*dependent.new.ebang[366:730,])*1.05)*1e-06)*1000*24)/1000000
others.China.2018 <- (((((0*hashes.China.2018)*dependent.new.others[366:730,])*1.05)*1e-06)*1000*24)/1000000
China.emission.2018 <- (bitmain.China.2018 + canaan.China.2018 + ebang.China.2018 + others.China.2018)

bitmain.China.2019 <- (((((0.315*hashes.China.2019)*dependent.new.bitmain[731:1095,])*1.05)*1e-06)*1000*24)/1000000
canaan.China.2019 <- (((((0.073*hashes.China.2019)*dependent.new.canaan[731:1095,])*1.05)*1e-06)*1000*24)/1000000
ebang.China.2019 <- (((((0.065*hashes.China.2019)*dependent.new.ebang[731:1095,])*1.05)*1e-06)*1000*24)/1000000
others.China.2019 <- (((((0.0125*hashes.China.2019)*dependent.new.others[731:1095,])*1.05)*1e-06)*1000*24)/1000000
China.emission.2019 <- (bitmain.China.2019 + canaan.China.2019 + ebang.China.2019 + others.China.2019)

China.emission <- c(China.emission.2017, China.emission.2018, China.emission.2019)

hashes.bycountry.asia <- cbind(hashes*bottom.asia$Emission[1], hashes*bottom.asia$Emission[2], 
                                  hashes*bottom.asia$Emission[3], hashes*bottom.asia$Emission[4],
                                  hashes*bottom.asia$Emission[5], hashes*bottom.asia$Emission[6],
                                  hashes*bottom.asia$Emission[7], hashes*bottom.asia$Emission[8],
                                  hashes*bottom.asia$Emission[9], hashes*bottom.asia$Emission[10],
                                  hashes*bottom.asia$Emission[11], hashes*bottom.asia$Emission[12],
                                  hashes*bottom.asia$Emission[13], hashes*bottom.asia$Emission[14],
                                  hashes*bottom.asia$Emission[15], hashes*bottom.asia$Emission[16],
                                  hashes*bottom.asia$Emission[17], hashes*bottom.asia$Emission[18],
                                  hashes*bottom.asia$Emission[19], hashes*bottom.asia$Emission[20],
                                  hashes*bottom.asia$Emission[21], hashes*bottom.asia$Emission[22], 
                                  hashes*bottom.asia$Emission[23], hashes*bottom.asia$Emission[24],
                                  hashes*bottom.asia$Emission[25])

hashes.Asia <- apply(hashes.bycountry.asia, 1, weighted.mean, w = bottom.asia$Weight)

hashes.Asia.2017 <- hashes.Asia[1:365]
hashes.Asia.2018 <- hashes.Asia[366:730]
hashes.Asia.2019 <- hashes.Asia[731:1095]

bitmain.Asia.2017 <- (((((0.167*hashes.Asia.2017)*dependent.new.bitmain[1:365,])*1.05)*1e-06)*1000*24)/1000000
canaan.Asia.2017 <- (((((0.01125*hashes.Asia.2017)*dependent.new.canaan[1:365,])*1.05)*1e-06)*1000*24)/1000000
ebang.Asia.2017 <- (((((0.01550*hashes.Asia.2017)*dependent.new.ebang[1:365,])*1.05)*1e-06)*1000*24)/1000000
others.Asia.2017 <- (((((0.037*hashes.Asia.2017)*dependent.new.others[1:365,])*1.05)*1e-06)*1000*24)/1000000
Asia.emission.2017 <- (bitmain.Asia.2017 + canaan.Asia.2017 + ebang.Asia.2017 + others.Asia.2017)

bitmain.Asia.2018 <- (((((0.17*hashes.Asia.2018)*dependent.new.bitmain[366:730,])*1.05)*1e-06)*1000*24)/1000000
canaan.Asia.2018 <- (((((0.005*hashes.Asia.2018)*dependent.new.canaan[366:730,])*1.05)*1e-06)*1000*24)/1000000
ebang.Asia.2018 <- (((((0.036*hashes.Asia.2018)*dependent.new.ebang[366:730,])*1.05)*1e-06)*1000*24)/1000000
others.Asia.2018 <- (((((0*hashes.Asia.2018)*dependent.new.others[366:730,])*1.05)*1e-06)*1000*24)/1000000
Asia.emission.2018 <- (bitmain.Asia.2018 + canaan.Asia.2018 + ebang.Asia.2018 + others.Asia.2018)

bitmain.Asia.2019 <- (((((0.147*hashes.Asia.2019)*dependent.new.bitmain[731:1095,])*1.05)*1e-06)*1000*24)/1000000
canaan.Asia.2019 <- (((((0.003*hashes.Asia.2019)*dependent.new.canaan[731:1095,])*1.05)*1e-06)*1000*24)/1000000
ebang.Asia.2019 <- (((((0.065*hashes.Asia.2019)*dependent.new.ebang[731:1095,])*1.05)*1e-06)*1000*24)/1000000
others.Asia.2019 <- (((((0.0125*hashes.Asia.2019)*dependent.new.others[731:1095,])*1.05)*1e-06)*1000*24)/1000000
Asia.emission.2019 <- (bitmain.Asia.2019 + canaan.Asia.2019 + ebang.Asia.2019 + others.Asia.2019)

Asia.emission <- c(Asia.emission.2017, Asia.emission.2018, Asia.emission.2019)

hashes.bycountry.europe <- cbind(hashes*bottom.europe$Emission[1], hashes*bottom.europe$Emission[2], 
                                 hashes*bottom.europe$Emission[3], hashes*bottom.europe$Emission[4],
                                 hashes*bottom.europe$Emission[5], hashes*bottom.europe$Emission[6],
                                 hashes*bottom.europe$Emission[7], hashes*bottom.europe$Emission[8],
                                 hashes*bottom.europe$Emission[9], hashes*bottom.europe$Emission[10],
                                 hashes*bottom.europe$Emission[11], hashes*bottom.europe$Emission[12],
                                 hashes*bottom.europe$Emission[13], hashes*bottom.europe$Emission[14],
                                 hashes*bottom.europe$Emission[15], hashes*bottom.europe$Emission[16],
                                 hashes*bottom.europe$Emission[17], hashes*bottom.europe$Emission[18],
                                 hashes*bottom.europe$Emission[19], hashes*bottom.europe$Emission[20],
                                 hashes*bottom.europe$Emission[21], hashes*bottom.europe$Emission[22], 
                                 hashes*bottom.europe$Emission[23], hashes*bottom.europe$Emission[24],
                                 hashes*bottom.europe$Emission[25], hashes*bottom.europe$Emission[26],
                                 hashes*bottom.europe$Emission[27], hashes*bottom.europe$Emission[28],
                                 hashes*bottom.europe$Emission[29], hashes*bottom.europe$Emission[30],
                                 hashes*bottom.europe$Emission[31])

hashes.Europe <- apply(hashes.bycountry.europe, 1, weighted.mean, w = bottom.europe$Weight)

hashes.Europe.2017 <- hashes.Europe[1:365]
hashes.Europe.2018 <- hashes.Europe[366:730]
hashes.Europe.2019 <- hashes.Europe[731:1095]

bitmain.Europe.2017 <- (((((0.08*hashes.Europe.2017)*dependent.new.bitmain[1:365,])*1.05)*1e-06)*1000*24)/1000000
canaan.Europe.2017 <- (((((0.01125*hashes.Europe.2017)*dependent.new.canaan[1:365,])*1.05)*1e-06)*1000*24)/1000000
ebang.Europe.2017 <- (((((0.01550*hashes.Europe.2017)*dependent.new.ebang[1:365,])*1.05)*1e-06)*1000*24)/1000000
others.Europe.2017 <- (((((0.037*hashes.Europe.2017)*dependent.new.others[1:365,])*1.05)*1e-06)*1000*24)/1000000
Europe.emission.2017 <- (bitmain.Europe.2017 + canaan.Europe.2017 + ebang.Europe.2017 + others.Europe.2017)

bitmain.Europe.2018 <- (((((0.081*hashes.Europe.2018)*dependent.new.bitmain[366:730,])*1.05)*1e-06)*1000*24)/1000000
canaan.Europe.2018 <- (((((0*hashes.Europe.2018)*dependent.new.canaan[366:730,])*1.05)*1e-06)*1000*24)/1000000
ebang.Europe.2018 <- (((((0.036*hashes.Europe.2018)*dependent.new.ebang[366:730,])*1.05)*1e-06)*1000*24)/1000000
others.Europe.2018 <- (((((0*hashes.Europe.2018)*dependent.new.others[366:730,])*1.05)*1e-06)*1000*24)/1000000
Europe.emission.2018 <- (bitmain.Europe.2018 + canaan.Europe.2018 + ebang.Europe.2018 + others.Europe.2018)

bitmain.Europe.2019 <- (((((0.07*hashes.Europe.2019)*dependent.new.bitmain[731:1095,])*1.05)*1e-06)*1000*24)/1000000
canaan.Europe.2019 <- (((((0*hashes.Europe.2019)*dependent.new.canaan[731:1095,])*1.05)*1e-06)*1000*24)/1000000
ebang.Europe.2019 <- (((((0.065*hashes.Europe.2019)*dependent.new.ebang[731:1095,])*1.05)*1e-06)*1000*24)/1000000
others.Europe.2019 <- (((((0.0125*hashes.Europe.2019)*dependent.new.others[731:1095,])*1.05)*1e-06)*1000*24)/1000000
Europe.emission.2019 <- (bitmain.Europe.2019 + canaan.Europe.2019 + ebang.Europe.2019 + others.Europe.2019)

Europe.emission <- c(Europe.emission.2017, Europe.emission.2018, Europe.emission.2019)

total.emission <- (America.emission + China.emission + Asia.emission + Europe.emission)[-c(1,2)]

#### Adjusting total emission to account for e-waste 
total.emission <- total.emission + 0.0087

#### Plotting the dependent variables: 
plot(total.emission, x = seq(as.Date("2017/01/01"), by = "day", length.out = 1093), type = "l", 
     main = "Daily CO2 Emission - Energy use and E-waste", ylab = "ktCO2", xlab = "Year")

#### Obtaining the dependent variable for the neural network:
dependent.train <- as.matrix(total.emission[1:1034])
dependent.test <- as.matrix(total.emission[1035:1093])

#### -------------------------------------------------------------------------------------------------
#### -------------------------------------------------------------------------------------------------
#### ---------------------------------------
#### ---------------------------------------
#### ----------     MC Dropout    ----------
#### ---------------------------------------
#### ---------------------------------------
#### -------------------------------------------------------------------------------------------------
#### -------------------------------------------------------------------------------------------------

#### The code below reports the implementation of the MC dropout proposed by Gal and Garhahami (2016)
#### the argument "proba" specifies the dropout probability. The code here is provided for a two 
#### layer neural network with 3 and 2 hidden nodes as implementation example.

T.replicas <- 30 
proba <- 0.005
pred.data1 <- matrix(0, ncol = T.replicas, nrow = nrow(test.data))

#### In order to impose dropout also at test phase, the layer dropout is specified outside the 
#### sequential neural network (this allows to set the Boolean condition while training)
dropout_1 <- layer_dropout(rate = proba)
dropout_2 <- layer_dropout(rate = proba)

tf$reset_default_graph()
k_set_session(tf$Session())

input <- layer_input(shape = dim(train.data)[[2]])

output <- input %>%
  layer_dense(units = 3, activation = "relu") %>% 
  dropout_1(training = TRUE) %>% 
  layer_dense(units = 2, activation = "relu") %>% 
  dropout_2(training = TRUE) %>% 
  layer_dense(units = 1)

model <- keras_model(input, output)

model %>% compile(loss = "mean_squared_error",optimizer_adam(lr = 0.01, beta_1 = 0.9, 
                  beta_2 = 0.999, epsilon = NULL, decay = 0, amsgrad = FALSE, clipnorm = NULL,
                  clipvalue = NULL), metrics = list("mean_absolute_error", "mean_squared_error"))

history <- model %>% fit(train.data, dependent.train, epochs = 80, verbose = 0, 
                         validation_data = list(test.data, dependent.test), 
                         callbacks = callback_model_checkpoint("weightdropsim.best.hdf5", 
                         monitor = "val_loss", verbose = 0,save_best_only = TRUE, 
                         save_weights_only = FALSE, mode = c("auto","min", "max"), period = 1)) 

results1 <- model %>% evaluate(test.data, dependent.test, verbose = 0)

#### The MC-dropout allows to approximate the predictive distribution by performing T.replicas
#### stochastic forward passes:
for(i in 1:T.replicas){
  pred.data1[,i] <- model %>% predict(model = model, x = test.data) %>% as.vector()
}

#### Computing the first and second moments of the distribution in order to estimate the uncertainty 
#### of the fitted neural network. 
forecast_avg <- apply(pred.data1, 1, mean)
forecast_sd <- apply(pred.data1, 1, sd)

aleatoric.uncertainty <- (sum((dependent.test - forecast_avg)**2))/length(dependent.test)
overall.variance <- sqrt((forecast_sd**2) + aleatoric.uncertainty)

#### ----------------------------------------------------------------------- 0.95 prediction interval

lower_interval <- forecast_avg + qnorm(0.025)*overall.variance
upper_interval <- forecast_avg + qnorm(1-0.025)*overall.variance
confidence_interval <- cbind(lower_interval, upper_interval)

#### Computing the empirical coverage: 
outside <- ifelse(confidence_interval[,1]>labels.test | confidence_interval[,2]<labels.test, 1, 0)
coverage <- sum(outside)/(length(dependent.test))

#### -------------------------------------------------------------------------------------------------
#### -------------------------------------------------------------------------------------------------
#### ---------------------------------------
#### ---------------------------------------
#### ----------   Training Plot    ---------
#### ---------------------------------------
#### ---------------------------------------
#### -------------------------------------------------------------------------------------------------
#### -------------------------------------------------------------------------------------------------

history <- model %>% fit(train.data, dependent.train, epochs = 5000, verbose = 0, 
                         validation_data = list(test.data, dependent.test), 
                         callbacks = callback_model_checkpoint("weightdropsim.best.hdf5", monitor = "val_loss", 
                         verbose = 0,save_best_only = TRUE, save_weights_only = FALSE, 
                         mode = c("auto","min", "max"), period = 1)) 

plot(history)

#### -------------------------------------------------------------------------------------------------
#### -------------------------------------------------------------------------------------------------
#### ---------------------------------------
#### ---------------------------------------
#### ----------    Random forest   ---------
#### ---------------------------------------
#### ---------------------------------------
#### -------------------------------------------------------------------------------------------------
#### -------------------------------------------------------------------------------------------------

searchGrid <- expand.grid(mtry = seq(20, 40, by = 2), node_size  = seq(2, 20, by = 2), 
                          ntree = seq(50, 500, by = 50))

optimal.sparsity <- apply(searchGrid, 1, function(parameterList){
  
  current.mtry <- parameterList[["mtry"]]
  current.node_size <- parameterList[["node_size"]]
  current.ntree <- parameterList[["ntree"]]
  
  fit_rf <- randomForest(train.data, dependent.train, mtry = current.mtry, 
                         ntree = current.ntree,  
                         nodesize = current.node_size)
  
  forecast_rf <- predict(fit_rf, test.data)
  
  mae_rf <- (sum(abs(forecast_rf-dependent.test)))/nrow(dependent.test)
  mse_rf <- (sum((forecast_rf-dependent.test)**2))/nrow(dependent.test)
  
  output <- c(mae_rf, mse_rf, current.mtry, current.ntree, current.node_size)

})

optimal.sparsity <- t(optimal.sparsity)

#### This is be the optimal combination
optimal.sparsity[which(optimal.sparsity[,1] == min(optimal.sparsity[,1])),]

fit_rf <- randomForest(train.data, dependent.train, ntree = 100, mtry = 40, nodesize = 5)

forecast_rf <- predict(fit_rf, test.data)
mae_rf <- (sum(abs(forecast_rf-dependent.test)))/nrow(dependent.test)
mse_rf <- (sum((forecast_rf-dependent.test)**2))/nrow(dependent.test)
