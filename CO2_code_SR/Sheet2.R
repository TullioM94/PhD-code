#### -------------------------------------------------------------------------------------------------
#### -------------------------------------------------------------------------------------------------
#### ---------------------------------------
#### ---------------------------------------
#### ---- Loading dataset and libraries ----
#### ---------------------------------------
#### ---------------------------------------
#### -------------------------------------------------------------------------------------------------
#### -------------------------------------------------------------------------------------------------
library(zoo)
library(dplyr)

data <- read.csv(file = "path_to_file/nonmissing.csv") #### Not Publicly available
usa <- read.csv("path_to_file/usaprice.csv") #### Not Publicly available
china <- read.csv("path_to_file/chinaprice.csv") #### Not Publicly available
russia <- read.csv("path_to_file/russiaprice.csv") #### Not Publicly available
efficiency <- read.csv("path_to_file/efficiency.csv")

#### -------------------------------------------------------------------------------------------------
#### -------------------------------------------------------------------------------------------------
#### ---------------------------------------
#### ---------------------------------------
#### -------    Adjusting the data   -------
#### ---------------------------------------
#### ---------------------------------------
#### -------------------------------------------------------------------------------------------------
#### -------------------------------------------------------------------------------------------------

final.data <- data[-1096,-1]

#### Adjusting dataset for electricity consumption: 
date.range <- seq(as.Date("2017/01/01"), by = "day", length.out = 1095) %>% as.character()

#### Converting the date into strings: 
usa$finaldate <- gsub("/", "-", usa$Date)
china$finaldate <- gsub("/", "-", china$Date)
russia$finaldate <- gsub("/", "-", russia$Date)

usa$final.year <- substr(usa$finaldate, start = 7, stop = 11)
china$final.year <- substr(china$finaldate, start = 7, stop = 11)
russia$final.year <- substr(russia$finaldate, start = 7, stop = 11)

#### Creating final dataset for energy prices: 
energy.data <- matrix(0, nrow = 1095, ncol = 8)
energy.data[,1] <- date.range
energy.data[,2] <- substr(date.range, start = 1, stop = 4)


energy.data[,3] <- ifelse(energy.data[,2] == usa$final.year[1], usa$Residential[1], 
                          ifelse(energy.data[,2] == usa$final.year[2], usa$Residential[2], 
                                  ifelse(energy.data[,2] == usa$final.year[3], usa$Residential[3], 0))) 

energy.data[,4] <- ifelse(energy.data[,2] == russia$final.year[2], russia$Price[2], 
                          ifelse(energy.data[,2] == russia$final.year[3], russia$Price[3], 
                                 ifelse(energy.data[,2] == russia$final.year[4], russia$Price[4], 0))) 

energy.data[,5] <- ifelse(energy.data[,2] == china$final.year[1], china$CatA[1], 
                          ifelse(energy.data[,2] == china$final.year[2], china$CatA[2], 
                                 ifelse(energy.data[,2] == china$final.year[3], china$CatA[3], 0))) 

energy.data[,6] <- ifelse(energy.data[,2] == china$final.year[1], china$CatD[1], 
                          ifelse(energy.data[,2] == china$final.year[2], china$CatD[2], 
                                 ifelse(energy.data[,2] == china$final.year[3], china$CatD[3], 0))) 

energy.data[,7] <- rep(x = 0.008, times = nrow(energy.data)) 

energy.data[,8] <- rep(x = 0.1284, times = nrow(energy.data))


energy.data.final <- energy.data[,-2] %>% as.data.frame() 

colnames(energy.data.final) <- c("date", "usa", "russia", "china1", "china2", "iran", "venezuela") 

str(energy.data.final)

energy.data.final$usa <- energy.data.final$usa %>% as.character() %>% as.numeric()
energy.data.final$russia <- energy.data.final$russia %>% as.character() %>% as.numeric()
energy.data.final$china1 <- energy.data.final$china1 %>% as.character() %>% as.numeric()
energy.data.final$china2 <- energy.data.final$china2 %>% as.character() %>% as.numeric()
energy.data.final$venezuela <- energy.data.final$venezuela %>% as.character() %>% as.numeric()
energy.data.final$iran <- energy.data.final$iran %>% as.character() %>% as.numeric()

#### Determining the weights, depending on the mining geographical location: 
tot <- (162 + 158 + 122 + 91 + 75)
china.mining <- 162/tot
russia.mining <- 158/tot
iran.mining <- 122/tot
venezuela.mining <- 91/tot
usa.mining <- 75/tot

#### Computing the weights average of the price of electricity: 
average.price <- (china.mining*energy.data.final$china1) + (venezuela.mining*energy.data.final$venezuela) + 
  (usa.mining*energy.data.final$usa) + (russia.mining*energy.data.final$russia) + (iran.mining*energy.data.final$iran)

#### Plotting the average electricity: 
plot(y = average.price, x = seq(as.Date("2017/01/01"), by = "day", length.out = 1095), type = "l", 
     ylab = "USD/kWh", xlab = "Year", main = "Average Electricity Price")

#### -------------------------------------------------------------------------------------------------
#### -------------------------------------------------------------------------------------------------
#### ---------------------------------------
#### ---------------------------------------
#### --- Computing Lower and Upper bound ---
#### ---------------------------------------
#### ---------------------------------------
#### -------------------------------------------------------------------------------------------------
#### -------------------------------------------------------------------------------------------------

btc.day <- (0.00002011656761*(12.5*(final.data$hashrate*(10**12))))/(final.data$difficulty)

par(mfrow=c(1,1))
d <- plot(y = btc.day, x = seq(as.Date("2017/01/01"),by = "day", length.out = 1095), type = "l", 
     main = "Bitcoin mined per day", ylab = "Bitcoin", xlab = "Time")
abline(d, h = mean(btc.day), col = "red", lty = 2)
text(x = as.Date("2019/06/06"), y = 1200, "mean = 1883")

#### Average number of Bitcoins mined per day
mean(btc.day)

term1 <- (final.data$price)*btc.day
term2 <- average.price*24

y.variable <- (term1/term2)*1000 

#### Plotting the MW
plot(y = y.variable*1e-6, x = seq(as.Date("2017/01/01"), by = "day", length.out = 1095), type = "l", 
     ylab = "MW", xlab = "Year", main = "Upper Bound Power Consumption")

#### Now we want to determine the lower bound for the CO2 emission
str(efficiency)

efficiency$Canaan3 <- as.numeric(efficiency$Canaan3)
efficiency$Bitfury2 <- as.numeric(efficiency$Bitfury2)
efficiency$Date <- as.character(efficiency$Date)
efficiency$trunc <- substr(efficiency$Date, start = 3, stop = 10)

date.new.range <- seq(as.Date("2017/01/01"), by = "day", length.out = 1095) %>% format(., "%d/%m/%Y") %>% as.character()
trunc.new.date <- substr(date.new.range, start = 3, stop = 10)

new.data.minimum <- matrix(0, nrow = 1095, ncol = 3)
new.data.minimum[,1] <- trunc.new.date
new.data.minimum[,3] <- date.new.range

#### Computing the minimum efficiency for the given month:
minimum <- apply(efficiency[, -28], 1, min, na.rm = TRUE) %>% as.numeric()

#### Replacing missing values with the efficiency from the previous month: 
efficiency$minimum <- na.locf(minimum, fromLast = FALSE)

new.data.minimum[,2] <- ifelse(new.data.minimum[,1] == efficiency$trunc[1], efficiency$minimum[1], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[2], efficiency$minimum[2], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[3], efficiency$minimum[3], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[4], efficiency$minimum[4], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[5], efficiency$minimum[5], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[6], efficiency$minimum[6], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[7], efficiency$minimum[7], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[8], efficiency$minimum[8], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[9], efficiency$minimum[9], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[10], efficiency$minimum[10], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[11], efficiency$minimum[11], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[12], efficiency$minimum[12], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[13], efficiency$minimum[13], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[14], efficiency$minimum[14], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[15], efficiency$minimum[15], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[16], efficiency$minimum[16],
                        ifelse(new.data.minimum[,1] == efficiency$trunc[17], efficiency$minimum[17], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[18], efficiency$minimum[18], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[19], efficiency$minimum[19], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[20], efficiency$minimum[20], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[21], efficiency$minimum[21], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[22], efficiency$minimum[22], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[23], efficiency$minimum[23], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[24], efficiency$minimum[24], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[25], efficiency$minimum[25], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[26], efficiency$minimum[26], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[27], efficiency$minimum[27], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[28], efficiency$minimum[28], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[29], efficiency$minimum[29], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[30], efficiency$minimum[30], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[31], efficiency$minimum[31], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[32], efficiency$minimum[32], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[33], efficiency$minimum[33], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[34], efficiency$minimum[34], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[35], efficiency$minimum[35], 
                        ifelse(new.data.minimum[,1] == efficiency$trunc[36], efficiency$minimum[36], 
                               0))))))))))))))))))))))))))))))))))))


mostefficient <- new.data.minimum[,2]  %>% as.numeric()
mostefficient.newmeasure <- mostefficient/(1000000000)
hashes <- final.data$hashrate*1000000000000

lower.bound <- (hashes*mostefficient.newmeasure)*1e-6

plot(y = lower.bound, x = seq(as.Date("2017/01/01"), by = "day", length.out = 1095), type = "l", 
     ylab = "MW", xlab = "Year", main = "Lower Bound Power Consumption")

bounds <- cbind((y.variable*1e-6), lower.bound)

#### Computing the yearly energy consumption: 
max.daily <- (y.variable*1e-6)*24 ### This is the estimate in MW
min.daily <- lower.bound*24 ### This is the estimate in MW 

maxyear2017 <- max.daily[1:365]
maxyear2018 <- max.daily[366:730]
maxyear2019 <- max.daily[731:1095]

minyear2017 <- min.daily[1:365]
minyear2018 <- min.daily[366:730]
minyear2019 <- min.daily[731:1095]

#### computing the Year production in Terawatt
max.year2017 <- sum(maxyear2017)*1e-6
max.year2018 <- sum(maxyear2018)*1e-6 
max.year2019 <- sum(maxyear2019)*1e-6 

min.year2017 <- sum(minyear2017)*1e-6 
min.year2018 <- sum(minyear2018)*1e-6 
min.year2019 <- sum(minyear2019)*1e-6 

#### First conversion factor (no green energy top-down)
conversion.factor <- (china.mining*0.974624913) + (iran.mining*0.631113877) + (russia.mining*0.513180381) + 
  (usa.mining*0.547096737) + (venezuela.mining*0.208069719)

#### Second conversion factor (green energy top-down)
conversion.factor <- (china.mining*0.60651) + (iran.mining*0.631113877) + (russia.mining*0.513180381) + 
  (usa.mining*0.20790) + (venezuela.mining*0.208069719)

#### Yearly CO2 emission:
maxco2.year2017 <- ((max.year2017*1e9)*conversion.factor)/1000000000
maxco2.year2018 <- ((max.year2018*1e9)*conversion.factor)/1000000000
maxco2.year2019 <- ((max.year2019*1e9)*conversion.factor)/1000000000 

minco2.year2017 <- ((min.year2017*1e9)*conversion.factor)/1000000000
minco2.year2018 <- ((min.year2018*1e9)*conversion.factor)/1000000000
minco2.year2019 <- ((min.year2019*1e9)*conversion.factor)/1000000000 

#### Daily CO2 emission:
max.daily.kwh <- ((y.variable*1e-6*1000)*24)*conversion.factor
min.daily.kwh <- ((lower.bound*1000)*24)*conversion.factor

plot(max.daily.kwh/1000000, x = seq(as.Date("2017/01/01"), by = "day", length.out = 1095), type = "l", 
     main = "Daily CO2 Emission", ylab = "ktCO2", xlab = "Year")

lines(min.daily.kwh/1000000, x = seq(as.Date("2017/01/01"), by = "day", length.out = 1095), type = "l", 
     main = "Daily CO2 Emission", ylab = "ktCO2", xlab = "Year")

polygon(c(seq(as.Date("2017/01/01"), by = "day", length.out = 1095), 
          rev(seq(as.Date("2017/01/01"), by = "day", length.out = 1095))), 
        c(max.daily.kwh/1000000 ,rev(min.daily.kwh/1000000)), col = rgb(0.2, 0.5, 0.5,1) )
