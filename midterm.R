#CS510 Midterm
#Author: Garrett Stemmler
#The purpose of this code is to create and test regression models to predict the Wilshire REIT index
#Real world data taken online is used
#Theres is monthly,weekly,daily, and quartely data so they have to be transformed to a single form


# LOAD PACKAGES
library(xts)
library(zoo)
library(dplyr)
library(tseries)
library(leaps)
library(caret)
library(lattice)
library(ggplot2)
library(lmtest)

# -------------------------------------------------------------------------------------------
# LOAD and CLEAN DATA
# This block of code reads in each format of data

daily <- read.csv("https://raw.githubusercontent.com/gstemmler/CS510Midterm/main/daily.csv", header = TRUE, stringsAsFactors = FALSE)
daily$date <- as.Date(daily$date, "%Y-%m-%d")

weekly <- read.csv("https://raw.githubusercontent.com/gstemmler/CS510Midterm/main/weekly.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(weekly)[1] <- "date"
colnames(weekly)[2] <- "Mortage_Rate_30Year"
weekly$date <- as.Date(weekly$date, "%Y-%m-%d")

monthly <- read.csv("https://raw.githubusercontent.com/gstemmler/CS510Midterm/main/monthly.csv", header = TRUE, stringsAsFactors = FALSE)
monthly <- monthly[1:238,1:7]
monthly$date <- as.Date(monthly$date , "%Y-%m-%d")

quarterly <- read.csv("https://raw.githubusercontent.com/gstemmler/CS510Midterm/main/quartely.csv", header = TRUE, stringsAsFactors = FALSE)
quarterly <- quarterly[1:76,1:8]
colnames(quarterly)[1] <- "date"
quarterly$date <- as.Date(quarterly$date , "%Y-%m-%d")

# -------------------------------------------------------------------------------------------
# CONVERT W/ TS
#Now we're going to use the ts function to change all the data to time series data

y.ts<- ts(daily$WILLREITIND, start = c(2000,1,3), frequency = 252.75)
S5UTIL.ts <- ts(daily$S5UTIL, start = c(2000,1,3), frequency = 252.75)
S5CONS.ts <- ts(daily$S5CONS, start = c(2000,1,3), frequency = 252.75)
S5COND.ts <- ts(daily$S5COND, start = c(2000,1,3),frequency = 252.75)
Gold.ts <- ts(daily$Gold_Price, start = c(2000,1,3),frequency = 252.75)

MortgageRate.ts <- ts(weekly$Mortage_Rate_30Year, start = c(1999,52), frequency = 52.15)

unemployment.ts <- ts(monthly$US_unemployment_rate, start = c(2000,1),frequency = 12)
PPI.petroleum.ts <- ts(monthly$PPI_Petroleun_and_Coal, start = c(2000,1),frequency = 12)
PPI.Lumber.ts <- ts(monthly$PPI_Lumber_Wood, start = c(2000,1),frequency = 12)
PPI.Tranportation.ts <- ts(monthly$PPI_Transportation_Equipment, start = c(2000,1),frequency = 12)
PPI.Machinery.ts <- ts(monthly$PPI_Machinery_Equipment, start = c(2000,1),frequency = 12)

# -------------------------------------------------------------------------------------------
# CONVERT W/ XTS
# Here we convert the data we just transformed to time series and make is an xts object

y.xts <- xts(daily$WILLREITIND,order.by = daily$date)
S5UTIL.xts <- xts(daily$S5UTIL,order.by = daily$date)
S5CONS.xts <- xts(daily$S5CONS,order.by = daily$date)
S5COND.xts <- xts(daily$S5COND,order.by = daily$date)
Gold.xts <- xts(daily$Gold_Price,order.by = daily$date)

MortgageRate.xts <- xts(weekly$Mortage_Rate_30Year,order.by = weekly$date)

unemployment.xts <- xts(monthly$US_unemployment_rate,order.by = monthly$date)
PPI.petroleum.xts <- xts(monthly$PPI_Petroleun_and_Coal,order.by = monthly$date)
PPI.Lumber.xts <- xts(monthly$PPI_Lumber_Wood,order.by = monthly$date)
PPI.Tranportation.xts <- xts(monthly$PPI_Transportation_Equipment,order.by = monthly$date)
PPI.Machinery.xts <- xts(monthly$PPI_Machinery_Equipment,order.by = monthly$date)

MortgageRate.365 <- na.locf(merge(MortgageRate.xts, foo=zoo(NA, order.by=seq(start(MortgageRate.xts), end(MortgageRate.xts),"day",drop=F)))[, 1])

unemployment.365 <- na.locf(merge(unemployment.xts, foo=zoo(NA, order.by=seq(start(unemployment.xts), end(unemployment.xts),"day",drop=F)))[, 1])
PPI.petroleum.365 <- na.locf(merge(PPI.petroleum.xts, foo=zoo(NA, order.by=seq(start(PPI.petroleum.xts), end(PPI.petroleum.xts),"day",drop=F)))[, 1])
PPI.Lumber.365 <- na.locf(merge(PPI.Lumber.xts, foo=zoo(NA, order.by=seq(start(PPI.Lumber.xts), end(PPI.Lumber.xts),"day",drop=F)))[, 1])
PPI.Tranportation.365 <- na.locf(merge(PPI.Tranportation.xts, foo=zoo(NA, order.by=seq(start(PPI.Tranportation.xts), end(PPI.Tranportation.xts),"day",drop=F)))[, 1])
PPI.Machinery.365 <- na.locf(merge(PPI.Machinery.xts, foo=zoo(NA, order.by=seq(start(PPI.Machinery.xts), end(PPI.Machinery.xts),"day",drop=F)))[, 1])

# -------------------------------------------------------------------------------------------
# LEFT JOIN ALL DATA
# an issue went up here when data in xts format have their date as indexes but not as a column within the data table,
# I could not left join these data with daily data by 'date', so I ended up exporting the CSV, added the dates and 
# merged all variables with vlookup function in Excel, and read it in again.

xts.df <- data.frame(merge(y.xts,S5UTIL.xts, join='left') %>% 
                       merge(.,S5CONS.xts,join ='left') %>%
                       merge(.,S5COND.ts,join ='left') %>%
                       merge(.,Gold.xts,join ='left') %>%
                       merge(.,MortgageRate.365,join ='left') %>%
                       merge(.,unemployment.365,join ='left') %>%
                       merge(.,PPI.petroleum.365,join ='left') %>%
                       merge(.,PPI.Lumber.365,join ='left') %>%
                       merge(.,PPI.Tranportation.365,join ='left') %>%
                       merge(.,PPI.Machinery.365,join ='left'))


# -------------------------------------------------------------------------------------------
# Manually combined all data into merged.csv
data <- read.csv("https://raw.githubusercontent.com/gstemmler/CS510Midterm/main/merged.csv", header = TRUE, stringsAsFactors = FALSE)
data <- data[1:4968,]
colnames(data) <- c("date","Y","S5UTIL","S5CONS","S5COND","GoldPrice","TreasuryBill3m","MortgageRate30y",
                    "UnemploymentRate","PPI.Petroleum","PPI.Lumber","PPI.Tranportation","PPI.Machinery",
                    "GovernmentBond","CPI","FederalDebt")

# -------------------------------------------------------------------------------------------
# STATIONARITY OF DATA

# To test stationary of data we use a augmented dickey-fueller test
# An ADF test tests the null hypothesis that a unit root is present in a time series sample.
# We take the adf tet of the normal data, then a test of the variables logged, and lastly an adf test of the difference of logs

## Adf test 
# Some adf test will come up with a warning message saying p - value is too small to show but this is good b/c we want a low p value
#S5COND has negatives/na's so we remove it

data_no_S5COND <- data[,-5]
for(i in 2:ncol(data_no_S5COND)){
  print(colnames(data_no_S5COND[i]))
  TS = ts(data_no_S5COND[,i], start = c(2000,1),frequency = 252.75)
  print(adf.test(TS))
}

## Adf test of loged variables
for(i in 2:ncol(data_no_S5COND)){
  print(colnames(data_no_S5COND[i]))
  TS_Log = ts(log(data_no_S5COND[,i]), start = c(2000,1),frequency = 252.75)
  print(adf.test(TS_Log))
}

## Adf test of diff of log
for(i in 2:ncol(data_no_S5COND)){
  print(colnames(data_no_S5COND[i]))
  TS_LogDiff = ts(diff(log(data_no_S5COND[,i])), start = c(2000,1),frequency = 252.75)
  print(adf.test(TS_LogDiff))
}

# -------------------------------------------------------------------------------------------
# APPEND Diff_Log of data into dataset, and run lm model
data$Y_DiffLog <- c(0,diff(log(data$Y)))
data$S5UTIL_DiffLog <- c(0,diff(log(data$S5UTIL)))
data$S5CONS_DiffLog <- c(0,diff(log(data$S5CONS)))
data$S5COND_dIFFLog <- c(0,diff(log(data$S5COND)))
data$GoldPrice_DiffLog <- c(0,diff(log(data$GoldPrice)))
data$TreasuryBill3m_DiffLog <- c(0,diff(log(data$TreasuryBill3m)))
data$MortgageRate30y_DiffLog <- c(0,diff(log(data$MortgageRate30y)))
data$UnemploymentRate_DiffLog <- c(0,diff(log(data$UnemploymentRate)))
data$PPI.Petroleum_DiffLog <- c(0,diff(log(data$PPI.Petroleum)))
data$PPI.Lumber_DiffLog <- c(0,diff(log(data$PPI.Lumber)))
data$PPI.Machinery_DiffLog <- c(0,diff(log(data$PPI.Machinery)))
data$PPI.Tranportation_DiffLog <- c(0,diff(log(data$PPI.Tranportation)))
data$GovernmentBond_DiffLog <- c(0,diff(log(data$GovernmentBond)))
data$CPI_DiffLog <- c(0,diff(log(data$CPI)))
data$FederalDebt_DiffLog <- c(0,diff(log(data$FederalDebt)))
adf.test(data$PPI.Lumber_DiffLog)
# -------------------------------------------------------------------------------------------
# look into regression models for our diff log data
# look into combionations and use the regsubsets function to find our best model

reg1a <- lm(data$Y_DiffLog ~ ., data[,17:31])
summary(reg1a)

plot(reg1a$residuals)

reg_all <- regsubsets(data$Y_DiffLog ~ ., data = data[,17:31], method=c("forward"))
regall_coef <- names(coef(reg_all, scale="adjr2",5))[-1] #get best variables without intercept
print(paste("selected variables:",list(regall_coef)))

reg2a <- lm(data$Y_DiffLog ~ data$S5UTIL_DiffLog + data$S5CONS_DiffLog + data$S5COND_dIFFLog + 
              data$UnemploymentRate_DiffLog + data$GoldPrice_DiffLog, data = data[,17:31])
summary(reg2a)

reg1b <- lm(data$Y_DiffLog ~ data$UnemploymentRate_DiffLog + data$GoldPrice_DiffLog, data = data[,17:31])
summary(reg1b)
# -------------------------------------------------------------------------------------------
# Now we're going to take a look into only monthly data and see if our models are better in this time format


data_diff_log <- data[,c(17:31)]

monthly.xts <- apply.monthly(xts.df, mean)

## all monthly data

monthdata_no_S5COND <- monthly.xts[,-c(4)]
for(i in 2:ncol(monthdata_no_S5COND)){
  print(colnames(monthdata_no_S5COND[i]))
  TS = ts(monthdata_no_S5COND[,i], start = c(2000,1),frequency = 252.75)
  TS = na.omit(TS)
  print(adf.test(TS))
}

## Adf test of loged variables
for(i in 2:ncol(monthdata_no_S5COND)){
  print(colnames(monthdata_no_S5COND[i]))
  TS_Log = na.omit(ts(log(monthdata_no_S5COND[,i]), start = c(2000,1),frequency = 252.75))
  print(adf.test(TS_Log))
}

## Adf test of diff of log
for(i in 2:ncol(monthdata_no_S5COND)){
  print(colnames(monthdata_no_S5COND[i]))
  TS_LogDiff = na.omit(ts(diff(log(monthdata_no_S5COND[,i])), start = c(2000,1),frequency = 252.75))
  print(adf.test(TS_LogDiff))
}

## Diff of Log of monthly data
monthly.xts$y.xts
monthly.xts$Y_DiffLog <- c(0,diff(log(monthly.xts$y.xts)))
monthly.xts$S5UTIL_DiffLog <- c(0,diff(log(monthly.xts$S5UTIL.xts)))
monthly.xts$S5CONS_DiffLog <- c(0,diff(log(monthly.xts$S5CONS.xts)))
monthly.xts$S5COND_dIFFLog <- c(0,diff(log(monthly.xts$S5COND.ts)))
monthly.xts$GoldPrice_DiffLog <- c(0,diff(log(monthly.xts$Gold.xts)))
monthly.xts$MortgageRate30y_DiffLog <- c(0,diff(log(monthly.xts$MortgageRate.xts)))
monthly.xts$UnemploymentRate_DiffLog <- c(0,diff(log(monthly.xts$unemployment.xts)))
monthly.xts$PPI.Petroleum_DiffLog <- c(0,diff(log(monthly.xts$PPI.petroleum.xts)))
monthly.xts$PPI.Lumber_DiffLog <- c(0,diff(log(monthly.xts$PPI.Lumber.xts)))
monthly.xts$PPI.Machinery_DiffLog <- c(0,diff(log(monthly.xts$PPI.Machinery.xts)))
monthly.xts$PPI.Tranportation_DiffLog <- c(0,diff(log(monthly.xts$PPI.Tranportation.xts)))

View(monthly.xts)
colnames(monthly.xts)
difflog_monthly.xts <- monthly.xts[,c(12:22)]

#Now we begin to rum regression models for monthly data
reg_monthly_a <- lm(difflog_monthly.xts$Y_DiffLog ~ ., difflog_monthly.xts)
summary(reg_monthly_a)

reg_all2 <- regsubsets(difflog_monthly.xts$Y_DiffLog ~ ., data = difflog_monthly.xts, method=c("forward"))
regall_coef2 <- names(coef(reg_all2, scale="adjr2",5))[-1] #get best variables without intercept
print(paste("selected variables:",list(regall_coef2)))

reg2a <- lm(data$Y_DiffLog ~ data$S5UTIL_DiffLog + data$S5CONS_DiffLog + data$S5COND_dIFFLog + 
              data$UnemploymentRate_DiffLog + data$GoldPrice_DiffLog, data = data)
summary(reg2a)

reg1b <- lm(difflog_monthly.xts$Y_DiffLog ~ ., data = difflog_monthly.xts)
summary(reg1b)

colnames(difflog_monthly.xts)
plot(reg_monthly_a$residuals)


#### 
####  Breusch-Pagan test ####
# Now we test our best monthly model using a Breusch-Pagan Test
# The Breusch-Pagan test is used to test for heteroskedasticity in a linear regression model

lmtest::bptest(reg_monthly_a)

#### Box-cox Transformation ####
# We now use a box-cox transformation on some of the variables
# A Box Cox transformation is a transformation of a non-normal dependent variables into a normal shape

distBCMod <- caret::BoxCoxTrans(monthly.xts$Y_DiffLog)
print(distBCMod)

# We put the transformed data back into dataset
data <- cbind(monthly.xts, dist_new=predict(distBCMod, monthly.xts$Y_DiffLog))
head(data)

# Run regression model again
lmMod_bc <- lm(data$dist_new ~ data$S5UTIL_DiffLog + data$S5CONS_DiffLog + data$S5COND_dIFFLog + 
                 data$UnemploymentRate_DiffLog + data$GoldPrice_DiffLog, data = data)

# Take Breusch-Pagan test agains
bptest(lmMod_bc)

par(mfrow = c(2,2))
plot(lmMod_bc)

