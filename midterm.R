#CS510 Midterm
#Author: Garrett Stemmler

# LOAD PACKAGES
library(xts)
library(zoo)
library(dplyr)
library(tseries)
library(leaps)

# -------------------------------------------------------------------------------------------
# LOAD and CLEAN DATA
daily <- read.csv("/Users/poochiestemmler/Documents/Fall\ 2020/CS510\ -\ Computing\ for\ Scientists/Midterm/daily.csv", header = TRUE, stringsAsFactors = FALSE)
daily$date <- as.Date(daily$date, "%Y-%m-%d")

weekly <- read.csv("/Users/poochiestemmler/Documents/Fall\ 2020/CS510\ -\ Computing\ for\ Scientists/Midterm/weekly.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(weekly)[1] <- "date"
colnames(weekly)[2] <- "Mortage_Rate_30Year"
weekly$date <- as.Date(weekly$date, "%Y-%m-%d")

monthly <- read.csv("/Users/poochiestemmler/Documents/Fall\ 2020/CS510\ -\ Computing\ for\ Scientists/Midterm/monthly.csv", header = TRUE, stringsAsFactors = FALSE)
monthly <- monthly[1:238,1:9]
monthly$date <- as.Date(monthly$date , "%Y-%m-%d")

#I cant seem to find this data set right now im still looking for is
quarterly <- read.csv("", header = TRUE, stringsAsFactors = FALSE)
quarterly <- quarterly[1:76,1:8]
colnames(quarterly)[1] <- "date"
quarterly$date <- as.Date(quarterly$date , "%Y-%m-%d")

# -------------------------------------------------------------------------------------------
# CONVERT W/ TS

y.ts<- ts(daily$WILLREITIND, start = c(2000,1,3), frequency = 252.75)
S5UTIL.ts <- ts(daily$S5UTIL, start = c(2000,1,3), frequency = 252.75)
S5CONS.ts <- ts(daily$S5CONS, start = c(2000,1,3), frequency = 252.75)
S5COND.ts <- ts(daily$S5COND, start = c(2000,1,3),frequency = 252.75)
Gold.ts <- ts(daily$Gold_Price, start = c(2000,1,3),frequency = 252.75)
TreasuryBill.ts <- ts(daily$Treasury_Bill_3M, start = c(2000,1,3), frequency = 252.75)

MortgageRate.ts <- ts(weekly$Mortage_Rate_30Year, start = c(1999,52), frequency = 52.15)

unemployment.ts <- ts(monthly$US_unemployment_rate, start = c(2000,1),frequency = 12)
PPI.petroleum.ts <- ts(monthly$PPI_Petroleun_and_Coal, start = c(2000,1),frequency = 12)
PPI.Lumber.ts <- ts(monthly$PPI_Lumber_Wood, start = c(2000,1),frequency = 12)
PPI.Tranportation.ts <- ts(monthly$PPI_Transportation_Equipment, start = c(2000,1),frequency = 12)
PPI.Machinery.ts <- ts(monthly$PPI_Machinery_Equipment, start = c(2000,1),frequency = 12)
Gov_Bond.ts <- ts(monthly$LongTerm_Gov_Bond_Yields10Year, start = c(2000,1), frequency = 12)
CPI.ts <- ts(monthly$CPIAUCSL, start = c(2000,1),frequency = 12)
FederalDebt.ts <- ts(monthly$Gross_Federal_Debt_Market_Value, start = c(2000,1), frequency=12)

# -------------------------------------------------------------------------------------------
# CONVERT W/ XTS
y.xts <- xts(daily$WILLREITIND,order.by = daily$date)
S5UTIL.xts <- xts(daily$S5UTIL,order.by = daily$date)
S5CONS.xts <- xts(daily$S5CONS,order.by = daily$date)
S5COND.xts <- xts(daily$S5COND,order.by = daily$date)
Gold.xts <- xts(daily$Gold_Price,order.by = daily$date)
TreasuryBill.xts <- xts(daily$Treasury_Bill_3M,order.by = daily$date) 

MortgageRate.xts <- xts(weekly$Mortage_Rate_30Year,order.by = weekly$date)

unemployment.xts <- xts(monthly$US_unemployment_rate,order.by = monthly$date)
PPI.petroleum.xts <- xts(monthly$PPI_Petroleun_and_Coal,order.by = monthly$date)
PPI.Lumber.xts <- xts(monthly$PPI_Lumber_Wood,order.by = monthly$date)
PPI.Tranportation.xts <- xts(monthly$PPI_Transportation_Equipment,order.by = monthly$date)
PPI.Machinery.xts <- xts(monthly$PPI_Machinery_Equipment,order.by = monthly$date)
Gov_Bond.xts <- xts(monthly$LongTerm_Gov_Bond_Yields10Year,order.by = monthly$date)
CPI.xts <- xts(monthly$CPIAUCSL,order.by = monthly$date)
FederalDebt.xts <- xts(monthly$Gross_Federal_Debt_Market_Value,order.by = monthly$date)

MortgageRate.365 <- na.locf(merge(MortgageRate.xts, foo=zoo(NA, order.by=seq(start(MortgageRate.xts), end(MortgageRate.xts),"day",drop=F)))[, 1])

unemployment.365 <- na.locf(merge(unemployment.xts, foo=zoo(NA, order.by=seq(start(unemployment.xts), end(unemployment.xts),"day",drop=F)))[, 1])
PPI.petroleum.365 <- na.locf(merge(PPI.petroleum.xts, foo=zoo(NA, order.by=seq(start(PPI.petroleum.xts), end(PPI.petroleum.xts),"day",drop=F)))[, 1])
PPI.Lumber.365 <- na.locf(merge(PPI.Lumber.xts, foo=zoo(NA, order.by=seq(start(PPI.Lumber.xts), end(PPI.Lumber.xts),"day",drop=F)))[, 1])
PPI.Tranportation.365 <- na.locf(merge(PPI.Tranportation.xts, foo=zoo(NA, order.by=seq(start(PPI.Tranportation.xts), end(PPI.Tranportation.xts),"day",drop=F)))[, 1])
PPI.Machinery.365 <- na.locf(merge(PPI.Machinery.xts, foo=zoo(NA, order.by=seq(start(PPI.Machinery.xts), end(PPI.Machinery.xts),"day",drop=F)))[, 1])
Gov_Bond.365 <- na.locf(merge(Gov_Bond.xts, foo=zoo(NA, order.by=seq(start(Gov_Bond.xts), end(Gov_Bond.xts),"day",drop=F)))[, 1])
CPI.365 <- na.locf(merge(CPI.xts, foo=zoo(NA, order.by=seq(start(CPI.xts), end(CPI.xts),"day",drop=F)))[, 1])
FederalDebt.365 <- na.locf(merge(FederalDebt.xts, foo=zoo(NA, order.by=seq(start(FederalDebt.xts), end(FederalDebt.xts),"day",drop=F)))[, 1])

# -------------------------------------------------------------------------------------------
# LEFT JOIN ALL DATA
# an issue went up here when data in xts format have their date as indexes but not as a column within the data table,
# I could not left join these data with daily data by 'date', so I ended up exporting the CSV, added the dates and 
# merged all variables with vlookup function in Excel, and read it in again.

xts.df <- data.frame(merge(y.xts,S5UTIL.xts, join='left') %>% 
                       merge(.,S5CONS.xts,join ='left') %>%
                       merge(.,S5COND.ts,join ='left') %>%
                       merge(.,Gold.xts,join ='left') %>%
                       merge(.,TreasuryBill.xts,join ='left') %>%
                       merge(.,MortgageRate.365,join ='left') %>%
                       merge(.,unemployment.365,join ='left') %>%
                       merge(.,PPI.petroleum.365,join ='left') %>%
                       merge(.,PPI.Lumber.365,join ='left') %>%
                       merge(.,PPI.Tranportation.365,join ='left') %>%
                       merge(.,PPI.Machinery.365,join ='left') %>% 
                       merge(.,Gov_Bond.365,join ='left') %>% 
                       merge(.,CPI.365,join ='left') %>% 
                       merge(.,FederalDebt.365,join ='left'))

#This could be downloaded from my Github page
#write.csv(xts.df,"C:/Users/6o8an/OneDrive/Documents/ECON452/ers/xts.csv", row.names = FALSE)

# -------------------------------------------------------------------------------------------
# Manually combined all data into merged.csv
data <- read.csv("/Users/poochiestemmler/Documents/Fall\ 2020/CS510\ -\ Computing\ for\ Scientists/Midterm/merged.csv", header = TRUE, stringsAsFactors = FALSE)
data <- data[1:4968,]
colnames(data) <- c("date","Y","S5UTIL","S5CONS","S5COND","GoldPrice","TreasuryBill3m","MortgageRate30y",
                    "UnemploymentRate","PPI.Petroleum","PPI.Lumber","PPI.Tranportation","PPI.Machinery",
                    "GovernmentBond","CPI","FederalDebt")

# -------------------------------------------------------------------------------------------
# STATIONARITY OF DATA
#TsNames <- list()
#for(i in 1:ncol(data)) { 
#  variable_name <- paste(colnames(data[i]), "ts" ,sep = ".")
#  TsNames[[i]] <- variable_name
#}
#TsList = list()

## Adf test 
# S5COND had
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

library(xts)

data_diff_log <- data[,c(17:31)]

monthly.xts <- apply.monthly(xts.df, mean)

## all monthly data

monthdata_no_S5COND <- monthly.xts[,-c(4)]
for(i in 2:ncol(monthdata_no_S5COND)){
  print(colnames(monthdata_no_S5COND[i]))
  TS = ts(monthdata_no_S5COND[,i], start = c(2000,1),frequency = 252.75)
  print(adf.test(TS))
}

## Adf test of loged variables
for(i in 2:ncol(monthdata_no_S5COND)){
  print(colnames(monthdata_no_S5COND[i]))
  TS_Log = ts(log(monthdata_no_S5COND[,i]), start = c(2000,1),frequency = 252.75)
  print(adf.test(TS_Log))
}

## Adf test of diff of log
for(i in 2:ncol(monthdata_no_S5COND)){
  print(colnames(monthdata_no_S5COND[i]))
  TS_LogDiff = ts(diff(log(monthdata_no_S5COND[,i])), start = c(2000,1),frequency = 252.75)
  print(adf.test(TS_LogDiff))
}

## Diff of Log
monthly.xts$y.xts
monthly.xts$Y_DiffLog <- c(0,diff(log(monthly.xts$y.xts)))
monthly.xts$S5UTIL_DiffLog <- c(0,diff(log(monthly.xts$S5UTIL.xts)))
monthly.xts$S5CONS_DiffLog <- c(0,diff(log(monthly.xts$S5CONS.xts)))
monthly.xts$S5COND_dIFFLog <- c(0,diff(log(monthly.xts$S5COND.ts)))
monthly.xts$GoldPrice_DiffLog <- c(0,diff(log(monthly.xts$Gold.xts)))
monthly.xts$TreasuryBill3m_DiffLog <- c(0,diff(log(monthly.xts$TreasuryBill.xts)))
monthly.xts$MortgageRate30y_DiffLog <- c(0,diff(log(monthly.xts$MortgageRate.xts)))
monthly.xts$UnemploymentRate_DiffLog <- c(0,diff(log(monthly.xts$unemployment.xts)))
monthly.xts$PPI.Petroleum_DiffLog <- c(0,diff(log(monthly.xts$PPI.petroleum.xts)))
monthly.xts$PPI.Lumber_DiffLog <- c(0,diff(log(monthly.xts$PPI.Lumber.xts)))
monthly.xts$PPI.Machinery_DiffLog <- c(0,diff(log(monthly.xts$PPI.Machinery.xts)))
monthly.xts$PPI.Tranportation_DiffLog <- c(0,diff(log(monthly.xts$PPI.Tranportation.xts)))
monthly.xts$GovernmentBond_DiffLog <- c(0,diff(log(monthly.xts$Gov_Bond.xts)))
monthly.xts$CPI_DiffLog <- c(0,diff(log(monthly.xts$CPI.xts)))
monthly.xts$FederalDebt_DiffLog <- c(0,diff(log(monthly.xts$FederalDebt.xts)))


View(monthly.xts)
colnames(monthly.xts)
difflog_monthly.xts <- monthly.xts[,c(16:30)]
reg_monthly_a <- lm(difflog_monthly.xts$Y_DiffLog ~ ., difflog_monthly.xts)
summary(reg_monthly_a)

reg_all2 <- regsubsets(difflog_monthly.xts$Y_DiffLog ~ ., data = difflog_monthly.xts, method=c("forward"))
regall_coef2 <- names(coef(reg_all2, scale="adjr2",5))[-1] #get best variables without intercept
print(paste("selected variables:",list(regall_coef2)))

reg2a <- lm(data$Y_DiffLog ~ data$S5UTIL_DiffLog + data$S5CONS_DiffLog + data$S5COND_dIFFLog + 
              data$UnemploymentRate_DiffLog + data$GoldPrice_DiffLog, data = data[,17:31])
summary(reg2a)

reg1b <- lm(difflog_monthly.xts$Y_DiffLog ~ ., data = difflog_monthly.xts[,c(4:15)])
summary(reg1b)

colnames(difflog_monthly.xts)
plot(reg_monthly_a$residuals)

library(caret)
library(lattice)
library(ggplot2)
library(lmtest)
#### 
####  Breusch-Pagan test ####
lmtest::bptest(reg_monthly_a)

#### Box-cox Transformation ####
distBCMod <- caret::BoxCoxTrans(monthly.xts$Y_DiffLog)
print(distBCMod)

data <- cbind(monthly.xts, dist_new=predict(distBCMod, monthly.xts$Y_DiffLog))
head(data)

lmMod_bc <- lm(data$dist_new ~ data$S5UTIL_DiffLog + data$S5CONS_DiffLog + data$S5COND_dIFFLog + 
                 data$UnemploymentRate_DiffLog + data$GoldPrice_DiffLog, data = data[,17:31])
bptest(lmMod_bc)

par(mfrow = c(2,2))
plot(lmMod_bc)
#################################### K-FOLDS ################################################
#------------------------------------------------
### k-fold Cross validation
#------------------------------------------------

### K-Fold Cross Validation
fold1 <- data_stationary[c(1:60),]
fold2 <- data_stationary[c(61:120),]
fold3 <- data_stationary[c(121:180),]
fold4 <- data_stationary[c(181:238),]

mod_fold1 <- lm(fold1$Y_DiffLog ~ ., data = fold1)
summary(mod_fold1)
mod_fold2 <- lm(fold2$Y_DiffLog ~., data = fold2)
summary(mod_fold2)
mod_fold3 <- lm(fold3$Y_DiffLog ~ ., data = fold2)
summary(mod_fold3)


preds_fold2 <- predict(mod_fold1, newdata = fold2)
preds_fold3 <- predict(mod_fold2, newdata = fold3)
preds_fold4 <- predict(mod_fold3, newdata = fold4)

RMSE(preds_fold2,fold1$Y_DiffLog)
RMSE(preds_fold3,fold2$Y_DiffLog)
RMSE(preds_fold4,fold3$Y_DiffLog)


