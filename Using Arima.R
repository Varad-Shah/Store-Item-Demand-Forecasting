library(fpp2)
library(forecast)
library(reshape2)
library(lubridate)
library(purrr)
library(dplyr)
library(MLmetrics)
library(openair)
library(tidyverse)
library(TSPred)

################################# FINDING METRICS OF THE MODEL #####################

train<-read.csv("C:\\Users\\Admin\\Desktop\\R Programming\\project\\train.csv", stringsAsFactors = F)
train[,1] = as.POSIXct(train[,1], format= "%Y-%m-%d")

reshaped.data1 <- reshape(train,  idvar =c('store', 'item'), direction = 'wide', timevar = 'date')
ncol(reshaped.data1)
reshaped.data <- reshaped.data1[,c(1:1738)] ## REMOVING LAST 90 DAYS FROM TRAIN DATA, i.e. 1828 - 90 = 1738

arimafunction <- function(x){
  model_arima <- auto.arima(x[3:1738])
  pred.arima <- forecast(model_arima,h=90)
  df=as.data.frame(pred.arima)
  return(df)
  }

result1<-apply(reshaped.data,1,arimafunction)
result2<-reduce(result1,bind_rows)
result3<-round(result2$`Point Forecast`)

sample_submission<-read.csv("C:\\Users\\Admin\\Desktop\\R Programming\\project\\sample_submission.csv", stringsAsFactors = F)
submission<-cbind(sample_submission,result3)
submission<-submission[,-2]
colnames(submission)[2] <- "sales"

train2<-selectByDate(train,start="2017-10-3",end="2017-12-31") ## SELECTING LAST 90 DAYS FROM TRAINING DATA

newdata <- arrange(train2,store)
validate_data<- arrange(newdata,item) ## ARRANGING VALIDATION DATA IN AN ORDER SIMILAR TO AS RETURNED BY ARIMA

MAE(validate_data$sales,submission$sales)   # [1] 11.39836
RMSE(validate_data$sales,submission$sales)  # [1] 14.94032
MAPE(validate_data$sales,submission$sales)  # [1] 26.11868
RMSLE(validate_data$sales,submission$sales) # [1] 0.278096
sMAPE(validate_data$sales,submission$sales)  # [1] 0.2178649

################################## PREDICTING FOR NEXT 90 DAYS ######################

arimafunction <- function(x){
  model_arima <- auto.arima(x[3:1828])
  pred.arima <- forecast(model_arima,h=90)
  df=as.data.frame(pred.arima)
  return(df)
  }

result1<-apply(reshaped.data,1,arimafunction)
result2<-reduce(result1,bind_rows)
result3<-round(result2$`Point Forecast`)


submission<-cbind(sample_submission,result3)
submission<-submission[,-2]
colnames(submission)[2] <- "sales"
write.csv(submission,"autoarima.csv",row.names=FALSE)
