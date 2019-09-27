library(fpp2)
library(forecast)
library(reshape2)
library(lubridate)
library(purrr)
library(dplyr)

train<-read.csv("C:\\Users\\Admin\\Desktop\\R Programming\\project\\train.csv", stringsAsFactors = F)
train[,1] = as.POSIXct(train[,1], format= "%Y-%m-%d")

reshaped.data <- reshape(train,  idvar =c('store', 'item'), direction = 'wide', timevar = 'date')

arimafunction <- function(x){
  model_arima <- auto.arima(x[3:1828])
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
write.csv(submission,"autoarima.csv",row.names=FALSE)
