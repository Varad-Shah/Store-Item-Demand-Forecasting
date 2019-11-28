
############# VALIDATION ##################

train<-read.csv("C:\\Users\\Admin\\Desktop\\R Programming\\project\\train.csv", stringsAsFactors = F)
train[,1] = as.POSIXct(train[,1], format= "%Y-%m-%d")

reshaped.data1 <- reshape(train,  idvar =c('store', 'item'), direction = 'wide', timevar = 'date')
reshaped.data <- reshaped.data1[,c(1:1738)]

sesfunction <- function(x){
  model_ses <- ses(x[3:1738],h=90)
  sesdf=as.data.frame(model_ses)
  return(sesdf)
}

sesresult1<-apply(reshaped.data,1,sesfunction)
sesresult2<-reduce(sesresult1,bind_rows)
sesresult3<-round(sesresult2$`Point Forecast`)

sample_submission<-read.csv("C:\\Users\\Admin\\Desktop\\R Programming\\project\\sample_submission.csv", stringsAsFactors = F)
sessubmission<-cbind(sample_submission,sesresult3)
sessubmission<-sessubmission[,-2]
colnames(sessubmission)[2] <- "sales"
write.csv(sessubmission,"ses.csv",row.names=FALSE)

train2<-selectByDate(train,start="2017-10-3",end="2017-12-31")

newdata <- arrange(train2,store)
validate_data<- arrange(newdata,item)

MAE(validate_data$sales,sessubmission$sales) #[1] 11.6554
RMSE(validate_data$sales,sessubmission$sales) #[1] 15.2831
RMSLE(validate_data$sales,sessubmission$sales) #[1] 0.2821162
MAPE(validate_data$sales,sessubmission$sales) #[1] 26.69952
sMAPE(validate_data$sales,sessubmission$sales) #[1] 0.2212053

########################FORECASTING FOR 90 DAYS ###################

reshaped.data <- reshape(train,  idvar =c('store', 'item'), direction = 'wide', timevar = 'date')

sesfunction <- function(x){
  model_ses <- ses(x[3:1828],h=90)
  sesdf=as.data.frame(model_ses)
  return(sesdf)
}

sesresult1<-apply(reshaped.data,1,sesfunction)
sesresult2<-reduce(sesresult1,bind_rows)
sesresult3<-round(sesresult2$`Point Forecast`)

sessubmission<-cbind(sample_submission,sesresult3)
sessubmission<-sessubmission[,-2]
colnames(sessubmission)[2] <- "sales"
write.csv(sessubmission,"ses.csv",row.names=FALSE)
