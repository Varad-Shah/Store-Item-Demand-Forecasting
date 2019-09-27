train<-read.csv("C:\\Users\\Admin\\Desktop\\R Programming\\project\\train.csv", stringsAsFactors = F)
train[,1] = as.POSIXct(train[,1], format= "%Y-%m-%d")

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