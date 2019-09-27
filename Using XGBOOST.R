

train<-read.csv("C:\\Users\\Admin\\Desktop\\R Programming\\project\\train.csv", stringsAsFactors = F)
library(lubridate)
dat<- parse_date_time(x= train$date ,"ymd")
train$day<-day(dat)
train$month<-month(dat)
train$year<-year(dat)
train<-train[,-1]

strain <- model.matrix(  ~ ., data = train)
itrain <- strain[,-1]

##################### xgboost
library(caret)
set.seed(123)
intrain <- createDataPartition(y=train$sales,
                               p=0.7,list = FALSE)
X_train <- itrain[intrain,-3]
y_train <- itrain[intrain,3]

X_val <- itrain[-intrain,-3]
y_val <- itrain[-intrain,3]


library(xgboost)
model.xgb <- xgboost(data=X_train,label=y_train,booster = "gbtree",
                     nrounds = 50)
pred.xgb <- predict(model.xgb,newdata=X_val)
postResample(pred.xgb,obs = y_val)

###

test<-read.csv("C:\\Users\\Admin\\Desktop\\R Programming\\project\\test.csv", stringsAsFactors = F)
test<-test[,-1]
dae<- parse_date_time(x= test$date ,"ymd")
test$day<-day(dae)
test$month<-month(dae)
test$year<-year(dae)
test<-test[,-1]

stest <- model.matrix(  ~ ., data = test)
itest <- stest[,-1]

soln<- predict(model.xgb,newdata=itest)

write.csv(x = soln, file = "abc.csv")

solution<-read.csv("C:\\Users\\Admin\\Desktop\\R Programming\\project\\abc.csv")
solution<-round(solution$sales)

write.csv(x=solution,file="123.csv")





