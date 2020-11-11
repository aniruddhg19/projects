data=read.csv(file.choose(),header=T)
dim(data)
head(data)
str(data)
summary(data)


#Deleting Store and Date Variables
data$Store=NULL
data$Date=NULL

#Assigning proper data type to variables
data$DayOfWeek <- as.factor(data$DayOfWeek)

#Checking for NA
sum(is.na(data)) 

# Total occurences of zero Sales (Target)
sum(data$Sales==0) 

library(dplyr)

#Deleting zero values 
data <- data%>%filter(Sales>0)
dim(data)

#Data manipulation 

data%>%group_by(Promo)%>%summarise(avg=mean(Sales))

data%>%group_by(SchoolHoliday)%>%summarise(avg_on_holiday=mean(Sales))

data%>%group_by(DayOfWeek)%>%summarise(Daily_Average=mean(Sales))

data%>%group_by(StateHoliday)%>%summarise(avg_by_holiday=mean(Sales))

data%>%group_by(Promo,DayOfWeek)%>%summarise(max_sales=max(Sales),median_of_customers=median(Customers))%>%
  arrange(desc(max_sales))

#Data Visualization

#Histograms for Sales and Customers

hist(data$Sales,xlim = c(0,25000),col='orange',main='Histogram of Sales',xlab='Sales',
     ylab='Frequency')

hist(data$Customers,xlim=c(0,2700),col='turquoise1',main='Histogram of Customers',
     xlab='No. of Customers',ylab='Frequency')

#Scatter plot of Sales vs Customers

library(ggplot2)
ggplot(data)+geom_point(mapping=aes(x=Sales,y=Customers),position = 'jitter',colour='blue')+
  theme_classic()+labs(x='Sales',y='Customers',title="Scatter plot of  Customers vs Sales")

#Correlation 
cor(data$Customers,data$Sales) # 0.823

#Data Partition into training and test
set.seed(111)
idx <- sample(2,nrow(data),prob=c(0.7,0.3),replace=T)
train <- data[idx==1,]
test <- data[idx==2,]
dim(train) 
dim(test)

#One hot encoding for training set
data_ohe <-as.data.frame(model.matrix(~.-1,data=train))
ohe_label <- data_ohe[,'Sales'] # Target variable - Sales

#One hot encoding for test set
test_ohe <- as.data.frame(model.matrix(~.-1,data=test))
test_label <- test_ohe[,'Sales']

#Xgboost Model

library(xgboost)

dtrain <- xgb.DMatrix(as.matrix(data_ohe%>%select(-Sales)),label=ohe_label)
dtest <- xgb.DMatrix(as.matrix(test_ohe%>%select(-Sales)),label=test_label)

# Modelling data in xgboost
set.seed(500)

w <- list(train=dtrain,test= dtest)

xgb_model1 <- xgb.train(data=dtrain,booster='gbtree',nrounds=500,max_depth=3,eval_metric='rmse',
                      eta=0.1,watchlist=w,early_stopping_rounds = 30) 

# [500] 	train-rmse:1481.239868	test-rmse:1486.935669 

xgb_model2 <- xgb.train(data=dtrain,booster='gbtree',nrounds=800,max_depth=6,eval_metric='rmse',
                  eta=0.1,watchlist=w,early_stopping_rounds = 30) 

# Stopping.  Best iteration:[95] train-rmse:1476.702759	test-rmse:1482.494141

xgb_model3 <- xgb.train(data=dtrain,booster='gbtree',nrounds=1000,max_depth=8,eval_metric='rmse',
                  eta=0.3,watchlist=w,early_stopping_rounds = 30)  

# Stopping.  [17]	train-rmse:1473.576050	test-rmse:1483.128174

xgb_model4 <- xgb.train(data=dtrain,booster='gbtree',nrounds=800,max_depth=6,eval_metric='rmse',
                  eta=1,watchlist=w,early_stopping_rounds = 100) 

# Stopping.  Best iteration:[9]	train-rmse:1482.701782	test-rmse:1490.353271

xgb_model5 <- xgb.train(data=dtrain,booster='gbtree',nrounds=500,max_depth=5,eval_metric='rmse',
                        eta=2,watchlist=w,early_stopping_rounds = 100) 

#  [500]	train-rmse:7078.938477	test-rmse:7069.501465 

xgb_model6 <- xgb.train(data=dtrain,booster='gbtree',nrounds=800,max_depth=6,eval_metric='rmse',
                        eta=0.15,watchlist=w,early_stopping_rounds = 100)

# Stopping.  Best iteration:[57] train-rmse:1478.008667	test-rmse:1482.335938

xgb_model7 <- xgb.train(data=dtrain,booster='gbtree',nrounds=800,max_depth=6,eval_metric='rmse',
                        eta=0.12,watchlist=w,early_stopping_rounds = 100)

# Stopping.  Best iteration:[71] train-rmse:1478.297607	test-rmse:1482.491089

xgb_model8 <- xgb.train(data=dtrain,booster='gbtree',nrounds=800,max_depth=6,eval_metric='rmse',
                        eta=0.135,watchlist=w,early_stopping_rounds = 100)

# Stopping. Best iteration: [60]	train-rmse:1478.544678	test-rmse:1482.349609

#  Model 2 is the best model

best_model <- xgb.train(data=dtrain,booster='gbtree',nrounds=95,max_depth=6,eval_metric='rmse',
                      eta=0.1,watchlist=w) 


#Prediction for test set

pred_sales <- predict(best_model,newdata = dtest,class='response')
pred_sales <- round(pred_sales)
head(pred_sales,10)

[1]  6439  8727 12703  7311 10844  6296  6858  6727  4423  4846

#Feature importance

imp <- xgb.importance(colnames(dtrain),model=best_model)
xgb.plot.importance(imp)




