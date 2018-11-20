library(ROCR)
library("fortunes")
library("VIF")
library("car")
library(sqldf)

stock <- read.csv("file:///D:/RStudio/Prediction_To_Close/Sample_Training_Data.csv")

View(stock)
summary(stock)
colSums(is.na(stock))


stock$volume<-as.numeric(stock$volume)
stock$binStartPrice<-as.numeric(stock$binStartPrice)
stock$binEndPrice<-as.numeric(stock$binEndPrice)
stock$day_lowPrice<-as.numeric(stock$day_lowPrice)
stock$day_highPrice<-as.numeric(stock$day_highPrice)
stock$day_openPrice<-as.numeric(stock$day_openPrice)
stock$day_closePrice<-as.numeric(stock$day_closePrice)

stock$volume[is.na(stock$volume)]<-mean(stock$volume, na.rm = T) #continuous variable so replacing it with mean
stock$binStartPrice[is.na(stock$binStartPrice)]<-mean(stock$binStartPrice, na.rm = T) #continuous variable so replacing it with mean
stock$binEndPrice[is.na(stock$binEndPrice)]<-mean(stock$binEndPrice, na.rm = T) #continuous variable so replacing it with mean
stock$day_lowPrice[is.na(stock$day_lowPrice)]<-mean(stock$day_lowPrice, na.rm = T) #continuous variable so replacing it with mean

colSums(is.na(stock))


model_6 <- lm(binEndPrice~ volume+day_openPrice+binEndPrice+binStartPrice+day_lowPrice+day_highPrice,data =stock[, -1])
summary(model_6)
vif(model_6)

stock_tprice1 <- read.csv("file:///D:/RStudio/Prediction_To_Close/Sample_Testing_Price_Data.csv")
View(stock_tprice1)
colSums(is.na(stock_tprice1))


stock_tprice1$volume[is.na(stock_tprice1$volume)]<-mean(stock_tprice1$volume, na.rm = T) #continuous variable so replacing it with mean
stock_tprice1$binStartPrice[is.na(stock_tprice1$binStartPrice)]<-mean(stock_tprice1$binStartPrice, na.rm = T) #continuous variable so replacing it with mean
stock_tprice1$binEndPrice[is.na(stock_tprice1$binEndPrice)]<-mean(stock_tprice1$binEndPrice, na.rm = T) #continuous variable so replacing it with mean
summary(stock_tprice1)
colSums(is.na(stock_tprice1))


fitted.results2 <- predict(model_6, newdata = stock_tprice1, type = 'response')
summary(fitted.results2)
View(fitted.results2)
day_closePrice<-fitted.results2 

pricPrediction<-as.matrix(data.frame(stock_tprice1,day_closePrice))
View(pricPrediction)
summary(pricPrediction)
pricPrediction <- as.data.frame(pricPrediction)

PriceDirection <-sqldf("select i.date,i.stock,i.binNum,i.binStartTime,i.binEndTime, (CASE WHEN i.day_closePrice > i1.binEndPrice THEN 1 WHEN i.day_closePrice < i1.binEndPrice THEN -1 else 0 END) AS output 
                        from stock_tprice1 i1,pricPrediction i where i1.date=i.date and i1.stock=i.stock 
                        and i.binNum = (select max(i.binNum) from pricPrediction i,stock_tprice1 i1 
                        where i1.date=i.date and i1.stock=i.stock) and i1.binNum =i.binNum-1")
View(PriceDirection)
