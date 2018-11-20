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

model_5 <- lm(output_remainingVolume~stock+volume+binStartPrice+binEndPrice,data =stock[, -1])

summary(model_5)
vif(model_5)

model_7 <- lm(volume~ stock+volume,data =stock[, -1])
summary(model_7)
vif(model_7)

stock_tvolume <- read.csv("file:///D:/RStudio/Hackthon/Sample_Testing_Volume_Data.csv")
View(stock_tvolume)
Stock <- stock_tvolume$stock
colSums(is.na(stock_tvolume))

stock_tvolume$volume[is.na(stock_tvolume$volume)]<-mean(stock_tvolume$volume, na.rm = T) #continuous variable so replacing it with mean
stock_tvolume$binStartPrice[is.na(stock_tvolume$binStartPrice)]<-mean(stock_tvolume$binStartPrice, na.rm = T) #continuous variable so replacing it with mean
stock_tvolume$binEndPrice[is.na(stock_tvolume$binEndPrice)]<-mean(stock_tvolume$binEndPrice, na.rm = T) #continuous variable so replacing it with mean
summary(stock_tvolume)
colSums(is.na(stock_tvolume))

fitted.results <- predict(model_5, newdata = stock_tvolume, type = 'response')
summary(fitted.results)
View(fitted.results)
output_remainingVolume <-fitted.results 

remainingVolume<-as.matrix(data.frame(stock_tvolume,output_remainingVolume))
View(remainingVolume)
remainingVolume1 <- as.data.frame(remainingVolume)

fitted.results3 <- predict(model_7, newdata = stock_tvolume, type = 'response')
summary(fitted.results3)
View(fitted.results3)
output_closeAuctionVolume <-fitted.results3 

volumePrediction<-as.matrix(data.frame(stock_tvolume,output_closeAuctionVolume))
View(volumePrediction)
summary(volumePrediction)
volumePrediction1 <- as.data.frame(volumePrediction)
volumePrediction <- sqldf("select v.date,v.stock,v.binNum,v.binStartTime,v.binEndTime,v1.output_remainingVolume,v.output_closeAuctionVolume 
                          from volumePrediction1 v,remainingVolume1 v1 
                          where  v.binNum = (select max(i.binNum) from volumePrediction1 i,remainingVolume1 i1  
                          where i1.date=i.date and i1.stock=i.stock) and v1.date=v.date and v1.stock=v.stock 
                          group by  v.date,v.stock,v.binNum,v.binStartTime,v.binEndTime,v.output_closeAuctionVolume")

View(volumePrediction)
