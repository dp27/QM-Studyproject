drainage<-read.table("C:/Users/Darell/Desktop/Statistik MAP/amudarya_kerki_formatted.txt", sep = "", header = TRUE)
snow<-read.table("C:/Users/Darell/Desktop/Statistik MAP/amudarya_kerki_snow_cloud_frac.txt", sep = ";", header = FALSE)


snow$month<-rep(rep(c(1,2,3,4,5,6,7,8,9,10,11,12), times = c(31,28,31,30,31,30,31,31,30,31,30,31)),19)
names(snow)[1]<-paste("year") 
#data preperation for merge
##Obsolete

total<-merge(snow,drainage,by=c("month","year"))
total$V4[total$V4 == -9]<- NA
total$V5 <- NULL
omitted<-na.omit(total)

monthly.mean<-aggregate(total[, 5:7], list(total$month), mean)
yearly.mean<-aggregate(total[, 5:7], list(total$year), mean)

##data preperation for seasonal mean
total2<-total
total2$V5<-NULL
total2$month[total2$month == 1] <- NA
total2$month[total2$month == 2] <- NA
total2$month[total2$month == 3] <- NA
total2$month[total2$month == 10] <- NA
total2$month[total2$month == 11] <- NA
total2$month[total2$month == 12] <- NA
total2$V4[total2$V4 == -9]<- NA
omit_total<- na.omit(total2)
####Monatsdurchsnitte !HIER WEITERMACHEN|10/02/2020
april.year<-omit_total
april.year$month[april.year$month == 5] <- NA
april.year$month[april.year$month == 6] <- NA
april.year$month[april.year$month == 7] <- NA
april.year$month[april.year$month == 8] <- NA
april.year$month[april.year$month == 9] <- NA
april.year <- na.omit(april.year)
seasonal.mean.april<- aggregate(april.year[, 5:6], list(april.year$year), mean)
seasonal.mean.april$Month<-c("April") 

may.year<-omit_total
may.year$month[may.year$month == 4] <- NA
may.year$month[may.year$month == 6] <- NA
may.year$month[may.year$month == 7] <- NA
may.year$month[may.year$month == 8] <- NA
may.year$month[may.year$month == 9] <- NA
may.year <- na.omit(may.year)
seasonal.mean.may<- aggregate(may.year[, 5:6], list(may.year$year), mean)
seasonal.mean.may$Month<-c("May") 

june.year<-omit_total
june.year$month[june.year$month == 4] <- NA
june.year$month[june.year$month == 5] <- NA
june.year$month[june.year$month == 7] <- NA
june.year$month[june.year$month == 8] <- NA
june.year$month[june.year$month == 9] <- NA
june.year <- na.omit(june.year)
seasonal.mean.june<- aggregate(june.year[, 5:6], list(june.year$year), mean)
seasonal.mean.june$Month<-c("June") 

july.year<-omit_total
july.year$month[july.year$month == 4] <- NA
july.year$month[july.year$month == 5] <- NA
july.year$month[july.year$month == 6] <- NA
july.year$month[july.year$month == 8] <- NA
july.year$month[july.year$month == 9] <- NA
july.year <- na.omit(july.year)
seasonal.mean.july<- aggregate(july.year[, 5:6], list(july.year$year), mean)
seasonal.mean.july$Month<-c("July") 

august.year<-omit_total
august.year$month[august.year$month == 4] <- NA
august.year$month[august.year$month == 5] <- NA
august.year$month[august.year$month == 6] <- NA
august.year$month[august.year$month == 7] <- NA
august.year$month[august.year$month == 9] <- NA
august.year <- na.omit(august.year)
seasonal.mean.august<- aggregate(august.year[, 5:6], list(august.year$year), mean)
seasonal.mean.august$Month<-c("August") 

september.year<-omit_total
september.year$month[september.year$month == 4] <- NA
september.year$month[september.year$month == 5] <- NA
september.year$month[september.year$month == 6] <- NA
september.year$month[september.year$month == 7] <- NA
september.year$month[september.year$month == 8] <- NA
september.year <- na.omit(september.year)
seasonal.mean.september<- aggregate(september.year[, 5:6], list(september.year$year), mean)
seasonal.mean.september$Month<-c("September") 

yearly.mean.month<- rbind(seasonal.mean.april,seasonal.mean.may,seasonal.mean.june,seasonal.mean.july,seasonal.mean.august,seasonal.mean.september)

#Cbind aber davor Group1 umbennen zu den jeweiligen Monatsnamen
seasonal.mean.year<- aggregate(omit_total[, 5:6], list(omit_total$year), mean)
seasonal.mean.month<- aggregate(omit_total[, 5:6], list(omit_total$month), mean)

##model selection
library(ggplot2)
model.total <- lm(data = omit_total, Q~V4)
model.seasonal.year<-lm(data = omit_total, Q ~ V4)
model.seasonal.month <-lm(data = seasonal.mean.month, Q ~ V4)

##Diese Modelle für uncertainty bands mit average seasonal discharge
##Ab hier in die anderen Datensätze kopieren und einfügen 25/02/2020
lm.september <- lm(data = seasonal.mean.september, Q~V4)
lm.august <- lm(data = seasonal.mean.august, Q~V4)
lm.july <- lm(data = seasonal.mean.july, Q~V4)
lm.june <- lm(data = seasonal.mean.june, Q~V4)
lm.may <- lm(data = seasonal.mean.may, Q~V4)
lm.april <- lm(data = seasonal.mean.april, Q~V4)
lm.vp <- lm(data = seasonal.mean.year, Q~V4)
 
library(tidyr)
library(dplyr)
prediction.september <- lm.september %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(seasonal.mean.september$Group.1))

prediction.august <- lm.august %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(seasonal.mean.august$Group.1))

prediction.july <- lm.july %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(seasonal.mean.july$Group.1))

prediction.june <- lm.june %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(seasonal.mean.june$Group.1))

prediction.vp <- lm.vp %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(seasonal.mean.year$Group.1))

geom_line(data = prediction.july, aes(x = x, y = fit, col = "july"))

ggplot(seasonal.mean.year, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = prediction.vp, aes(x = x, y = fit, col = "prediction"))
ggplot(seasonal.mean.year, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = prediction.september, aes(x = x, y = fit, col = "september"))+ geom_line(data = prediction.august, aes(x = x, y = fit, col = "august")) + geom_line(data = prediction.july, aes(x = x, y = fit, col = "july")) +geom_line(data = prediction.june, aes(x = x, y = fit, col = "june"))

plot(seasonal.mean.april$V4, seasonal.mean.april$Q)
##bis hier kopieren und dann einfügen
##model.seasonal.month is the good one
####
ggplot(seasonal.mean.month, aes(x = V4, y= Q, col= Group.1)) + geom_point() + geom_abline(slope = -36.30, intercept = 2367.81)
ggplot(seasonal.mean.year, aes(x = Group.1, y = Q)) + geom_line() + geom_point()
ggplot(yearly.mean.month, aes(x = Group.1, y = Q)) + geom_line(aes(col = Month)) + geom_point(aes(col = Month))
ggplot(omit_total, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = -23.548, intercept = 2209.961, col = 2)

summary(model.seasonal.month)
summary(model.seasonal.year)
summary(model.total)

##########

#obsolete
snow[snow == -9] <- NA 
snow2000<-snow[1:365,] # select rows and columns
snow2001<-snow[366:730,]
snow2002<-snow[731:1095,]
snow2003<-snow[1096:1460,]
snow2004<-snow[1461:1825,]
snow2005<-snow[1826:2190,]
snow2006<-snow[2191:2555,]
snow2007<-snow[2556:2920,]
snow2008<-snow[2921:3285,]
snow2009<-snow[2826:3650,]
snow2010<-snow[3651:4015,]
snow2011<-snow[4016:4380,]
snow2012<-snow[4381:4745,]
snow2013<-snow[4746:5110,]
snow2014<-snow[5111:5475,]
snow2015<-snow[5476:5840,]
snow2016<-snow[5841:6205,]
snow2017<-snow[6206:6570,]
snow2018<-snow[6571:6935,]