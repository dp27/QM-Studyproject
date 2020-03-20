drainage<-read.table("C:/Users/Darell/Desktop/Statistik MAP/amudarya_kerki_formatted.txt", sep = "", header = TRUE)
snow<-read.table("C:/Users/Darell/Desktop/Statistik MAP/amudarya_kerki_snow_cloud_frac.txt", sep = ";", header = FALSE)


snow$month<-rep(rep(c(1,2,3,4,5,6,7,8,9,10,11,12), times = c(31,28,31,30,31,30,31,31,30,31,30,31)),19)
names(snow)[1]<-paste("year") 

snow2<-snow
snow2$V5<-NULL
snow2$month[snow2$month == 1] <- NA
snow2$month[snow2$month == 2] <- NA
snow2$month[snow2$month == 4] <- NA
snow2$month[snow2$month == 5] <- NA
snow2$month[snow2$month == 6] <- NA
snow2$month[snow2$month == 7] <- NA
snow2$month[snow2$month == 8] <- NA
snow2$month[snow2$month == 9] <- NA
snow2$month[snow2$month == 10] <- NA
snow2$month[snow2$month == 11] <- NA
snow2$month[snow2$month == 12] <- NA
snow2$V4[snow2$V4 == -9]<- NA
omitted_snow<- na.omit(snow2)
omitted_snow2<-aggregate(omitted_snow[, 2:4], list(omitted_snow$year), mean)
names(omitted_snow2)[1]<-paste("year")
#data preperation for merge
##Obsolete

total<-merge(drainage,omitted_snow2,by=c("year"))
total$V4[total$V4 == -9]<- NA
total$V5 <- NULL
names(total)[2]<-paste("month")
omitted<-na.omit(total)

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
seasonal.mean.april<- aggregate(april.year[, 3:6], list(april.year$year), mean)
seasonal.mean.april$Month<-c("April") 

may.year<-omit_total
may.year$month[may.year$month == 4] <- NA
may.year$month[may.year$month == 6] <- NA
may.year$month[may.year$month == 7] <- NA
may.year$month[may.year$month == 8] <- NA
may.year$month[may.year$month == 9] <- NA
may.year <- na.omit(may.year)
seasonal.mean.may<- aggregate(may.year[, 3:6], list(may.year$year), mean)
seasonal.mean.may$Month<-c("May") 

june.year<-omit_total
june.year$month[june.year$month == 4] <- NA
june.year$month[june.year$month == 5] <- NA
june.year$month[june.year$month == 7] <- NA
june.year$month[june.year$month == 8] <- NA
june.year$month[june.year$month == 9] <- NA
june.year <- na.omit(june.year)
seasonal.mean.june<- aggregate(june.year[, 3:6], list(june.year$year), mean)
seasonal.mean.june$Month<-c("June") 

july.year<-omit_total
july.year$month[july.year$month == 4] <- NA
july.year$month[july.year$month == 5] <- NA
july.year$month[july.year$month == 6] <- NA
july.year$month[july.year$month == 8] <- NA
july.year$month[july.year$month == 9] <- NA
july.year <- na.omit(july.year)
seasonal.mean.july<- aggregate(july.year[, 3:6], list(july.year$year), mean)
seasonal.mean.july$Month<-c("July") 

august.year<-omit_total
august.year$month[august.year$month == 4] <- NA
august.year$month[august.year$month == 5] <- NA
august.year$month[august.year$month == 6] <- NA
august.year$month[august.year$month == 7] <- NA
august.year$month[august.year$month == 9] <- NA
august.year <- na.omit(august.year)
seasonal.mean.august<- aggregate(august.year[, 3:6], list(august.year$year), mean)
seasonal.mean.august$Month<-c("August") 

september.year<-omit_total
september.year$month[september.year$month == 4] <- NA
september.year$month[september.year$month == 5] <- NA
september.year$month[september.year$month == 6] <- NA
september.year$month[september.year$month == 7] <- NA
september.year$month[september.year$month == 8] <- NA
september.year <- na.omit(september.year)
seasonal.mean.september<- aggregate(september.year[, 3:6], list(september.year$year), mean)
seasonal.mean.september$Month<-c("September") 

yearly.mean.month<- rbind(seasonal.mean.april,seasonal.mean.may,seasonal.mean.june,seasonal.mean.july,seasonal.mean.august,seasonal.mean.september)
##model von yearly mean month machen und plotten

#Cbind aber davor Group1 umbennen zu den jeweiligen Monatsnamen
seasonal.mean.year<- aggregate(omit_total[, 3:6], list(omit_total$year), mean)
seasonal.mean.year$name <- c("amu")
seasonal.mean.month<- aggregate(omit_total[, 3:6], list(omit_total$month), mean)

##model selection
library(ggplot2)

lm.months <- lm(data = yearly.mean.month, Q ~ V4)
amu_line_p <-ggplot(yearly.mean.month, aes(x=V4, y = Q)) + geom_point() + geom_abline(slope = -24.388, intercept = 2193.698, col="red") 
amu_line_p
summary(lm.months) ##verwenden

##Diese Modelle für uncertainty bands mit average seasonal discharge
##Ab hier in die anderen Datensätze kopieren und einfügen 25/02/2020
lm.september <- lm(data = september.year, Q~V4)
lm.august <- lm(data = august.year, Q~V4)
lm.july <- lm(data = july.year, Q~V4)
lm.june <- lm(data = june.year, Q~V4)
lm.may <- lm(data = may.year, Q~V4)
lm.april <- lm(data = april.year, Q~V4)
lm.vp <- lm(data = omit_total, Q~V4)
summary(lm.vp)
library(ggplot2) 
library(tidyr)
library(dplyr)
pb.september <- lm.september %>%
  predict(., interval = 'prediction') %>%
  as.data.frame() %>% mutate(x = sample(september.year$year))
mean.pb.september<-aggregate(prediction.september[, 1:3], list(prediction.september$x), mean)
mean.pb.september$month<-c("September")

pb.august <- lm.august %>%
  predict(., interval = 'prediction') %>%
  as.data.frame() %>% mutate(x = sample(august.year$year))
mean.pb.august<-aggregate(prediction.august[, 1:3], list(prediction.august$x), mean)
mean.pb.august$month<-c("august")

pb.july <- lm.july %>%
  predict(., interval = 'prediction') %>%
  as.data.frame() %>% mutate(x = sample(july.year$year))
mean.pb.july<-aggregate(prediction.july[, 1:3], list(prediction.july$x), mean)
mean.pb.july$month<-c("july")

pb.june <- lm.june %>%
  predict(., interval = 'prediction') %>%
  as.data.frame() %>% mutate(x = sample(june.year$year))
mean.pb.june<-aggregate(prediction.june[, 1:3], list(prediction.june$x), mean)
mean.pb.june$month<-c("june")

pb.may <- lm.may %>%
  predict(., interval = 'prediction') %>%
  as.data.frame() %>% mutate(x = sample(may.year$year))
mean.pb.may<-aggregate(prediction.may[, 1:3], list(prediction.may$x), mean)
mean.pb.may$month<-c("may")

pb.april <- lm.april %>%
  predict(., interval = 'prediction') %>%
  as.data.frame() %>% mutate(x = sample(april.year$year))
mean.pb.april<-aggregate(prediction.april[, 1:3], list(prediction.april$x), mean)
mean.pb.april$month<-c("april")


##confidence bands
prediction.september <- lm.september %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(september.year$year))
mean.p.september<-aggregate(prediction.september[, 1:3], list(prediction.september$x), mean)
mean.p.september$month<-c("September")

prediction.august <- lm.august %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(august.year$year))
mean.p.august<-aggregate(prediction.august[, 1:3], list(prediction.august$x), mean)
mean.p.august$month<-c("August")

prediction.july <- lm.july %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(july.year$year))
mean.p.july<-aggregate(prediction.july[, 1:3], list(prediction.july$x), mean)
mean.p.july$month<-c("July")

prediction.june <- lm.june %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(june.year$year))
mean.p.june<-aggregate(prediction.june[, 1:3], list(prediction.june$x), mean)
mean.p.june$month<-c("June")

prediction.may <- lm.may %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(may.year$year))
mean.p.may<-aggregate(prediction.may[, 1:3], list(prediction.may$x), mean)
mean.p.may$month<-c("May")

prediction.april <- lm.april %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(april.year$year))
mean.p.april<-aggregate(prediction.april[, 1:3], list(prediction.april$x), mean)
mean.p.april$month<-c("April")

prediction.vp <- lm.vp %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(omit_total$year))
mean.p.vp<-aggregate(prediction.vp[, 1:3], list(prediction.vp$x), mean)##das ist richtig

pred.all.months <- rbind(mean.p.april,mean.p.may,mean.p.june,mean.p.july,mean.p.august,mean.p.september)
mean.pred.month <- aggregate(pred.all.months[, 2:4], list(pred.all.months$month), mean)

p.amu.july<-ggplot(seasonal.mean.july, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.july, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year")
p.amu.july

p.amu<-ggplot(seasonal.mean.year, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.vp, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year")
p.amu
summary(lm.vp)
ggplot(seasonal.mean.year, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = prediction.september, aes(x = x, y = fit, col = "september"))+ geom_line(data = prediction.august, aes(x = x, y = fit, col = "august")) + geom_line(data = prediction.july, aes(x = x, y = fit, col = "july")) +geom_line(data = prediction.june, aes(x = x, y = fit, col = "june"))

?geom_ribbon()
plot(seasonal.mean.april$V4, seasonal.mean.april$Q)
##bis hier kopieren und dann einfügen
##model.seasonal.month is the good one
####
ggplot(seasonal.mean.year, aes(x = Group.1, y = Q)) + geom_line() + geom_point()
ggplot(yearly.mean.month, aes(x = Group.1, y = Q)) + geom_line(aes(col = Month)) + geom_point(aes(col = Month))
p.line.amu<-ggplot(omit_total, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = -23.548, intercept = 2209.961, col = 2)

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