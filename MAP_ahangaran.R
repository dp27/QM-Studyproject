ahangaran.elevation<-read.table("C:/Users/Darell/Desktop/Statistik MAP/ahangaran_irtash_elevation_bands_1000m.txt", sep = "", header = FALSE)
ahangaran.snow<-read.table("C:/Users/Darell/Desktop/Statistik MAP/cloud_snow_fraction_ahangaran_irtash_report.txt", sep = ";", header = FALSE)
ahangaran.drainage<-read.table("C:/Users/Darell/Desktop/Statistik MAP/ahangaran_irtash.txt", sep = "", header = FALSE)

ahangaran.snow <- ahangaran.snow[-c(6886:7237), ]

for (i in 1:nrow(ahangaran.snow)){
 if(ahangaran.snow$V2[i] <= 31) {
    ahangaran.snow[i, "Month"]<-1
 }
  else if(ahangaran.snow$V2[i] ==32 | ahangaran.snow$V2[i] <=59) {
   ahangaran.snow[i, "Month"]<-2
 }
  else if(ahangaran.snow$V2[i] ==60 | ahangaran.snow$V2[i] <= 90) {
   ahangaran.snow[i, "Month"]<-3
 }
  else if(ahangaran.snow$V2[i] ==91 | ahangaran.snow$V2[i] <= 120) {
   ahangaran.snow[i, "Month"]<-4
 }
  else if(ahangaran.snow$V2[i] ==121 | ahangaran.snow$V2[i] <= 151) {
   ahangaran.snow[i, "Month"]<-5
 }
  else if(ahangaran.snow$V2[i] ==152 | ahangaran.snow$V2[i] <= 181) {
   ahangaran.snow[i, "Month"]<-6
 }
  else if(ahangaran.snow$V2[i] ==182 | ahangaran.snow$V2[i] <= 212) {
   ahangaran.snow[i, "Month"]<-7
 }
  else if(ahangaran.snow$V2[i] ==213 | ahangaran.snow$V2[i] <= 243) {
   ahangaran.snow[i, "Month"]<-8
 }
  else if(ahangaran.snow$V2[i] ==244 | ahangaran.snow$V2[i] <= 273) {
   ahangaran.snow[i, "Month"]<-9
 }
  else if(ahangaran.snow$V2[i] ==274 | ahangaran.snow$V2[i] <= 304) {
   ahangaran.snow[i, "Month"]<-10
 }
  else if(ahangaran.snow$V2[i] ==305 | ahangaran.snow$V2[i] <= 334) {
   ahangaran.snow[i, "Month"]<-11
  }
  else{
   ahangaran.snow[i, "Month"]<-12
 }
}
names(ahangaran.snow)[1]<-paste("year.aha")
names(ahangaran.drainage)[1]<-paste("year.aha")
names(ahangaran.drainage)[2]<-paste("Month")
names(ahangaran.drainage)[3]<-paste("Q")
#data preperation for merge
##Obsolete

aha.total<-merge(ahangaran.snow,ahangaran.drainage,by=c("Month","year.aha"))
aha.total$V4[aha.total$V4 == -9]<- NA
aha.total$V5 <- NULL
omitted<-na.omit(aha.aha.total)

aha.Monthly.mean<-aggregate(aha.total[, 5:7], list(aha.total$Month), mean)
aha.year.ahaly.mean<-aggregate(aha.total[, 5:7], list(aha.total$year.aha), mean)

##data preperation for seasonal.aha mean
aha.total2<-aha.total
aha.total2$V5<-NULL
aha.total2$Month[aha.total2$Month == 1] <- NA
aha.total2$Month[aha.total2$Month == 2] <- NA
aha.total2$Month[aha.total2$Month == 3] <- NA
aha.total2$Month[aha.total2$Month == 10] <- NA
aha.total2$Month[aha.total2$Month == 11] <- NA
aha.total2$Month[aha.total2$Month == 12] <- NA
aha.total2$V4[aha.total2$V4 == -9]<- NA
omit_aha.total<- na.omit(aha.total2)
####Monatsdurchsnitte !HIER WEITERMACHEN|10/02/2020
april.year.aha<-omit_aha.total
april.year.aha$Month[april.year.aha$Month == 5] <- NA
april.year.aha$Month[april.year.aha$Month == 6] <- NA
april.year.aha$Month[april.year.aha$Month == 7] <- NA
april.year.aha$Month[april.year.aha$Month == 8] <- NA
april.year.aha$Month[april.year.aha$Month == 9] <- NA
april.year.aha <- na.omit(april.year.aha)
seasonal.aha.mean.april<- aggregate(april.year.aha[, 5:6], list(april.year.aha$year.aha), mean)
seasonal.aha.mean.april$Month<-c("April") 

may.year.aha<-omit_aha.total
may.year.aha$Month[may.year.aha$Month == 4] <- NA
may.year.aha$Month[may.year.aha$Month == 6] <- NA
may.year.aha$Month[may.year.aha$Month == 7] <- NA
may.year.aha$Month[may.year.aha$Month == 8] <- NA
may.year.aha$Month[may.year.aha$Month == 9] <- NA
may.year.aha <- na.omit(may.year.aha)
seasonal.aha.mean.may<- aggregate(may.year.aha[, 5:6], list(may.year.aha$year.aha), mean)
seasonal.aha.mean.may$Month<-c("May") 

june.year.aha<-omit_aha.total
june.year.aha$Month[june.year.aha$Month == 4] <- NA
june.year.aha$Month[june.year.aha$Month == 5] <- NA
june.year.aha$Month[june.year.aha$Month == 7] <- NA
june.year.aha$Month[june.year.aha$Month == 8] <- NA
june.year.aha$Month[june.year.aha$Month == 9] <- NA
june.year.aha <- na.omit(june.year.aha)
seasonal.aha.mean.june<- aggregate(june.year.aha[, 5:6], list(june.year.aha$year.aha), mean)
seasonal.aha.mean.june$Month<-c("June") 

july.year.aha<-omit_aha.total
july.year.aha$Month[july.year.aha$Month == 4] <- NA
july.year.aha$Month[july.year.aha$Month == 5] <- NA
july.year.aha$Month[july.year.aha$Month == 6] <- NA
july.year.aha$Month[july.year.aha$Month == 8] <- NA
july.year.aha$Month[july.year.aha$Month == 9] <- NA
july.year.aha <- na.omit(july.year.aha)
seasonal.aha.mean.july<- aggregate(july.year.aha[, 5:6], list(july.year.aha$year.aha), mean)
seasonal.aha.mean.july$Month<-c("July") 

august.year.aha<-omit_aha.total
august.year.aha$Month[august.year.aha$Month == 4] <- NA
august.year.aha$Month[august.year.aha$Month == 5] <- NA
august.year.aha$Month[august.year.aha$Month == 6] <- NA
august.year.aha$Month[august.year.aha$Month == 7] <- NA
august.year.aha$Month[august.year.aha$Month == 9] <- NA
august.year.aha <- na.omit(august.year.aha)
seasonal.aha.mean.august<- aggregate(august.year.aha[, 5:6], list(august.year.aha$year.aha), mean)
seasonal.aha.mean.august$Month<-c("August") 

september.year.aha<-omit_aha.total
september.year.aha$Month[september.year.aha$Month == 4] <- NA
september.year.aha$Month[september.year.aha$Month == 5] <- NA
september.year.aha$Month[september.year.aha$Month == 6] <- NA
september.year.aha$Month[september.year.aha$Month == 7] <- NA
september.year.aha$Month[september.year.aha$Month == 8] <- NA
september.year.aha <- na.omit(september.year.aha)
seasonal.aha.mean.september<- aggregate(september.year.aha[, 5:6], list(september.year.aha$year.aha), mean)
seasonal.aha.mean.september$Month<-c("September") 

year.ahaly.mean.Month<- rbind(seasonal.aha.mean.april,seasonal.aha.mean.may,seasonal.aha.mean.june,seasonal.aha.mean.july,seasonal.aha.mean.august,seasonal.aha.mean.september)
#Cbind aber davor Group1 umbennen zu den jeweiligen Monatsnamen
seasonal.aha.mean.year<- aggregate(omit_aha.total[, 5:6], list(omit_aha.total$year.aha), mean)
seasonal.aha.mean.Month<- aggregate(omit_aha.total[, 5:6], list(omit_aha.total$Month), mean)

##model selection
library(ggplot2)
model.aha.total <- lm(data = omit_aha.total, Q~V4)
model.seasonal.aha.year.aha<-lm(data = omit_aha.total, Q ~ V4)
model.seasonal.aha.Month <-lm(data = seasonal.aha.mean.Month, Q ~ V4)
##model.seasonal.aha.Month is the good one
##ab hier neu 25/02/2020
lm.aha.september <- lm(data = september.year.aha, Q~V4)
lm.aha.august <- lm(data = august.year.aha, Q~V4)
lm.aha.july <- lm(data = july.year.aha, Q~V4)
lm.aha.june <- lm(data = june.year.aha, Q~V4)
lm.aha.may <- lm(data = may.year.aha, Q~V4)
lm.aha.april <- lm(data = april.year.aha, Q~V4)
lm.aha.vp <- lm(data = omit_aha.total, Q~V4)

library(tidyr)
library(dplyr)
prediction.aha.september <- lm.aha.september %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(september.year.aha$year.aha))
mean.p.september.aha<-aggregate(prediction.aha.september[, 1:3], list(prediction.aha.september$x), mean)
mean.p.september.aha$month<-c("September")

prediction.aha.august <- lm.aha.august %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(august.year.aha$year.aha))
mean.p.august.aha<-aggregate(prediction.aha.august[, 1:3], list(prediction.aha.august$x), mean)
mean.p.august.aha$month<-c("August")

prediction.aha.july <- lm.aha.july %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(july.year.aha$year.aha))
mean.p.july.aha<-aggregate(prediction.aha.july[, 1:3], list(prediction.aha.july$x), mean)
mean.p.july.aha$month<-c("July")

prediction.aha.june <- lm.aha.june %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(june.year.aha$year.aha))
mean.p.june.aha<-aggregate(prediction.aha.june[, 1:3], list(prediction.aha.june$x), mean)
mean.p.june.aha$month<-c("June")

prediction.aha.may <- lm.aha.may %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(may.year.aha$year.aha))
mean.p.may.aha<-aggregate(prediction.aha.may[, 1:3], list(prediction.aha.may$x), mean)
mean.p.may.aha$month<-c("May")

prediction.aha.april <- lm.aha.april %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(april.year.aha$year.aha))
mean.p.april.aha<-aggregate(prediction.aha.april[, 1:3], list(prediction.aha.april$x), mean)
mean.p.april.aha$month<-c("April")

prediction.aha.vp <- lm.aha.vp %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(omit_aha.total$year.aha))
aha.mean.p.vp<-aggregate(prediction.aha.vp[, 1:3], list(prediction.aha.vp$x), mean)##das ist richtig

pred.all.months.aha <- rbind(mean.p.april.aha,mean.p.may.aha,mean.p.june.aha,mean.p.july.aha,mean.p.august.aha,mean.p.september.aha)
mean.pred.month.aha <- aggregate(pred.all.months.aha[, 2:4], list(pred.all.months.aha$month), mean)

p.aha<-ggplot(seasonal.aha.mean.year, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = aha.mean.p.vp, aes(x = Group.1, y = x, col = "prediction")) + xlab("year")
p.aha
summary(lm.aha.vp)
ggplot(seasonal.mean.aha.year, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = prediction.ahaseptember, aes(x = x, y = fit, col = "september"))+ geom_line(data = prediction.ahaaugust, aes(x = x, y = fit, col = "august")) + geom_line(data = prediction.ahajuly, aes(x = x, y = fit, col = "july")) +geom_line(data = prediction.ahajune, aes(x = x, y = fit, col = "june"))

plot(seasonal.aha.mean.april$V4, seasonal.aha.mean.april$Q)

####
ggplot(seasonal.aha.mean.year, aes(x = V4, y = Q))  + geom_point()

ggplot(seasonal.aha.mean.Month, aes(x = V4, y= Q, col= Group.1)) + geom_point() + geom_abline(slope = 0.9439, intercept = 19.3847)
ggplot(seasonal.aha.mean.year, aes(x = Group.1, y = Q)) + geom_line() + geom_point()
ggplot(year.ahaly.mean.Month, aes(x = Group.1, y = Q)) + geom_line(aes(col = Month)) + geom_point(aes(col = Month))
p.line.aha<-ggplot(omit_aha.total, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.84913, intercept = 20.98355 , col = "red")

p.aha<-ggplot(seasonal.aha.mean.year, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = aha.mean.p.vp, aes(x = Group.1, y = x, col = "prediction")) + xlab("year")
aha.year.p<-ggplot(seasonal.aha.mean.year, aes(x = Group.1, y = Q)) + geom_line() + geom_point()
aha.year.p
seasonal.aha.mean.year$Area <- c("ahangaran_irtash")
