chirchik.snow<-read.table("C:/Users/Darell/Desktop/Statistik MAP/chirchik_snow_cloud_frac.txt", sep = ";", header = FALSE)
chirchik.drainage<-read.table("C:/Users/Darell/Desktop/Statistik MAP/pritok_chirchik1.txt", sep = "", header = FALSE)

chirchik.snow <- chirchik.snow[-c(6886:7237), ]

for (i in 1:nrow(chirchik.snow)){
  if(chirchik.snow$V2[i] <= 31) {
    chirchik.snow[i, "Month"]<-1
  }
  else if(chirchik.snow$V2[i] ==32 | chirchik.snow$V2[i] <=59) {
    chirchik.snow[i, "Month"]<-2
  }
  else if(chirchik.snow$V2[i] ==60 | chirchik.snow$V2[i] <= 90) {
    chirchik.snow[i, "Month"]<-3
  }
  else if(chirchik.snow$V2[i] ==91 | chirchik.snow$V2[i] <= 120) {
    chirchik.snow[i, "Month"]<-4
  }
  else if(chirchik.snow$V2[i] ==121 | chirchik.snow$V2[i] <= 151) {
    chirchik.snow[i, "Month"]<-5
  }
  else if(chirchik.snow$V2[i] ==152 | chirchik.snow$V2[i] <= 181) {
    chirchik.snow[i, "Month"]<-6
  }
  else if(chirchik.snow$V2[i] ==182 | chirchik.snow$V2[i] <= 212) {
    chirchik.snow[i, "Month"]<-7
  }
  else if(chirchik.snow$V2[i] ==213 | chirchik.snow$V2[i] <= 243) {
    chirchik.snow[i, "Month"]<-8
  }
  else if(chirchik.snow$V2[i] ==244 | chirchik.snow$V2[i] <= 273) {
    chirchik.snow[i, "Month"]<-9
  }
  else if(chirchik.snow$V2[i] ==274 | chirchik.snow$V2[i] <= 304) {
    chirchik.snow[i, "Month"]<-10
  }
  else if(chirchik.snow$V2[i] ==305 | chirchik.snow$V2[i] <= 334) {
    chirchik.snow[i, "Month"]<-11
  }
  else{
    chirchik.snow[i, "Month"]<-12
  }
}
names(chirchik.snow)[1]<-paste("year.chir")
names(chirchik.drainage)[1]<-paste("year.chir")
names(chirchik.drainage)[2]<-paste("Month")
names(chirchik.drainage)[3]<-paste("Q")
chirchik.drainage$V4<-NULL
#data preperation for merge
##Obsolete

chir.total<-merge(chirchik.snow,chirchik.drainage,by=c("Month","year.chir"))
chir.total$V4[chir.total$V4 == -9]<- NA
chir.total$V5 <- NULL
omitted<-na.omit(chir.total)

chir.Monthly.mean<-aggregate(chir.total[, 5:7], list(chir.total$Month), mean)
chir.year.chirly.mean<-aggregate(chir.total[, 5:7], list(chir.total$year.chir), mean)

##data preperation for seasonal.chir mean
chir.total2<-chir.total
chir.total2$V5<-NULL
chir.total2$Month[chir.total2$Month == 1] <- NA
chir.total2$Month[chir.total2$Month == 2] <- NA
chir.total2$Month[chir.total2$Month == 3] <- NA
chir.total2$Month[chir.total2$Month == 10] <- NA
chir.total2$Month[chir.total2$Month == 11] <- NA
chir.total2$Month[chir.total2$Month == 12] <- NA
chir.total2$V4[chir.total2$V4 == -9]<- NA
omit_chir.total<- na.omit(chir.total2)
####Monatsdurchsnitte !HIER WEITERMACHEN|10/02/2020
april.year.chir<-omit_chir.total
april.year.chir$Month[april.year.chir$Month == 5] <- NA
april.year.chir$Month[april.year.chir$Month == 6] <- NA
april.year.chir$Month[april.year.chir$Month == 7] <- NA
april.year.chir$Month[april.year.chir$Month == 8] <- NA
april.year.chir$Month[april.year.chir$Month == 9] <- NA
april.year.chir <- na.omit(april.year.chir)
seasonal.chir.mean.april<- aggregate(april.year.chir[, 5:6], list(april.year.chir$year.chir), mean)
seasonal.chir.mean.april$Month<-c("April") 

may.year.chir<-omit_chir.total
may.year.chir$Month[may.year.chir$Month == 4] <- NA
may.year.chir$Month[may.year.chir$Month == 6] <- NA
may.year.chir$Month[may.year.chir$Month == 7] <- NA
may.year.chir$Month[may.year.chir$Month == 8] <- NA
may.year.chir$Month[may.year.chir$Month == 9] <- NA
may.year.chir <- na.omit(may.year.chir)
seasonal.chir.mean.may<- aggregate(may.year.chir[, 5:6], list(may.year.chir$year.chir), mean)
seasonal.chir.mean.may$Month<-c("May") 

june.year.chir<-omit_chir.total
june.year.chir$Month[june.year.chir$Month == 4] <- NA
june.year.chir$Month[june.year.chir$Month == 5] <- NA
june.year.chir$Month[june.year.chir$Month == 7] <- NA
june.year.chir$Month[june.year.chir$Month == 8] <- NA
june.year.chir$Month[june.year.chir$Month == 9] <- NA
june.year.chir <- na.omit(june.year.chir)
seasonal.chir.mean.june<- aggregate(june.year.chir[, 5:6], list(june.year.chir$year.chir), mean)
seasonal.chir.mean.june$Month<-c("June") 

july.year.chir<-omit_chir.total
july.year.chir$Month[july.year.chir$Month == 4] <- NA
july.year.chir$Month[july.year.chir$Month == 5] <- NA
july.year.chir$Month[july.year.chir$Month == 6] <- NA
july.year.chir$Month[july.year.chir$Month == 8] <- NA
july.year.chir$Month[july.year.chir$Month == 9] <- NA
july.year.chir <- na.omit(july.year.chir)
seasonal.chir.mean.july<- aggregate(july.year.chir[, 5:6], list(july.year.chir$year.chir), mean)
seasonal.chir.mean.july$Month<-c("July") 

august.year.chir<-omit_chir.total
august.year.chir$Month[august.year.chir$Month == 4] <- NA
august.year.chir$Month[august.year.chir$Month == 5] <- NA
august.year.chir$Month[august.year.chir$Month == 6] <- NA
august.year.chir$Month[august.year.chir$Month == 7] <- NA
august.year.chir$Month[august.year.chir$Month == 9] <- NA
august.year.chir <- na.omit(august.year.chir)
seasonal.chir.mean.august<- aggregate(august.year.chir[, 5:6], list(august.year.chir$year.chir), mean)
seasonal.chir.mean.august$Month<-c("August") 

september.year.chir<-omit_chir.total
september.year.chir$Month[september.year.chir$Month == 4] <- NA
september.year.chir$Month[september.year.chir$Month == 5] <- NA
september.year.chir$Month[september.year.chir$Month == 6] <- NA
september.year.chir$Month[september.year.chir$Month == 7] <- NA
september.year.chir$Month[september.year.chir$Month == 8] <- NA
september.year.chir <- na.omit(september.year.chir)
seasonal.chir.mean.september<- aggregate(september.year.chir[, 5:6], list(september.year.chir$year.chir), mean)
seasonal.chir.mean.september$Month<-c("September") 

year.chirly.mean.Month<- rbind(seasonal.chir.mean.april,seasonal.chir.mean.may,seasonal.chir.mean.june,seasonal.chir.mean.july,seasonal.chir.mean.august,seasonal.chir.mean.september)
#Cbind aber davor Group1 umbennen zu den jeweiligen Monatsnamen
seasonal.chir.mean.year.chir<- aggregate(omit_chir.total[, 5:6], list(omit_chir.total$year.chir), mean)
seasonal.chir.mean.Month<- aggregate(omit_chir.total[, 5:6], list(omit_chir.total$Month), mean)

##model selection
library(ggplot2)
model.chir.total <- lm(data = omit_chir.total, Q~V4)
model.seasonal.chir.year.chir<-lm(data = omit_chir.total, Q ~ V4)
model.seasonal.chir.Month <-lm(data = seasonal.chir.mean.Month, Q ~ V4)
##model.seasonal.chir.Month is the good one
##NEU ab hier 25/02/2020
lm.chir.september <- lm(data = seasonal.chir.mean.september, Q~V4)
lm.chir.august <- lm(data = seasonal.chir.mean.august, Q~V4)
lm.chir.july <- lm(data = seasonal.chir.mean.july, Q~V4)
lm.chir.june <- lm(data = seasonal.chir.mean.june, Q~V4)
lm.chir.may <- lm(data = seasonal.chir.mean.may, Q~V4)
lm.chir.april <- lm(data = seasonal.chir.mean.april, Q~V4)
lm.chir.vp <- lm(data = omit_chir.total, Q~V4)

library(tidyr)
library(dplyr)
prediction.chir.september <- lm.chir.september %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(seasonal.chir.mean.september$Group.1))

prediction.chir.august <- lm.chir.august %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(seasonal.chir.mean.august$Group.1))

prediction.chir.july <- lm.chir.july %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(seasonal.chir.mean.july$Group.1))

prediction.chir.june <- lm.chir.june %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(seasonal.chir.mean.june$Group.1))

prediction.chir.vp <- lm.chir.vp %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(omit_chir.total$year.chir))
chir.mean.p.vp<-aggregate(prediction.chir.vp[, 1], list(prediction.chir.vp$x), mean)##das ist richtig


geom_line(data = prediction.chir.july, aes(x = x, y = fit, col = "july"))

p.chir<-ggplot(seasonal.chir.mean.year.chir, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = chir.mean.p.vp, aes(x = Group.1, y = x, col = "prediction.chir")) + xlab("year")
p.chir
summary(lm.chir.vp)
ggplot(seasonal.chir.mean.year, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = prediction.chir.september, aes(x = x, y = fit, col = "september"))+ geom_line(data = prediction.chir.august, aes(x = x, y = fit, col = "august")) + geom_line(data = prediction.chir.july, aes(x = x, y = fit, col = "july")) +geom_line(data = prediction.chir.june, aes(x = x, y = fit, col = "june"))


####
ggplot(seasonal.chir.mean.year.chir, aes(x = V4, y = Q))  + geom_point()

ggplot(seasonal.chir.mean.Month, aes(x = V4, y= Q, col= Group.1)) + geom_point() + geom_abline(slope = 0.665, intercept = 341.047)
ggplot(seasonal.chir.mean.year.chir, aes(x = Group.1, y = Q)) + geom_line() + geom_point()
ggplot(year.chirly.mean.Month, aes(x = Group.1, y = Q)) + geom_line(aes(col = Month)) + geom_point(aes(col = Month))
p.line.chir<-ggplot(omit_chir.total, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.9614, intercept = 336.2887 , col = 2)

p.chir<-ggplot(seasonal.chir.mean.year.chir, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = prediction.chir.vp, aes(x = x, y = fit, col = "prediction.chir")) + xlab("year") + ggtitle("Pritok Chirchik")
chir.year.p<-ggplot(seasonal.chir.mean.year.chir, aes(x = Group.1, y = Q)) + geom_line() + geom_point()
seasonal.chir.mean.year.chir$Area <- c("pritok_chirchik")

summary(model.seasonal.chir.Month)
summary(model.seasonal.chir.year.chir) 
summary(model.chir.total)
