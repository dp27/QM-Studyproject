nauvalisay.elevation<-read.table("C:/Users/Darell/Desktop/Statistik MAP/nauvalisay_sidjak_elevation_bands_1000m.txt", sep = "", header = FALSE)
nauvalisay.snow<-read.table("C:/Users/Darell/Desktop/Statistik MAP/cloud_snow_fraction_nauvalisay_sidjak_report.txt", sep = ";", header = FALSE)
nauvalisay.drainage<-read.table("C:/Users/Darell/Desktop/Statistik MAP/sidjak_nauvalisay.txt", sep = ";", header = FALSE)

nauvalisay.snow <- nauvalisay.snow[-c(6886:7237), ]

for (i in 1:nrow(nauvalisay.snow)){
  if(nauvalisay.snow$V2[i] <= 31) {
    nauvalisay.snow[i, "Month"]<-1
  }
  else if(nauvalisay.snow$V2[i] ==32 | nauvalisay.snow$V2[i] <=59) {
    nauvalisay.snow[i, "Month"]<-2
  }
  else if(nauvalisay.snow$V2[i] ==60 | nauvalisay.snow$V2[i] <= 90) {
    nauvalisay.snow[i, "Month"]<-3
  }
  else if(nauvalisay.snow$V2[i] ==91 | nauvalisay.snow$V2[i] <= 120) {
    nauvalisay.snow[i, "Month"]<-4
  }
  else if(nauvalisay.snow$V2[i] ==121 | nauvalisay.snow$V2[i] <= 151) {
    nauvalisay.snow[i, "Month"]<-5
  }
  else if(nauvalisay.snow$V2[i] ==152 | nauvalisay.snow$V2[i] <= 181) {
    nauvalisay.snow[i, "Month"]<-6
  }
  else if(nauvalisay.snow$V2[i] ==182 | nauvalisay.snow$V2[i] <= 212) {
    nauvalisay.snow[i, "Month"]<-7
  }
  else if(nauvalisay.snow$V2[i] ==213 | nauvalisay.snow$V2[i] <= 243) {
    nauvalisay.snow[i, "Month"]<-8
  }
  else if(nauvalisay.snow$V2[i] ==244 | nauvalisay.snow$V2[i] <= 273) {
    nauvalisay.snow[i, "Month"]<-9
  }
  else if(nauvalisay.snow$V2[i] ==274 | nauvalisay.snow$V2[i] <= 304) {
    nauvalisay.snow[i, "Month"]<-10
  }
  else if(nauvalisay.snow$V2[i] ==305 | nauvalisay.snow$V2[i] <= 334) {
    nauvalisay.snow[i, "Month"]<-11
  }
  else{
    nauvalisay.snow[i, "Month"]<-12
  }
}
names(nauvalisay.snow)[1]<-paste("year.nau")
names(nauvalisay.drainage)[1]<-paste("year.nau")
names(nauvalisay.drainage)[2]<-paste("Month")
names(nauvalisay.drainage)[3]<-paste("Q")
nauvalisay.drainage$V4<-NULL
#data preperation for merge
##Obsolete

nau.total<-merge(nauvalisay.snow,nauvalisay.drainage,by=c("Month","year.nau"))
nau.total$V4[nau.total$V4 == -9]<- NA
nau.total$V5 <- NULL
omitted<-na.omit(nau.total)

nau.Monthly.mean<-aggregate(nau.total[, 5:7], list(nau.total$Month), mean)
nau.year.nauly.mean<-aggregate(nau.total[, 5:7], list(nau.total$year.nau), mean)

##data preperation for seasonal.nau mean
nau.total2<-nau.total
nau.total2$V5<-NULL
nau.total2$Month[nau.total2$Month == 1] <- NA
nau.total2$Month[nau.total2$Month == 2] <- NA
nau.total2$Month[nau.total2$Month == 3] <- NA
nau.total2$Month[nau.total2$Month == 10] <- NA
nau.total2$Month[nau.total2$Month == 11] <- NA
nau.total2$Month[nau.total2$Month == 12] <- NA
nau.total2$V4[nau.total2$V4 == -9]<- NA
omit_nau.total<- na.omit(nau.total2)
####Monatsdurchsnitte !HIER WEITERMACHEN|10/02/2020
april.year.nau<-omit_nau.total
april.year.nau$Month[april.year.nau$Month == 5] <- NA
april.year.nau$Month[april.year.nau$Month == 6] <- NA
april.year.nau$Month[april.year.nau$Month == 7] <- NA
april.year.nau$Month[april.year.nau$Month == 8] <- NA
april.year.nau$Month[april.year.nau$Month == 9] <- NA
april.year.nau <- na.omit(april.year.nau)
seasonal.nau.mean.april<- aggregate(april.year.nau[, 5:6], list(april.year.nau$year.nau), mean)
seasonal.nau.mean.april$Month<-c("April") 

may.year.nau<-omit_nau.total
may.year.nau$Month[may.year.nau$Month == 4] <- NA
may.year.nau$Month[may.year.nau$Month == 6] <- NA
may.year.nau$Month[may.year.nau$Month == 7] <- NA
may.year.nau$Month[may.year.nau$Month == 8] <- NA
may.year.nau$Month[may.year.nau$Month == 9] <- NA
may.year.nau <- na.omit(may.year.nau)
seasonal.nau.mean.may<- aggregate(may.year.nau[, 5:6], list(may.year.nau$year.nau), mean)
seasonal.nau.mean.may$Month<-c("May") 

june.year.nau<-omit_nau.total
june.year.nau$Month[june.year.nau$Month == 4] <- NA
june.year.nau$Month[june.year.nau$Month == 5] <- NA
june.year.nau$Month[june.year.nau$Month == 7] <- NA
june.year.nau$Month[june.year.nau$Month == 8] <- NA
june.year.nau$Month[june.year.nau$Month == 9] <- NA
june.year.nau <- na.omit(june.year.nau)
seasonal.nau.mean.june<- aggregate(june.year.nau[, 5:6], list(june.year.nau$year.nau), mean)
seasonal.nau.mean.june$Month<-c("June") 

july.year.nau<-omit_nau.total
july.year.nau$Month[july.year.nau$Month == 4] <- NA
july.year.nau$Month[july.year.nau$Month == 5] <- NA
july.year.nau$Month[july.year.nau$Month == 6] <- NA
july.year.nau$Month[july.year.nau$Month == 8] <- NA
july.year.nau$Month[july.year.nau$Month == 9] <- NA
july.year.nau <- na.omit(july.year.nau)
seasonal.nau.mean.july<- aggregate(july.year.nau[, 5:6], list(july.year.nau$year.nau), mean)
seasonal.nau.mean.july$Month<-c("July") 

august.year.nau<-omit_nau.total
august.year.nau$Month[august.year.nau$Month == 4] <- NA
august.year.nau$Month[august.year.nau$Month == 5] <- NA
august.year.nau$Month[august.year.nau$Month == 6] <- NA
august.year.nau$Month[august.year.nau$Month == 7] <- NA
august.year.nau$Month[august.year.nau$Month == 9] <- NA
august.year.nau <- na.omit(august.year.nau)
seasonal.nau.mean.august<- aggregate(august.year.nau[, 5:6], list(august.year.nau$year.nau), mean)
seasonal.nau.mean.august$Month<-c("August") 

september.year.nau<-omit_nau.total
september.year.nau$Month[september.year.nau$Month == 4] <- NA
september.year.nau$Month[september.year.nau$Month == 5] <- NA
september.year.nau$Month[september.year.nau$Month == 6] <- NA
september.year.nau$Month[september.year.nau$Month == 7] <- NA
september.year.nau$Month[september.year.nau$Month == 8] <- NA
september.year.nau <- na.omit(september.year.nau)
seasonal.nau.mean.september<- aggregate(september.year.nau[, 5:6], list(september.year.nau$year.nau), mean)
seasonal.nau.mean.september$Month<-c("September") 

year.nauly.mean.Month<- rbind(seasonal.nau.mean.april,seasonal.nau.mean.may,seasonal.nau.mean.june,seasonal.nau.mean.july,seasonal.nau.mean.august,seasonal.nau.mean.september)
#Cbind aber davor Group1 umbennen zu den jeweiligen Monatsnamen
seasonal.nau.mean.year<- aggregate(omit_nau.total[, 5:6], list(omit_nau.total$year.nau), mean)
seasonal.nau.mean.Month<- aggregate(omit_nau.total[, 5:6], list(omit_nau.total$Month), mean)

##model selection
library(ggplot2)
model.nau.total <- lm(data = omit_nau.total, Q~V4)
model.seasonal.nau.year.nau<-lm(data = omit_nau.total, Q ~ V4)
model.seasonal.nau.Month <-lm(data = seasonal.nau.mean.Month, Q ~ V4)
##model.seasonal.nau.Month is the good one
##Ab hier neu 25/02/2020
lm.nau.september <- lm(data = september.year.nau, Q~V4)
lm.nau.august <- lm(data = august.year.nau, Q~V4)
lm.nau.july <- lm(data = july.year.nau, Q~V4)
lm.nau.june <- lm(data = june.year.nau, Q~V4)
lm.nau.may <- lm(data = may.year.nau, Q~V4)
lm.nau.april <- lm(data = april.year.nau, Q~V4)
lm.nau.vp <- lm(data = omit_nau.total, Q~V4)

library(tidyr)
library(dplyr)
prediction.nau.september <- lm.nau.september %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(september.year.nau$year.nau))

prediction.nau.august <- lm.nau.august %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(august.year.nau$year.nau))

prediction.nau.july <- lm.nau.july %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(july.year.nau$year.nau))

prediction.nau.june <- lm.nau.june %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(june.year.nau$year.nau))

prediction.nau.may <- lm.nau.may %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(may.year.nau$year.nau))

prediction.nau.april<- lm.nau.april %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(april.year.nau$year.nau))

prediction.nau.vp <- lm.nau.vp %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>% mutate(x = sample(omit_nau.total$year.nau))
nau.mean.p.vp<-aggregate(prediction.nau.vp[, 1:3], list(prediction.nau.vp$x), mean)##das ist richtig

p.nau<-ggplot(seasonal.nau.mean.year, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = nau.mean.p.vp, aes(x = Group.1, y = x, col = "prediction"))+ xlab("year")
p.nau
summary(lm.nau.vp)
ggplot(seasonal.nau.mean.year, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = prediction.nau.september, aes(x = x, y = fit, col = "september"))+ geom_line(data = prediction.nau.august, aes(x = x, y = fit, col = "august")) + geom_line(data = prediction.nau.july, aes(x = x, y = fit, col = "july")) +geom_line(data = prediction.nau.june, aes(x = x, y = fit, col = "june"))

####
ggplot(seasonal.nau.mean.year, aes(x = V4, y = Q))  + geom_point()

ggplot(seasonal.nau.mean.Month, aes(x = V4, y= Q, col= Group.1)) + geom_point() + geom_abline(slope = 0.08981, intercept = 5.09004)
ggplot(year.nauly.mean.Month, aes(x = Group.1, y = Q)) + geom_line(aes(col = Month)) + geom_point(aes(col = Month))
p.line.nau<-ggplot(omit_nau.total, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.083307, intercept = 5.179838 , col = 2)

nau.year.p<-ggplot(seasonal.nau.mean.year, aes(x = Group.1, y = Q)) + geom_line() + geom_point()
p.nau<-ggplot(seasonal.nau.mean.year, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = prediction.nau.vp, aes(x = x, y = fit, col = "prediction"))+ xlab("year") + ggtitle("Sidjak Nauvalisay")
seasonal.nau.mean.year$Area <- c("sidjak_nauvalisay")
