library(gridExtra)
library(reshape)
library(ggplot2)
library(tidyr)
library(dplyr)

Pred.Obs<-grid.arrange(p.amu,p.chir,p.nau,p.aha, nrow = 2)
plot(Pred.Obs)

Obs.yearly.data<-rbind(seasonal.mean.year, seasonal.chir.mean.year.chir, seasonal.nau.mean.year, seasonal.aha.mean.year)

Obs.yearly <-ggplot(Obs.yearly.data,aes(x = Group.1, y = Q, log = "y")) + geom_point(aes(col=Area)) + geom_line(aes(col=Area))
Obs.yearly
Obs.yearly + scale_y_sqrt()

regression.lines<- grid.arrange(p.line.amu,p.line.chir,p.line.nau,p.line.aha, nrow = 2)
plot(regression.lines)

##Models
###Amudarya Kerki
lm.september <- lm(data = september.year, Q~V4)
lm.august <- lm(data = august.year, Q~V4)
lm.july <- lm(data = july.year, Q~V4)
lm.june <- lm(data = june.year, Q~V4)
lm.may <- lm(data = may.year, Q~V4)
lm.april <- lm(data = april.year, Q~V4)
lm.vp <- lm(data = omit_total, Q~V4)
summary(lm.april)

p.line.amu<- ggplot(omit_total, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = -23.548, intercept = 2209.961, col = 2)

p.line.september.amu<- ggplot(september.year, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 3.045, intercept = 1169.320, col = 2 ) + xlab("Snow accumulation") + ggtitle("September")
p.line.august.amu<- ggplot(august.year, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 145.6, intercept = 1577.3, col = 2 ) + xlab("Snow accumulation") + ggtitle("August")
p.line.july.amu<- ggplot(july.year, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 181.53, intercept = 1805.83, col = 2 ) + xlab("Snow accumulation") + ggtitle("July")
p.line.june.amu<- ggplot(june.year, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 73.706, intercept = 1728.077, col = 2 ) + xlab("Snow accumulation") + ggtitle("June")
p.line.may.amu<- ggplot(may.year, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 36.083, intercept = 1143.022, col = 2 ) + xlab("Snow accumulation") + ggtitle("May")
p.line.april.amu<- ggplot(april.year, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 18.654, intercept = 400.395, col = 2 ) + xlab("Snow accumulation") + ggtitle("April")

p.line.months.amu <- grid.arrange(p.line.april.amu,p.line.may.amu,p.line.june.amu,p.line.july.amu,p.line.august.amu,p.line.september.amu, nrow = 2, top = "Amudarya Kerki")
plot(p.line.months)
##bei den anderen Flüssen auch "line" machen

seasonal.mean.month
seasonal.mean.september
p.amu.september <- ggplot(seasonal.mean.september, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.september, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") +ggtitle("September")
p.amu.august <- ggplot(seasonal.mean.august, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.august, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("August")
p.amu.july <- ggplot(seasonal.mean.july, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.july, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("July")
p.amu.june <- ggplot(seasonal.mean.june, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.june, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("June")
p.amu.may <- ggplot(seasonal.mean.may, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.may, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("May")
p.amu.april <- ggplot(seasonal.mean.april, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.april, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("April")

p.obs.amu.months <- grid.arrange(p.amu.april,p.amu.may,p.amu.june,p.amu.july,p.amu.august,p.amu.september, nrow = 2, top = "Amudarya Kerki")
plot(p.obs.amu.months)

###chirchiki
lm.chir.september <- lm(data = september.year.chir, Q~V4)
lm.chir.august <- lm(data = august.year.chir, Q~V4)
lm.chir.july <- lm(data = july.year.chir, Q~V4)
lm.chir.june <- lm(data = june.year.chir, Q~V4)
lm.chir.may <- lm(data = may.year.chir, Q~V4)
lm.chir.april <- lm(data = april.year.chir, Q~V4)
lm.chir.vp <- lm(data = omit_chir.total, Q~V4)
summary(lm.chir.april)

p.line.chir<- ggplot(omit_chir.total, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.9614, intercept = 336.2887, col = 2)

p.line.september.chir<- ggplot(september.year.chir, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.1066, intercept = 140.5, col = 2 ) + xlab("Snow accumulation") + ggtitle("September")
p.line.august.chir<- ggplot(august.year.chir, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 5.746, intercept = 223.613, col = 2 ) + xlab("Snow accumulation") + ggtitle("August")
p.line.july.chir<- ggplot(july.year.chir, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 22.322, intercept = 339.771, col = 2 ) + xlab("Snow accumulation") + ggtitle("July")
p.line.june.chir<- ggplot(june.year.chir, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 9.228, intercept = 470.960, col = 2 ) + xlab("Snow accumulation") + ggtitle("June")
p.line.may.chir<- ggplot(may.year.chir, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 2.5865, intercept = 418.6527, col = 2 ) + xlab("Snow accumulation") + ggtitle("May")
p.line.april.chir<- ggplot(april.year.chir, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 1.0474, intercept = 225.6249, col = 2 ) + xlab("Snow accumulation") + ggtitle("April")

p.line.months.chir <- grid.arrange(p.line.april.chir,p.line.may.chir,p.line.june.chir,p.line.july.chir,p.line.august.chir,p.line.september.chir, nrow = 2, top = "Chirchiki")
plot(p.line.months.chir)

p.chir.september <- ggplot(seasonal.chir.mean.september, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.september.chir, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") +ggtitle("September")
p.chir.august <- ggplot(seasonal.chir.mean.august, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.august.chir, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("August")
p.chir.july <- ggplot(seasonal.chir.mean.july, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.july.chir, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("July")
p.chir.june <- ggplot(seasonal.chir.mean.june, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.june.chir, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("June")
p.chir.may <- ggplot(seasonal.chir.mean.may, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.may.chir, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("May")
p.chir.april <- ggplot(seasonal.chir.mean.april, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.april.chir, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("April")

p.obs.chir.months <- grid.arrange(p.chir.april,p.chir.may,p.chir.june,p.chir.july,p.chir.august,p.chir.september, nrow = 2, top = "Chirchiki")
plot(p.obs.chir.months)


###nauvalisay
lm.nau.september <- lm(data = september.year.nau, Q~V4)
lm.nau.august <- lm(data = august.year.nau, Q~V4)
lm.nau.july <- lm(data = july.year.nau, Q~V4)
lm.nau.june <- lm(data = june.year.nau, Q~V4)
lm.nau.may <- lm(data = may.year.nau, Q~V4)
lm.nau.april <- lm(data = april.year.nau, Q~V4)
lm.nau.vp <- lm(data = omit_nau.total, Q~V4)
summary(lm.nau.april)

p.line.nau<- ggplot(omit_nau.total, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.083307, intercept = 5.179838, col = 2)

p.line.september.nau<- ggplot(september.year.nau, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.009759, intercept = 3.224016, col = 2 ) + xlab("Snow accumulation") + ggtitle("September")
p.line.august.nau<- ggplot(august.year.nau, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = -0.02224, intercept = 0.02008, col = 2 ) + xlab("Snow accumulation") + ggtitle("August")
p.line.july.nau<- ggplot(july.year.nau, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.13563, intercept = 4.76973, col = 2 ) + xlab("Snow accumulation") + ggtitle("July")
p.line.june.nau<- ggplot(june.year.nau, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.23168, intercept = 6.20987, col = 2 ) + xlab("Snow accumulation") + ggtitle("June")
p.line.may.nau<- ggplot(may.year.nau, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.088295, intercept = 8.010183, col = 2 ) + xlab("Snow accumulation") + ggtitle("May")
p.line.april.nau<- ggplot(april.year.nau, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.035666, intercept = 6.779410, col = 2 ) + xlab("Snow accumulation") + ggtitle("April")

p.line.months.nau <- grid.arrange(p.line.april.nau,p.line.may.nau,p.line.june.nau,p.line.july.nau,p.line.august.nau,p.line.september.nau, nrow = 2, top = "Nauvalisay")
plot(p.line.months.nau)

p.nau.september <- ggplot(seasonal.nau.mean.september, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.september.nau, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") +ggtitle("September")
p.nau.august <- ggplot(seasonal.nau.mean.august, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.august.nau, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("August")
p.nau.july <- ggplot(seasonal.nau.mean.july, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.july.nau, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("July")
p.nau.june <- ggplot(seasonal.nau.mean.june, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.june.nau, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("June")
p.nau.may <- ggplot(seasonal.nau.mean.may, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.may.nau, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("May")
p.nau.april <- ggplot(seasonal.nau.mean.april, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.april.nau, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("April")

p.obs.nau.months <- grid.arrange(p.nau.april,p.nau.may,p.nau.june,p.nau.july,p.nau.august,p.nau.september, nrow = 2, top = "Nauvalisay")
plot(p.obs.nau.months)

###ahangaran
lm.aha.september <- lm(data = september.year.aha, Q~V4)
lm.aha.august <- lm(data = august.year.aha, Q~V4)
lm.aha.july <- lm(data = july.year.aha, Q~V4)
lm.aha.june <- lm(data = june.year.aha, Q~V4)
lm.aha.may <- lm(data = may.year.aha, Q~V4)
lm.aha.april <- lm(data = april.year.aha, Q~V4)
lm.aha.vp <- lm(data = omit_aha.total, Q~V4)
summary(lm.aha.april)

p.line.aha<- ggplot(omit_aha.total, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.84913, intercept = 20.98355, col = 2)

p.line.september.aha<- ggplot(september.year.aha, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.01944, intercept = 4.39384, col = 2 ) + xlab("Snow accumulation") + ggtitle("September")
p.line.august.aha<- ggplot(august.year.aha, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = -0.1196, intercept = 6.7021, col = 2 ) + xlab("Snow accumulation") + ggtitle("August")
p.line.july.aha<- ggplot(july.year.aha, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 5.7621, intercept = 13.1350, col = 2 ) + xlab("Snow accumulation") + ggtitle("July")
p.line.june.aha<- ggplot(june.year.aha, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 2.1731, intercept = 30.0954, col = 2 ) + xlab("Snow accumulation") + ggtitle("June")
p.line.may.aha<- ggplot(may.year.aha, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.47501, intercept = 61.18836, col = 2 ) + xlab("Snow accumulation") + ggtitle("May")
p.line.april.aha<- ggplot(april.year.aha, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.12997, intercept = 54.4792, col = 2 ) + xlab("Snow accumulation") + ggtitle("April")

p.line.months.aha <- grid.arrange(p.line.april.aha,p.line.may.aha,p.line.june.aha,p.line.july.aha,p.line.august.aha,p.line.september.aha, nrow = 2, top = "Ahangaran")
plot(p.line.months.aha)

p.aha.september <- ggplot(seasonal.aha.mean.september, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.september.aha, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") +ggtitle("September")
p.aha.august <- ggplot(seasonal.aha.mean.august, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.august.aha, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("August")
p.aha.july <- ggplot(seasonal.aha.mean.july, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.july.aha, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("July")
p.aha.june <- ggplot(seasonal.aha.mean.june, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.june.aha, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("June")
p.aha.may <- ggplot(seasonal.aha.mean.may, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.may.aha, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("May")
p.aha.april <- ggplot(seasonal.aha.mean.april, aes(x = Group.1, y = Q)) + geom_line() + geom_line(data = mean.p.april.aha, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("April")

p.obs.aha.months <- grid.arrange(p.aha.april,p.aha.may,p.aha.june,p.aha.july,p.aha.august,p.aha.september, nrow = 2, top = "Ahangaran")
plot(p.obs.aha.months)


?scale_y_sqrt()
