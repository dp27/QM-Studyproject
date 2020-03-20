library(gridExtra)
library(reshape)
library(ggplot2)
library(tidyr)
library(dplyr)

Pred.Obs<-grid.arrange(p.amu,p.chir,p.nau,p.aha, nrow = 2)
plot(Pred.Obs)

Obs.yearly.data<-rbind(seasonal.mean.year, seasonal.chir.mean.year.chir, seasonal.nau.mean.year, seasonal.aha.mean.year)

Obs.yearly <-ggplot(Obs.yearly.data,aes(x = Group.1, y = Q)) + geom_point(aes(col=name)) + geom_line(aes(col=name)) + xlab("Year")+scale_color_discrete(name = "River", breaks = c("aha","amu","chir","nau"),labels = c("Ahangaran", "Amudarya Kerki", "Chirchiki","Nauvalisay"))
Obs.yearly
Obs.yearly + scale_y_sqrt()

regression.lines<- grid.arrange(p.line.amu,p.line.chir,p.line.nau,p.line.aha, nrow = 2)
plot(regression.lines)

p.line.months.total<-grid.arrange(p.line.month.amu,p.line.months.chir,p.line.months.nau,p.line.months.aha, nrow=2)
plot(p.line.months.total)

p.obs.months.total<-grid.arrange(p.obs.amu.months,p.obs.chir.months,p.obs.nau.months,p.obs.aha.months, nrow=2)
plot(p.obs.months.total)

p.ribbons.months.total<-grid.arrange(p.ribbon.amu.months,p.ribbon.chir.months,p.ribbon.nau.months,p.ribbon.aha.months, nrow=2)
plot(p.ribbons.months.total)
##Models
###Amudarya Kerki

##error percentage  rate
sigma(lm.april)*100/mean(april.year$Q)

lm.september <- lm(data = september.year, Q~V4)
lm.august <- lm(data = august.year, Q~V4)
lm.july <- lm(data = july.year, Q~V4)
lm.june <- lm(data = june.year, Q~V4)
lm.may <- lm(data = may.year, Q~V4)
lm.april <- lm(data = april.year, Q~V4)
lm.vp <- lm(data = omit_total, Q~V4)
summary(lm.april)

p.line.amu<- ggplot(yearly.mean.month, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 38.856, intercept = 1.637, col = 2) + xlab("Snow Cover Area") + ggtitle("April-September")
p.line.amu

p.line.september.amu<- ggplot(september.year, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 16.88, intercept = 352.93, col = 2 ) + xlab("Snow cover area") + ggtitle("September")
p.line.august.amu<- ggplot(august.year, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 21.45, intercept = 1044.86, col = 2 ) + xlab("Snow cover area") + ggtitle("August")
p.line.july.amu<- ggplot(july.year, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 65.76, intercept = -507.81, col = 2 ) + xlab("Snow cover area") + ggtitle("July")
p.line.june.amu<- ggplot(june.year, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 51.544, intercept = -2.691, col = 2 ) + xlab("Snow cover area") + ggtitle("June")
p.line.may.amu<- ggplot(may.year, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 37.48, intercept = 42.50, col = 2 ) + xlab("Snow cover area") + ggtitle("May")
p.line.april.amu<- ggplot(april.year, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 40.02, intercept = -919.97, col = 2 ) + xlab("Snow cover area") + ggtitle("April")
p.line.april.amu
p.line.month.amu <- grid.arrange(p.line.april.amu, p.line.may.amu, p.line.june.amu, p.line.july.amu, p.line.august.amu, p.line.september.amu, nrow = 2, top = "Amudarya Kerki")
plot(p.line.month.amu)
##bei den anderen Flüssen auch "line" machen

seasonal.mean.month
seasonal.mean.september

p.amu.september <- ggplot(september.year, aes(x = year, y = Q, col = "blue")) + geom_line() + geom_line(data = mean.p.september, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") +ggtitle("September")+ scale_color_discrete(name="Lines", breaks=c("blue","prediction"),labels=c("Observation","Prediction"))
p.amu.september
p.amu.august <- ggplot(august.year, aes(x = year, y = Q, col = "blue")) + geom_line() + geom_line(data = mean.p.august, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("August")+ scale_color_discrete(name="Lines", breaks=c("blue","prediction"),labels=c("Observation","Prediction"))
p.amu.july <- ggplot(july.year, aes(x = year, y = Q, col = "blue")) + geom_line() + geom_line(data = mean.p.july, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("July")+ scale_color_discrete(name="Lines", breaks=c("blue","prediction"),labels=c("Observation","Prediction"))
p.amu.june <- ggplot(june.year, aes(x = year, y = Q, col = "blue")) + geom_line() + geom_line(data = mean.p.june, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("June")+ scale_color_discrete(name="Lines", breaks=c("blue","prediction"),labels=c("Observation","Prediction"))
p.amu.may <- ggplot(may.year, aes(x = year, y = Q, col = "blue")) + geom_line() + geom_line(data = mean.p.may, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("May")+ scale_color_discrete(name="Lines", breaks=c("blue","prediction"),labels=c("Observation","Prediction"))
p.amu.april <- ggplot(april.year, aes(x = year, y = Q, col = "blue")) + geom_line() + geom_line(data = mean.p.april, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("April")+ scale_color_discrete(name="Lines", breaks=c("blue","prediction"),labels=c("Observation","Prediction"))

p.obs.amu.months <- grid.arrange(p.amu.april,p.amu.may,p.amu.june,p.amu.july,p.amu.august,p.amu.september, nrow = 2, top = "Amudarya Kerki")
plot(p.obs.amu.months)

p.ribbon.september <- ggplot(september.year, aes(x = year, y = Q, col= "blue")) + geom_line()+  xlab("year") +ggtitle("September")+geom_line(data = pb.september, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line(data = pb.september, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Observation","Prediction band"))
p.ribbon.september
p.ribbon.august <- ggplot(august.year, aes(x = year, y = Q, col= "blue")) + geom_line() + xlab("year") +ggtitle("August")+geom_line(data = pb.august, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line(data = pb.august, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Observation","Prediction band"))
p.ribbon.july <- ggplot(july.year, aes(x = year, y = Q, col= "blue")) + geom_line() + xlab("year") +ggtitle("July")+geom_line(data = pb.july, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line(data = pb.july, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Observation","Prediction band"))
p.ribbon.june <- ggplot(june.year, aes(x = year, y = Q, col= "blue")) + geom_line() +  xlab("year") +ggtitle("june")+geom_line(data = pb.june, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line(data = pb.june, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Observation","Prediction band"))
p.ribbon.may <- ggplot(may.year, aes(x = year, y = Q, col= "blue")) + geom_line() + xlab("year") +ggtitle("May")+geom_line(data = pb.may, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line(data = pb.may, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Observation","Prediction band"))
p.ribbon.april <- ggplot(april.year, aes(x = year, y = Q, col= "blue")) + geom_line() + xlab("year") +ggtitle("April")+geom_line(data = pb.april, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line(data = pb.april, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Observation","Prediction band"))

p.ribbon.amu.months <- grid.arrange(p.ribbon.april,p.ribbon.may,p.ribbon.june,p.ribbon.july,p.ribbon.august,p.ribbon.september, nrow = 2, top = "Amudarya Kerki")
plot(p.ribbon.amu.months)

p.conf.september <- ggplot(data = prediction.september, aes(x = x, y =fit, col = "blue"), linetype = "dashed" )+ xlab("year")+ylab("Q") +geom_line(data = prediction.september, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line()+ggtitle("September") + geom_line(data = prediction.september, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Prediction","Confidence band"))
p.conf.august <- ggplot(data = prediction.august, aes(x = x, y =fit, col = "blue"), linetype = "dashed" )+ xlab("year")+ylab("Q") +geom_line(data = prediction.august, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line()+ggtitle("August") + geom_line(data = prediction.august, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Prediction","Confidence band"))
p.conf.july <- ggplot(data = prediction.july, aes(x = x, y =fit, col = "blue"), linetype = "dashed" )+ xlab("year")+ylab("Q") +geom_line(data = prediction.july, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line()+ggtitle("July") + geom_line(data = prediction.july, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Prediction","Confidence band"))
p.conf.june <- ggplot(data = prediction.june, aes(x = x, y =fit, col = "blue"), linetype = "dashed" )+ xlab("year")+ylab("Q") +geom_line(data = prediction.june, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line()+ggtitle("June") + geom_line(data = prediction.june, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Prediction","Confidence band"))
p.conf.may <- ggplot(data = prediction.may, aes(x = x, y =fit, col = "blue"), linetype = "dashed" )+ xlab("year")+ylab("Q") +geom_line(data = prediction.may, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line()+ggtitle("May") + geom_line(data = prediction.may, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Prediction","Confidence band"))
p.conf.april <- ggplot(data = prediction.april, aes(x = x, y =fit, col = "blue"), linetype = "dashed" )+ xlab("year")+geom_line(data = prediction.april, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line()+ggtitle("April") + geom_line(data = prediction.april, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Prediction","Confidence band"))
p.conf.september

p.conf.months <- grid.arrange(p.conf.april, p.conf.may, p.conf.june, p.conf.july, p.conf.august, p.conf.september, nrow = 2, top = "Amudarya Kerki")
plot(p.conf.months)
###chirchiki
lm.chir.september <- lm(data = september.year.chir, Q~V4)
lm.chir.august <- lm(data = august.year.chir, Q~V4)
lm.chir.july <- lm(data = july.year.chir, Q~V4)
lm.chir.june <- lm(data = june.year.chir, Q~V4)
lm.chir.may <- lm(data = may.year.chir, Q~V4)
lm.chir.april <- lm(data = april.year.chir, Q~V4)
lm.chir.vp <- lm(data = omit_chir.total, Q~V4)
summary(lm.chir.september)

p.line.chir<- ggplot(omit_chir.total, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 4.316, intercept = 144.321, col = 2)

p.line.september.chir<- ggplot(september.year.chir, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 1.1891, intercept = 85.2898, col = 2 ) + xlab("Snow cover area") + ggtitle("September")
p.line.august.chir<- ggplot(august.year.chir, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 2.036, intercept = 126.804, col = 2 ) + xlab("Snow cover area") + ggtitle("August")
p.line.july.chir<- ggplot(july.year.chir, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 5.846, intercept = 106.190, col = 2 ) + xlab("Snow cover area") + ggtitle("July")
p.line.june.chir<- ggplot(june.year.chir, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 8.317, intercept = 169.855, col = 2 ) + xlab("Snow cover area") + ggtitle("June")
p.line.may.chir<- ggplot(may.year.chir, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 4.587, intercept = 274.170, col = 2 ) + xlab("Snow cover area") + ggtitle("May")
p.line.april.chir<- ggplot(april.year.chir, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 3.921, intercept = 103.618, col = 2 ) + xlab("Snow cover area") + ggtitle("April")

p.line.months.chir <- grid.arrange(p.line.april.chir,p.line.may.chir,p.line.june.chir,p.line.july.chir,p.line.august.chir,p.line.september.chir, nrow = 2, top = "Chirchiki")
plot(p.line.months.chir)

p.chir.september <- ggplot(seasonal.chir.mean.september, aes(x = Group.1, y = Q, col = "blue")) + geom_line() + geom_line(data = mean.p.september.chir, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") +ggtitle("September") + scale_color_discrete(name="Lines", breaks=c("blue","prediction"),labels=c("Observation","Prediction"))
p.chir.august <- ggplot(seasonal.chir.mean.august, aes(x = Group.1, y = Q,col = "blue")) + geom_line() + geom_line(data = mean.p.august.chir, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("August") + scale_color_discrete(name="Lines", breaks=c("blue","prediction"),labels=c("Observation","Prediction"))
p.chir.july <- ggplot(seasonal.chir.mean.july, aes(x = Group.1, y = Q,col = "blue")) + geom_line() + geom_line(data = mean.p.july.chir, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("July") + scale_color_discrete(name="Lines", breaks=c("blue","prediction"),labels=c("Observation","Prediction"))
p.chir.june <- ggplot(seasonal.chir.mean.june, aes(x = Group.1, y = Q,col = "blue")) + geom_line() + geom_line(data = mean.p.june.chir, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("June") + scale_color_discrete(name="Lines", breaks=c("blue","prediction"),labels=c("Observation","Prediction"))
p.chir.may <- ggplot(seasonal.chir.mean.may, aes(x = Group.1, y = Q,col = "blue")) + geom_line() + geom_line(data = mean.p.may.chir, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("May") + scale_color_discrete(name="Lines", breaks=c("blue","prediction"),labels=c("Observation","Prediction"))
p.chir.april <- ggplot(seasonal.chir.mean.april, aes(x = Group.1, y = Q,col = "blue")) + geom_line() + geom_line(data = mean.p.april.chir, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("April") + scale_color_discrete(name="Lines", breaks=c("blue","prediction"),labels=c("Observation","Prediction"))

p.obs.chir.months <- grid.arrange(p.chir.april,p.chir.may,p.chir.june,p.chir.july,p.chir.august,p.chir.september, nrow = 2, top = "Chirchiki")
plot(p.obs.chir.months)

p.ribbon.september.chir <- ggplot(september.year.chir, aes(x = year.chir, y = Q, col= "blue")) + geom_line() +  xlab("year") +ggtitle("September")+geom_line(data = pb.chir.september, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line(data = pb.chir.september, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Observation","Prediction band"))
p.ribbon.september.chir
p.ribbon.august.chir <- ggplot(august.year.chir, aes(x = year.chir, y = Q, col= "blue")) + geom_line() +  xlab("year") +ggtitle("August")+geom_line(data = pb.chir.august, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line(data = pb.chir.august, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed") + scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Observation","Prediction band"))
p.ribbon.july.chir <- ggplot(july.year.chir, aes(x = year.chir, y = Q, col= "blue")) + geom_line() +  xlab("year") +ggtitle("July")+geom_line(data = pb.chir.july, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line(data = pb.chir.july, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Observation","Prediction band"))
p.ribbon.june.chir <- ggplot(june.year.chir, aes(x = year.chir, y = Q, col= "blue")) + geom_line() +  xlab("year") +ggtitle("June")+geom_line(data = pb.chir.june, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line(data = pb.chir.june, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Observation","Prediction band"))
p.ribbon.may.chir <- ggplot(may.year.chir, aes(x = year.chir, y = Q, col= "blue")) + geom_line() + xlab("year") +ggtitle("May")+geom_line(data = pb.chir.may, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line(data = pb.chir.may, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Observation","Prediction band"))
p.ribbon.april.chir <- ggplot(april.year.chir, aes(x = year.chir, y = Q, col= "blue")) + geom_line() + xlab("year") +ggtitle("April")+geom_line(data = pb.chir.april, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line(data = pb.chir.april, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Observation","Prediction band"))

p.ribbon.chir.months <- grid.arrange(p.ribbon.april.chir,p.ribbon.may.chir,p.ribbon.june.chir,p.ribbon.july.chir,p.ribbon.august.chir,p.ribbon.september.chir, nrow = 2, top = "Chirchiki")
plot(p.ribbon.chir.months)

p.conf.september.chir <- ggplot(data = prediction.chir.september, aes(x = x, y =fit, col = "blue"), linetype = "dashed" )+ xlab("year")+ ylab("Q") +geom_line(data = prediction.chir.september, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line()+ggtitle("September") + geom_line(data = prediction.chir.september, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Prediction","Confidence band"))
p.conf.august.chir <- ggplot(data = prediction.chir.august, aes(x = x, y =fit, col = "blue"), linetype = "dashed" )+ xlab("year")+ylab("Q")+ geom_line(data = prediction.chir.august, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line()+ggtitle("August") + geom_line(data = prediction.chir.august, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Prediction","Confidence band"))
p.conf.july.chir <- ggplot(data = prediction.chir.july, aes(x = x, y =fit, col = "blue"), linetype = "dashed" )+ xlab("year")+ylab("Q")+geom_line(data = prediction.chir.july, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line()+ggtitle("July") + geom_line(data = prediction.chir.july, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Prediction","Confidence band"))
p.conf.june.chir <- ggplot(data = prediction.chir.june, aes(x = x, y =fit, col = "blue"), linetype = "dashed" )+ xlab("year")+ylab("Q")+geom_line(data = prediction.chir.june, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line()+ggtitle("June") + geom_line(data = prediction.chir.june, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Prediction","Confidence band"))
p.conf.may.chir <- ggplot(data = prediction.chir.may, aes(x = x, y =fit, col = "blue"), linetype = "dashed" )+ xlab("year")+ylab("Q")+geom_line(data = prediction.chir.may, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line()+ggtitle("May") + geom_line(data = prediction.chir.may, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Prediction","Confidence band"))
p.conf.april.chir <- ggplot(data = prediction.chir.april, aes(x = x, y =fit, col = "blue"), linetype = "dashed" )+ xlab("year")+ylab("Q")+geom_line(data = prediction.chir.april, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line()+ggtitle("April") + geom_line(data = prediction.chir.april, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Prediction","Confidence band"))
p.conf.september.chir

p.conf.months.chir <- grid.arrange(p.conf.april.chir, p.conf.may.chir, p.conf.june.chir, p.conf.july.chir, p.conf.august.chir, p.conf.september.chir, nrow = 2, top = "Chirchik")
plot(p.conf.months.chir)

###nauvalisay
lm.nau.september <- lm(data = september.year.nau, Q~V4)
lm.nau.august <- lm(data = august.year.nau, Q~V4)
lm.nau.july <- lm(data = july.year.nau, Q~V4)
lm.nau.june <- lm(data = june.year.nau, Q~V4)
lm.nau.may <- lm(data = may.year.nau, Q~V4)
lm.nau.april <- lm(data = april.year.nau, Q~V4)
lm.nau.vp <- lm(data = omit_nau.total, Q~V4)
summary(lm.nau.august)

p.line.nau<- ggplot(omit_nau.total, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.07193, intercept = 0.5733, col = 2)

p.line.september.nau<- ggplot(september.year.nau, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.01814, intercept = 1.67610, col = 2 ) + xlab("Snow cover area") + ggtitle("September")
p.line.august.nau<- ggplot(august.year.nau, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.02680, intercept = 1.57925, col = 2 ) + xlab("Snow cover area") + ggtitle("August")
p.line.july.nau<- ggplot(july.year.nau, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.06632, intercept = -0.85901, col = 2 ) + xlab("Snow cover area") + ggtitle("July")
p.line.june.nau<- ggplot(june.year.nau, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.10886, intercept = -2.15913, col = 2 ) + xlab("Snow cover area") + ggtitle("June")
p.line.may.nau<- ggplot(may.year.nau, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.11929, intercept = -0.60345, col = 2 ) + xlab("Snow cover area") + ggtitle("May")
p.line.april.nau<- ggplot(april.year.nau, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.09219, intercept = 0.71024, col = 2 ) + xlab("Snow cover area") + ggtitle("April")

p.line.months.nau <- grid.arrange(p.line.april.nau,p.line.may.nau,p.line.june.nau,p.line.july.nau,p.line.august.nau,p.line.september.nau, nrow = 2, top = "Nauvalisay")
plot(p.line.months.nau)

p.nau.september <- ggplot(seasonal.nau.mean.september, aes(x = Group.1, y = Q, col = "blue")) + geom_line() + geom_line(data = mean.p.september.nau, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") +ggtitle("September") + scale_color_discrete(name="Lines", breaks=c("blue","prediction"),labels=c("Observation","Prediction"))
p.nau.august <- ggplot(seasonal.nau.mean.august, aes(x = Group.1, y = Q, col = "blue")) + geom_line() + geom_line(data = mean.p.august.nau, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("August")+ scale_color_discrete(name="Lines", breaks=c("blue","prediction"),labels=c("Observation","Prediction"))
p.nau.july <- ggplot(seasonal.nau.mean.july, aes(x = Group.1, y = Q, col = "blue")) + geom_line() + geom_line(data = mean.p.july.nau, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("July")+ scale_color_discrete(name="Lines", breaks=c("blue","prediction"),labels=c("Observation","Prediction"))
p.nau.june <- ggplot(seasonal.nau.mean.june, aes(x = Group.1, y = Q, col = "blue")) + geom_line() + geom_line(data = mean.p.june.nau, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("June")+ scale_color_discrete(name="Lines", breaks=c("blue","prediction"),labels=c("Observation","Prediction"))
p.nau.may <- ggplot(seasonal.nau.mean.may, aes(x = Group.1, y = Q, col = "blue")) + geom_line() + geom_line(data = mean.p.may.nau, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("May")+ scale_color_discrete(name="Lines", breaks=c("blue","prediction"),labels=c("Observation","Prediction"))
p.nau.april <- ggplot(seasonal.nau.mean.april, aes(x = Group.1, y = Q, col = "blue")) + geom_line() + geom_line(data = mean.p.april.nau, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("April")+ scale_color_discrete(name="Lines", breaks=c("blue","prediction"),labels=c("Observation","Prediction"))

p.obs.nau.months <- grid.arrange(p.nau.april,p.nau.may,p.nau.june,p.nau.july,p.nau.august,p.nau.september, nrow = 2, top = "Nauvalisay")
plot(p.obs.nau.months)

p.ribbon.september.nau <- ggplot(september.year.nau, aes(x = year.nau, y = Q, col= "blue")) + geom_line()+  xlab("year") +ggtitle("September")+geom_line(data = pb.nau.september, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line(data = pb.nau.september, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Observation","Prediction band"))
p.ribbon.september.nau
p.ribbon.august.nau <- ggplot(august.year.nau, aes(x = year.nau, y = Q, col= "blue")) + geom_line() +  xlab("year") +ggtitle("August")+geom_line(data = pb.nau.august, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line(data = pb.nau.august, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed") + scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Observation","Prediction band"))
p.ribbon.july.nau <- ggplot(july.year.nau, aes(x = year.nau, y = Q, col= "blue")) + geom_line() +  xlab("year") +ggtitle("July")+geom_line(data = pb.nau.july, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line(data = pb.nau.july, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Observation","Prediction band"))
p.ribbon.june.nau <- ggplot(june.year.nau, aes(x = year.nau, y = Q, col= "blue")) + geom_line() +  xlab("year") +ggtitle("June")+geom_line(data = pb.nau.june, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line(data = pb.nau.june, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Observation","Prediction band"))
p.ribbon.may.nau <- ggplot(may.year.nau, aes(x = year.nau, y = Q, col= "blue")) + geom_line() + xlab("year") +ggtitle("May")+geom_line(data = pb.nau.may, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line(data = pb.nau.may, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Observation","Prediction band"))
p.ribbon.april.nau <- ggplot(april.year.nau, aes(x = year.nau, y = Q, col= "blue")) + geom_line() + xlab("year") +ggtitle("April")+geom_line(data = pb.nau.april, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line(data = pb.nau.april, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Observation","Prediction band"))

p.ribbon.nau.months <- grid.arrange(p.ribbon.april.nau,p.ribbon.may.nau,p.ribbon.june.nau,p.ribbon.july.nau,p.ribbon.august.nau,p.ribbon.september.nau, nrow = 2, top = "Nauvalisay")
plot(p.ribbon.nau.months)

p.conf.september.nau <- ggplot(data = prediction.nau.september, aes(x = x, y =fit, col = "blue"), linetype = "dashed" )+ xlab("year")+ylab("Q") +geom_line(data = prediction.nau.september, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line()+ggtitle("September") + geom_line(data = prediction.nau.september, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Prediction","Confidence band"))
p.conf.august.nau <- ggplot(data = prediction.nau.august, aes(x = x, y =fit, col = "blue"), linetype = "dashed" )+ xlab("year")+ylab("Q") +geom_line(data = prediction.nau.august, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line()+ggtitle("August") + geom_line(data = prediction.nau.august, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Prediction","Confidence band"))
p.conf.july.nau <- ggplot(data = prediction.nau.july, aes(x = x, y =fit, col = "blue"), linetype = "dashed" )+ xlab("year")+ylab("Q") +geom_line(data = prediction.nau.july, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line()+ggtitle("July") + geom_line(data = prediction.nau.july, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Prediction","Confidence band"))
p.conf.june.nau <- ggplot(data = prediction.nau.june, aes(x = x, y =fit, col = "blue"), linetype = "dashed" )+ xlab("year")+ylab("Q") +geom_line(data = prediction.nau.june, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line()+ggtitle("June") + geom_line(data = prediction.nau.june, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Prediction","Confidence band"))
p.conf.may.nau <- ggplot(data = prediction.nau.may, aes(x = x, y =fit, col = "blue"), linetype = "dashed" )+ xlab("year")+ylab("Q") +geom_line(data = prediction.nau.may, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line()+ggtitle("May") + geom_line(data = prediction.nau.may, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Prediction","Confidence band"))
p.conf.april.nau <- ggplot(data = prediction.nau.april, aes(x = x, y =fit, col = "blue"), linetype = "dashed" )+ xlab("year")+ylab("Q") +geom_line(data = prediction.nau.april, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line()+ggtitle("April") + geom_line(data = prediction.nau.april, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Prediction","Confidence band"))
p.conf.september.nau

p.conf.months.nau <- grid.arrange(p.conf.april.nau, p.conf.may.nau, p.conf.june.nau, p.conf.july.nau, p.conf.august.nau, p.conf.september.nau, nrow = 2, top = "Nauvalisay")
plot(p.conf.months.nau)

###ahangaran
lm.aha.september <- lm(data = september.year.aha, Q~V4)
lm.aha.august <- lm(data = august.year.aha, Q~V4)
lm.aha.july <- lm(data = july.year.aha, Q~V4)
lm.aha.june <- lm(data = june.year.aha, Q~V4)
lm.aha.may <- lm(data = may.year.aha, Q~V4)
lm.aha.april <- lm(data = april.year.aha, Q~V4)
lm.aha.vp <- lm(data = omit_aha.total, Q~V4)
summary(lm.aha.april)

p.line.aha<- ggplot(omit_aha.total, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.1152, intercept = 23.27, col = 2)

p.line.september.aha<- ggplot(september.year.aha, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.04411, intercept = 0.62528, col = 2 ) + xlab("Snow cover area") + ggtitle("September")
p.line.august.aha<- ggplot(august.year.aha, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.05477, intercept = 1.96679, col = 2 ) + xlab("Snow cover area") + ggtitle("August")
p.line.july.aha<- ggplot(july.year.aha, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.04992, intercept = 11.25922, col = 2 ) + xlab("Snow cover area") + ggtitle("July")
p.line.june.aha<- ggplot(june.year.aha, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.3982, intercept = 4.5792, col = 2 ) + xlab("Snow cover area") + ggtitle("June")
p.line.may.aha<- ggplot(may.year.aha, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = -0.2075, intercept = 89.4704, col = 2 ) + xlab("Snow cover area") + ggtitle("May")
p.line.april.aha<- ggplot(april.year.aha, aes(x = V4, y= Q)) + geom_point() + geom_abline(slope = 0.3517, intercept = 31.7575, col = 2 ) + xlab("Snow cover area") + ggtitle("April")

p.line.months.aha <- grid.arrange(p.line.april.aha,p.line.may.aha,p.line.june.aha,p.line.july.aha,p.line.august.aha,p.line.september.aha, nrow = 2, top = "Ahangaran")
plot(p.line.months.aha)

p.aha.september <- ggplot(seasonal.aha.mean.september, aes(x = Group.1, y = Q, col = "blue")) + geom_line() + geom_line(data = mean.p.september.aha, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") +ggtitle("September") + scale_color_discrete(name="Lines", breaks=c("blue","prediction"),labels=c("Observation","Prediction"))
p.aha.august <- ggplot(seasonal.aha.mean.august, aes(x = Group.1, y = Q, col = "blue")) + geom_line() + geom_line(data = mean.p.august.aha, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("August")+ scale_color_discrete(name="Lines", breaks=c("blue","prediction"),labels=c("Observation","Prediction"))
p.aha.july <- ggplot(seasonal.aha.mean.july, aes(x = Group.1, y = Q, col = "blue")) + geom_line() + geom_line(data = mean.p.july.aha, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("July")+ scale_color_discrete(name="Lines", breaks=c("blue","prediction"),labels=c("Observation","Prediction"))
p.aha.june <- ggplot(seasonal.aha.mean.june, aes(x = Group.1, y = Q, col = "blue")) + geom_line() + geom_line(data = mean.p.june.aha, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("June")+ scale_color_discrete(name="Lines", breaks=c("blue","prediction"),labels=c("Observation","Prediction"))
p.aha.may <- ggplot(seasonal.aha.mean.may, aes(x = Group.1, y = Q, col = "blue")) + geom_line() + geom_line(data = mean.p.may.aha, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("May")+ scale_color_discrete(name="Lines", breaks=c("blue","prediction"),labels=c("Observation","Prediction"))
p.aha.april <- ggplot(seasonal.aha.mean.april, aes(x = Group.1, y = Q, col = "blue")) + geom_line() + geom_line(data = mean.p.april.aha, aes(x = Group.1, y = fit, col = "prediction")) + xlab("year") + ggtitle("April")+ scale_color_discrete(name="Lines", breaks=c("blue","prediction"),labels=c("Observation","Prediction"))

p.obs.aha.months <- grid.arrange(p.aha.april,p.aha.may,p.aha.june,p.aha.july,p.aha.august,p.aha.september, nrow = 2, top = "Ahangaran")
plot(p.obs.aha.months)

p.ribbon.september.aha <- ggplot(september.year.aha, aes(x = year.aha, y = Q, col= "blue")) + geom_line() +  xlab("year") +ggtitle("September")+geom_line(data = pb.aha.september, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line(data = pb.aha.september, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Observation","Prediction band"))
p.ribbon.september.aha
p.ribbon.august.aha <- ggplot(august.year.aha, aes(x = year.aha, y = Q, col= "blue")) + geom_line() + xlab("year") +ggtitle("August")+geom_line(data = pb.aha.august, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line(data = pb.aha.august, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed") + scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Observation","Prediction band"))
p.ribbon.july.aha <- ggplot(july.year.aha, aes(x = year.aha, y = Q, col= "blue")) + geom_line() +  xlab("year") +ggtitle("July")+geom_line(data = pb.aha.july, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line(data = pb.aha.july, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Observation","Prediction band"))
p.ribbon.june.aha <- ggplot(june.year.aha, aes(x = year.aha, y = Q, col= "blue")) + geom_line() +  xlab("year") +ggtitle("June")+geom_line(data = pb.aha.june, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line(data = pb.aha.june, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Observation","Prediction band"))
p.ribbon.may.aha <- ggplot(may.year.aha, aes(x = year.aha, y = Q, col= "blue")) + geom_line() +  xlab("year") +ggtitle("May")+geom_line(data = pb.aha.may, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line(data = pb.aha.may, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Observation","Prediction band"))
p.ribbon.april.aha <- ggplot(april.year.aha, aes(x = year.aha, y = Q, col= "blue")) + geom_line() +  xlab("year") +ggtitle("April")+geom_line(data = pb.aha.april, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line(data = pb.aha.april, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Observation","Prediction band"))

p.ribbon.aha.months <- grid.arrange(p.ribbon.april.aha,p.ribbon.may.aha,p.ribbon.june.aha,p.ribbon.july.aha,p.ribbon.august.aha,p.ribbon.september.aha, nrow = 2, top = "Ahangaran")
plot(p.ribbon.aha.months)

p.conf.september.aha <- ggplot(data = prediction.aha.september, aes(x = x, y =fit, col = "blue"), linetype = "dashed" )+ xlab("year")+ylab("Q") +geom_line(data = prediction.aha.september, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line()+ggtitle("September") + geom_line(data = prediction.aha.september, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Prediction","Confidence band"))
p.conf.august.aha <- ggplot(data = prediction.aha.august, aes(x = x, y =fit, col = "blue"), linetype = "dashed" )+ xlab("year")+ylab("Q") +geom_line(data = prediction.aha.august, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line()+ggtitle("August") + geom_line(data = prediction.aha.august, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Prediction","Confidence band"))
p.conf.july.aha <- ggplot(data = prediction.aha.july, aes(x = x, y =fit, col = "blue"), linetype = "dashed" )+ xlab("year")+ylab("Q") +geom_line(data = prediction.aha.july, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line()+ggtitle("July") + geom_line(data = prediction.aha.july, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Prediction","Confidence band"))
p.conf.june.aha <- ggplot(data = prediction.aha.june, aes(x = x, y =fit, col = "blue"), linetype = "dashed" )+ xlab("year")+ylab("Q") +geom_line(data = prediction.aha.june, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line()+ggtitle("June") + geom_line(data = prediction.aha.june, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Prediction","Confidence band"))
p.conf.may.aha <- ggplot(data = prediction.aha.may, aes(x = x, y =fit, col = "blue"), linetype = "dashed" )+ xlab("year")+ylab("Q") +geom_line(data = prediction.aha.may, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line()+ggtitle("May") + geom_line(data = prediction.aha.may, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Prediction","Confidence band"))
p.conf.april.aha <- ggplot(data = prediction.aha.april, aes(x = x, y =fit, col = "blue"), linetype = "dashed" )+ xlab("year")+ylab("Q") +geom_line(data = prediction.aha.april, aes(x = x, y = upr, col = "darkblue"), linetype = "dashed" ) + geom_line()+ggtitle("April") + geom_line(data = prediction.aha.april, aes(x = x, y = lwr, col = "darkblue"), linetype = "dashed")+ scale_color_discrete(name="Lines", breaks=c("blue","darkblue"),labels=c("Prediction","Confidence band"))
p.conf.september.aha

p.conf.months.aha <- grid.arrange(p.conf.april.aha, p.conf.may.aha, p.conf.june.aha, p.conf.july.aha, p.conf.august.aha, p.conf.september.aha, nrow = 2, top = "Ahangaran")
plot(p.conf.months.aha)

