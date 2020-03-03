library(gridExtra)

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
summary(lm.july)

###chirchiki
lm.chir.september <- lm(data = september.year.chir, Q~V4)
lm.chir.august <- lm(data = august.year.chir, Q~V4)
lm.chir.july <- lm(data = july.year.chir, Q~V4)
lm.chir.june <- lm(data = june.year.chir, Q~V4)
lm.chir.may <- lm(data = may.year.chir, Q~V4)
lm.chir.april <- lm(data = april.year.chir, Q~V4)
lm.chir.vp <- lm(data = omit_chir.total, Q~V4)

###navalisay
lm.nau.september <- lm(data = september.year.nau, Q~V4)
lm.nau.august <- lm(data = august.year.nau, Q~V4)
lm.nau.july <- lm(data = july.year.nau, Q~V4)
lm.nau.june <- lm(data = june.year.nau, Q~V4)
lm.nau.may <- lm(data = may.year.nau, Q~V4)
lm.nau.april <- lm(data = april.year.nau, Q~V4)
lm.nau.vp <- lm(data = omit_nau.total, Q~V4)

###ahangaran
lm.aha.september <- lm(data = september.year.aha, Q~V4)
lm.aha.august <- lm(data = august.year.aha, Q~V4)
lm.aha.july <- lm(data = july.year.aha, Q~V4)
lm.aha.june <- lm(data = june.year.aha, Q~V4)
lm.aha.may <- lm(data = may.year.aha, Q~V4)
lm.aha.april <- lm(data = april.year.aha, Q~V4)
lm.aha.vp <- lm(data = omit_aha.total, Q~V4)



?scale_y_sqrt()
