library(gridExtra)

Pred.Obs<-grid.arrange(p.amu,p.chir,p.nau,p.aha, nrow = 2)
plot(Pred.Obs)

Obs.yearly.data<-rbind(seasonal.mean.year, seasonal.chir.mean.year.chir, seasonal.nau.mean.year, seasonal.aha.mean.year)

Obs.yearly <-ggplot(Obs.yearly.data,aes(x = Group.1, y = Q, log = "y")) + geom_point(aes(col=Area)) + geom_line(aes(col=Area))
Obs.yearly
Obs.yearly + scale_y_sqrt()

regression.lines<- grid.arrange(p.line.amu,p.line.chir,p.line.nau,p.line.aha, nrow = 2)
regression.lines
?scale_y_sqrt()
