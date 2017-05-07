data <- c("03/29/16", "04/01/16")
as.Date(data,"%m/ %d/ %Y")
as.Date(data,"%d/ %m/ %Y")
data1 <- c("20160329","20160401")
as.Date(data1, "%Y %m %d")

data <- c("24/04/2016", "08/05/2016")
time <- c("15:45:30", "08:00:01")
czas <- paste(data, time)
strptime(czas, "%d/%m/%Y    %H:%M:%S")

ISOdate(2016, 04, 24)
wybrana.data <- ISOdatetime(2016, 4, 24, 16, 24, 59)

czas.operacji <- system.time(for(i in c(1:10^4)) sample(i,1:10^6)) #podaje czas wykonania operacji

#wylosuj 10 liczb z rozkladu normalnego
rnorm(10)
# losowanie 10 liczb ze srednia 2 i odchyleniem 3
rnorm(10,2,3)
#rysowanie dwoch wykresow na jednym wykresie z rozkladem normalnym
x <- seq(-4,4,by=0.01)
plot(x,dnorm(x), type="l", lwd=3, cex.axis=1.5, cex.lab=1.5)
par(usr=c(-4,4,-0.04, 1.04))
lines(x, pnorm(x), lty= 2, lwd=3, cex.axis=1.5, cex.lab=1.5)
axis(side=4, cex.axis=1.5, cex.lab = 1.5)
mtext(side=4, "pnorm()", line = 2.5, cex.axis=1.5, cex = 1.5)

#to samo ale dla poissona
x <- seq(0,40,by=1)
plot(x,dpois(x, 0.1), type="l", lwd=3, cex.axis=1.5, cex.lab=1.5)
par(usr=c(0,40,-0.04,1.5))
lines(x, ppois(x,0.1), lty= 2, lwd=3, cex.axis=1.5, cex.lab=1.5)
axis(side=4, cex.axis=1.5, cex.lab = 1.5)
mtext(side=4, "ppois()", line = 2.5, cex.axis=1.5, cex = 1.5)
