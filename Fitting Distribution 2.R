library(forecast)
MAP=read_excel("MAPA.xlsx",sheet="MAPA",col_names=T)
mapa_returns <- MAP$Return
# ts.plot() will create a time series plot
ts.plot(mapa_returns, col="red",xlab="week", ylab="number of attacs",lwd=2)
text(280,120,"Maximum",col="blue")# will add text to our graph

ts.plot(mapa_returns, col="blue",xlab="week", ylab="Average time of attacs",lwd=2)
text(110,1700,"Maximum",col="red")

summary(mapa_returns)# descriptive statistics of our dataset

boxplot(mapa_returns,col="green",main="BoxPlot attacs",xlab="Count")

boxplot(mapa_returns,col="orange",main="BoxPlot av.length",xlab="time")
#
par(mfrow=c(1,2))
hist(mapa_returns,col="green",main="Histogram attacs",xlab="number of attacs",ylab="Freq")

library(fitdistrplus)
library(stats4)
library(MASS)
# for other necessary test or graphical tools
library(survival)
library(actuar)
library(distrMod)

plotdist(mapa_returns, histo = TRUE, demp = TRUE)

descdist(mapa_returns)

descdist(mapa_returns, boot = 1000)# bootstrapped values 1000

descdist(mapa_returns, boot = 10000)# bootstrapped values 10^4

# observe the concentration of the simulations
descdist(mapa_returns, boot = 1000,discrete=TRUE)

# Negative Binomial
nbinom.f<- fitdist(mapa_returns, "nbinom")
pois.f<- fitdist(mapa_returns, "pois")# Poisson
norm.f <- fitdist(mapa_returns, "norm") # Normal
exp.f <- fitdist(mapa_returns, "exp") # Exponential

summary(nbinom.f)
summary(pois.f)
summary(norm.f)
summary(exp.f)

par(mfrow = c(2, 2))
plot.legend <- c("Binomial neg", "Poisson","Normal","Exponential")
denscomp(list(nbinom.f,pois.f,norm.f,exp.f), legendtext = plot.legend)
qqcomp(list(nbinom.f,pois.f,norm.f,exp.f), legendtext = plot.legend)

cdfcomp(list(nbinom.f,pois.f,norm.f,exp.f), legendtext = plot.legend)
ppcomp(list(nbinom.f,pois.f,norm.f,exp.f), legendtext = plot.legend)

set.seed(512)
Ex=rexp(length(cyber.data$event.count),0.03934638)# estimated rate= 0.03934638
plot(cyber.data$event.count,Ex)# scatterplot observed vs fitted

No.r=rnorm(length(cyber.data$event.count),25.41530,19.82332) # estimated parameters
plot(cyber.data$event.count,No.r)#  scatterplot observed vs fitted

Po=rpois(length(cyber.data$event.count),25.4153)# estimated parameters
plot(cyber.data$event.count,Po)#  scatterplot observed vs fitted

neg.bin=rnbinom(length(cyber.data$event.count),size=1.658441, mu=25.415193)# estimated parameters
plot(cyber.data$event.count,neg.bin)#  scatterplot observed vs fitted

par(mfrow = c(2, 2))
plot.legend <- c("Normal","Exponencial")
denscomp(list(norm.f,exp.f), legendtext = plot.legend)
qqcomp(list(norm.f,exp.f), legendtext = plot.legend)
cdfcomp(list(norm.f,exp.f), legendtext = plot.legend)
ppcomp(list(norm.f,exp.f), legendtext = plot.legend)

nbinom.f <- fitdist(cyber.data$event.count, "nbinom") #
norm.f <- fitdist(cyber.data$event.count, "norm") #

#
par(mfrow = c(2, 2))
plot.legend <- c("Binomial neg", "Normal")
denscomp(list(nbinom.f,norm.f), legendtext = plot.legend)
qqcomp(list(nbinom.f,norm.f), legendtext = plot.legend)
cdfcomp(list(nbinom.f,norm.f), legendtext = plot.legend)
ppcomp(list(nbinom.f,norm.f), legendtext = plot.legend)

avg.report.length=ifelse(cyber.data$avg.report.length==0,0.01,cyber.data$avg.report.length)
expo.f <- fitdist(avg.report.length, "exp",method="mme")#
lnorm.f<- fitdist(avg.report.length, "lnorm",method="mme")#
norma.f<- fitdist(avg.report.length, "norm")#
weib <- fitdist(avg.report.length, "weibull", start = list(shape = 1, scale = 500))#
logis.f <- fitdist(avg.report.length, "llogis",start=NULL, fix.arg=NULL, discrete=FALSE, keepdata = TRUE)

descdist(avg.report.length)
descdist(avg.report.length, boot = 1000)# bootstrapped values 1000
descdist(avg.report.length, boot = 10000)# bootstrapped values 10^4
# observe the concentration of the simulations
descdist(avg.report.length, boot = 1000,discrete=TRUE)

summary(expo.f)
summary(lnorm.f)
summary(norma.f)
summary(weib)
summary(logis.f)

par(mfrow = c(2, 2))
plot.legend <- c("Exponential","Normal", "log-Normal","Weibull","Logis")
denscomp(list(expo.f,norma.f,lnorm.f,weib,logis.f), legendtext = plot.legend)
qqcomp(list(expo.f,norma.f,lnorm.f,weib,logis.f), legendtext = plot.legend)
cdfcomp(list(expo.f,norma.f,lnorm.f,weib,logis.f), legendtext = plot.legend)
ppcomp(list(expo.f,norma.f,lnorm.f,weib,logis.f), legendtext = plot.legend)

#
gofstat(list(expo.f,norma.f,lnorm.f,weib,logis.f),fitnames = c("Exponential","Normal", "log-Normal","Weibull","Logis"))

