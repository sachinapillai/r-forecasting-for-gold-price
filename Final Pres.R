#########################################################
#  Economic forecasting and analysis
#  Sachin A Pillai
#  Presentation
##########################################################


# load libraries
library(fpp)
library(vars)


# rename
GP = Gold.Price
USD = FRED.DTWEXM
IR=Interest.Rates
GP[,2]
USD[,2]

# tell R that data set is a time series
GPTS = ts(GP[,2], start=c(1968,4), end = c(2016,4), frequency=12)
View(GPTS)
USDTS = ts(USD[,2], start=c(1973,1), end = c(2016,11), frequency=12)
View(USDTS)
IRTS = ts(IR[,2], start=c(1954,7), end = c(2016,11), frequency=12)
View(IRTS)

#use shorter time span

GPWindowed <- window(GPTS,start=c(2000, 4),end=c(2016, 4))
USDWindowed <- window(USDTS,start=c(2000, 4),end=c(2016, 4))
IRWindowed <- window(IRTS,start=c(2000, 4),end=c(2016, 4))

plot(GPWindowed, main="Gold Price Montly",  xlab="YEAR", ylab="",col = 'blue', lwd=2)
plot(USDWindowed, main="USD Index Montly",  xlab="YEAR", ylab="",col = 'green', lwd=2)
plot(IRWindowed, main="Interest Rate Montly",  xlab="YEAR", ylab="",col = 'red', lwd=2)

DS <- data.frame(GPWindowed,USDWindowed)
View(DS)
TS <- data.frame(GPWindowed,USDWindowed,IRWindowed)
View(TS)


seasonplot(GPWindowed,ylab="Gold Price", xlab="Year",
           main="Seasonal plot: God Price",
           year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19)

monthplot(GPWindowed,ylab="$ million",xlab="Month",xaxt="n",
          main="Seasonal deviation plot: Gold Price")
axis(1,at=1:12,labels=month.abb,cex=0.8)

acf(GPWindowed)

# use a shorter time span. estimate model from 2000:1 to 2012:12
GPTS.Tra <- window(GPWindowed,start=c(2000,4),end=c(2012,12))
GPTest <- window(GPWindowed, start=2013)
h=length(GPTest)

GPMean <- meanf(GPTS.Tra, h=h)
GPNaive <- naive(GPTS.Tra, h=h)
GPSNaive <- snaive(GPTS.Tra, h=h)
GPDrift <- rwf(GPTS.Tra, h, drift=TRUE)

plot(GPMean, plot.conf=FALSE,
     main="Forecasts for Gold Price")
lines(GPNaive$mean,col=2)
lines(GPSNaive$mean,col=3)
lines(GPDrift$mean, col=6)
lines(GPWindowed)
legend("topleft",lty=1,col=c(4,2,3,6),
       legend=c("Mean method","Naive method","Seasonal naive method","Drift method"))

#Summary
a1=accuracy(GPMean, GPTest)
a2=accuracy(GPNaive, GPTest)
a3=accuracy(GPSNaive, GPTest)
a4=accuracy(GPDrift, GPTest)

a.table<-rbind(a1, a2, a3, a4)

row.names(a.table)<-c('Mean training','Mean test', 'Naive training', 'Naive test', 'S. Naive training', 'S. Naive test' , 'Drift Training','Drift Test')

# order the table according to MASE
a.table<-as.data.frame(a.table)
a.table<-a.table[order(a.table$MASE),]
a.table

tsdisplay(residuals(GPNaive))

resSF <- residuals(GPNaive)
plot(resSF, main="Residuals from naive method",
     ylab="", xlab="Years")
Acf(resSF, main="ACF of residuals")
hist(resSF, nclass="FD", main="Histogram of residuals")


#Time Series Decomposition

fitSTL<- stl(GPWindowed, s.window=5)
plot(fitSTL)
eeadj <- seasadj(fitSTL)

plot(naive(eeadj), xlab="New orders index",
     main="Naive forecasts of seasonally adjusted data")
fcast <- forecast(fitSTL, method="naive")
plot(fcast, ylab="New orders index")


#Exponential Smoothing

GPMa <- ses(GPTS.Tra, h=h)
GPHoltLinear <- holt(GPTS.Tra, initial = "simple", h=h)
GPHoltExpo <- holt(GPTS.Tra, initial = "simple",  exponential=TRUE, h=h)
GPHoltAdditive <- holt(GPTS.Tra,damped=TRUE, h=h)
GPHoltMult <- holt(GPTS.Tra,exponential=TRUE,damped=TRUE,h=h)
summary(GPHoltMult)
GPHoltsWintersMulti <- hw(GPTS.Tra, seasonal="multiplicative", h=h) 
GPHoltsWintersAdditive <- hw(GPTS.Tra, seasonal="additive", h=h) 

tsdisplay(residuals(GPMa))

plot(GPWindowed,ylim=c(0,2400),ylab="Gold Price")
lines(GPMa$mean, col="blue")
lines(GPHoltLinear$mean, col="red")
lines(GPHoltExpo$mean, col="green")
lines(GPHoltAdditive$mean, col="grey")
lines(GPHoltMult$mean, col="purple")
lines(GPHoltsWintersMulti$mean, col="yellow")
lines(GPHoltsWintersAdditive$mean, col="orange")
legend("topleft", lty=1, col=c("black","blue","red","green","grey","purple","yellow","orange"),
       c("Data","Exponential Smoothing","Holt's linear trend","Exponential trend","Additive Damped","Multiplicative Damped","Holt Winters Multiplicative","Holt Winters Seasonal"))

#forecast summary
a5=accuracy(GPMa,GPTest)
a6=accuracy(GPHoltLinear,GPTest)
a7=accuracy(GPHoltExpo,GPTest)
a8=accuracy(GPHoltAdditive,GPTest)
a9=accuracy(GPHoltMult,GPTest)
a10=accuracy(GPHoltsWintersMulti,GPTest)
a11=accuracy(GPHoltsWintersAdditive,GPTest)



#Combining forecast summary statistics into a table with row names
a.table<-rbind(a5, a6, a7,a8,a9,a10,a11)

row.names(a.table)<-c('SES training','SES test',  'Holt Linear training', 'Holt Linear test', 'Exponential Trend training', 'Exponential Trend test',
                      'Additive Damped Training','Additive Damped Test','Multiplicative Damped Training','Multiplicative Damped Test',
                      'Holt Winters Multiplicative Training','Holt Winters Multiplicative Test','Holt Winters Additive Training','Holt Winters Additive Test')

# order the table according to MASE
a.table<-as.data.frame(a.table)
a.table<-a.table[order(a.table$MASE),]
a.table

#ARIMA Method
invBoxCox <- function(test, lambda)
  if (lambda == 0) exp(test) else (lambda*test + 1)^(1/lambda)
lambda <- BoxCox.lambda(GPTS.Tra)
GPTS.Box<-BoxCox(GPTS.Tra,lambda)
eeadjB <- seasadj(stl(GPTS.Box, s.window="periodic"))
ns <- nsdiffs(eeadjB)
if(ns > 0) {
  xstar <- diff(eeadjB,lag=frequency(GPTS.Tra),differences=ns)
} else {
  xstar <- GPTS.Tra
}
nd <- ndiffs(xstar)
if(nd > 0) {
  GP.diff <- diff(xstar,differences=nd)
}
GP.arimaaB <- auto.arima(eeadjB,stepwise=FALSE, approximation=FALSE,d=1)
GPArimaFit <- forecast(GP.arimaaB, h=h)

accuracy(GPArimaFit,GPTest)


#multivariate
plot(jitter(USDWindowed) ~ jitter(GPWindowed),xlab="USD",
     ylab="Gold Prices",data=DS)
fitr <- tslm(USDWindowed ~ GPWindowed)
fitT <- tslm(USDWindowed ~ GPWindowed+ IRWindowed)
summary(fitT)




var.df = cbind(GPTS.Tra, USDWindowed, IRWindowed)
var.df = na.omit(var.df)
plot(var.df, main = "")

VARselect(var.df,type = "both")
VARselect(var.df, type = "const")

varBoth <- VAR(var.df, p = 2, type = "both")
summary(varBoth)
roots(varBoth)
plot(varBoth)


varConst <- VAR(var.df, p = 2, type = "const")
summary(varConst)
roots(varConst)
plot(varConst)


ser1 <- serial.test(varBoth, lags.pt = 16, type = "PT.asymptotic")
ser1$serial
ser2 <- serial.test(varConst, lags.pt = 16, type = "PT.asymptotic")
ser2$serial



# forecasts
fc1 = predict(varBoth, n.ahead=h)
fc1 
plot(fc1)

fc2 = predict(varConst, n.ahead=h)
fc2 
plot(fc2)

fitBoth = fc1$fcst$GPTS.Tra[,1]
fitConst = fc2$fcst$GPTS.Tra[,1]


t_fit = cbind(GPTest, fitBoth,fitConst)

par(font.axis = 2)
par(font.lab = 2)
plot(GPWindowed, main="VAR forecast for XOM", xlab="", ylab="", lwd=2 ,ylim=c(0,2400))
lines(t_fit[,2],  col="purple", lwd=2)
lines(t_fit[,3],  col="green", lwd=2)
legend("topleft",lty=1,col=c("black","purple","green"),
       legend=c("Actual","VAR method Both","VAR method Constant"),bty="n")

#Check accuracy of models
a1 = accuracy(fitBoth, GPTest)
a2 = accuracy(fitConst, GPTest)

#Combining forecast summary statistics into a table with row names
a.table<-rbind(a1, a2)

row.names(a.table)<-c('VAR(Const) test', 'VAR(Both) test')

# order the table according to MAPE
a.table<-as.data.frame(a.table)
a.table<-a.table[order(a.table$MAPE),]
a.table

