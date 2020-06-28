
##################Load data and libraries ##################
setwd("/Users/Vicky/Desktop/PGP-BABI-vikas/Time series")
install.packages("forecast")

library(forecast)

data<- forecast::gas
View(data)


###########EDA ############
data.ts=ts(data, start=c(1956,1),frequency=12)
autoplot(data.ts,main="Australian monthly gas production")
ts.plot(data.ts)
hist(data.ts)
boxplot(data.ts~cycle(data.ts))
#taking data from 1970 onwards
data.ts1=window(data.ts,start=c(1970,1),frequency=12)
autoplot(data.ts1)
summary(data.ts)

ggseasonplot(data.ts1)
## Plot 1: Seasonal plot Year-wise (using ggseasonalplot()) 
ggseasonplot(data.ts1, year.labels=TRUE, year.labels.left=TRUE
) + ylab("degree") +
  ggtitle("Seasonal plot: GAS Monthly Production")

## Plot 2: Polar Seasonal plot Year-wise (using ggseasonplot()) 
ggseasonplot(data.ts1, polar=TRUE) +
ylab("degree") +
  ggtitle("Polar seasonal plot: GAS Monthly Production")

## Plot 3: Seasonal plot Month-wise 
monthplot(data.ts1)


# Split data
data.ts
data.ts.train=window(data.ts, start=c(1970,1),end=c(1993,12))
data.ts.test=window(data.ts,start=c(1994,1))
data.ts.test
ts.plot(data.ts.train,data.ts.test,col=c("blue","red"))

autoplot(data.ts.train, series="Train") + autolayer(data.ts.test, series="Test") + ggtitle("GAS Monthly Production")+ xlab("Year") + ylab("production") + guides(colour=guide_legend(title="Forecast"))
 
 
#Augmented dickey fuller test
library(tseries)
adf.test(data.ts1) #indicates stationary time series

#decompoing
data.ts.train.dec=stl(data.ts.train,s.window="p")
plot(data.ts.train.dec)
data.ts.train.dec

#########################forecasting methods #####################
fore=forecast(data.ts.train,h=20)
autoplot(fore)  



#########Random Walk with drift###############
fore.rwd=forecast(data.ts.train.dec,method="rwdrift",h=20)
fore.rwd$mean
autoplot(fore.rwd)
#Accuracy measures using RWD method

ts.plot(data.ts.test,fore.rwd$mean, col=c("blue", "red"), main="Gas production: Actual vs Forecast",xlab="year",ylab="Gas production")
fore.rwd$model
accuracy(fore.rwd,data.ts.test)



##############Simple exponential smoothing###########
library(fpp2)
fore.ses=ses(data.ts.train,h=20)
autoplot(fore.ses)
fore.ses
ts.plot(data.ts.test,fore.ses$mean, col=c("blue", "red"), main="Gas production: Actual vs Forecast",xlab="year",ylab="Gas production")
accuracy(fore.ses,data.ts.test)


##################Holts smoothing##################
fore.holt=holt(data.ts.train,h=20)
autoplot(fore.holt)

###################holt winters smoothing#################
fore.hw=hw(data.ts.train,h=20)
autoplot(fore.hw)
fore.hw$model
ts.plot(data.ts.test,fore.hw$mean, col=c("blue", "red"), main="Gas production: Actual vs Forecast- Holt winter's",xlab="year",ylab="Gas production")
accuracy(fore.hw,data.ts.test)
class(fore.hw)
class(data.ts.test)
#######################ARIMA########################

#determining value of d
adf.test(data.ts.train) #stationary series =. no need to difference

#determining value of p and q
train.trans=diff(log(data.ts.train))
acf(train.trans,lag=50, main="autocorrelation") #determine q value
# q = 2

pacf(train.trans,lag=50,main="Partial Autocorrealtion") #determine p value
#p = 2

fore.arima=arima(log(data.ts.train),c(2,1,2))

hist(2.718^(fore.arima$residuals))
arima.fit=fitted(fore.arima)
arima.fit
pred=predict(fore.arima,n.ahead=20)
2.718^pred$pred
data.ts.test
ts.plot(data.ts.test,2.718^pred$pred,col=c("red","blue"))
ts.plot(data.ts.train,2.718^fitted(fore.arima),col=c("red","blue"))
accuracy(data.ts.train,2.718^fitted(fore.arima))
?accuracy
class(fore.arima)
class(pred$pred)
accuracy(fore.arima)

#boxtest
Box.test(fore.arima$residuals,lag=30,type="Ljung-Box")
# p is low => reject null hypothesis => residuals are not independant
#ARIMA model found not adequate to explain gas production.


################auto arima###################

fore.auto=auto.arima(data.ts.train,seasonal=TRUE,trace=T)
 summary(fore.auto)                     
Box.test(fore.auto$residuals,lag=30,type="Ljung-Box")

# p is higher than 0.05 => fail to reject null hypothesis => residuals are independant
#ARIMA model found adequate to explain gas production.

     