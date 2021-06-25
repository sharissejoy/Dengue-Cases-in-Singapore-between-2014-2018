#Sharisse Joy A. Gomez
#2-BS Applied Mathematics
#AMAT132 Forecasting Final Requirement

#import excel file to global environment
DENGUE <- read_excel("C:/Users/63917/Desktop/DENGUE.xlsx")

#check content of imported file
head (DENGUE)
tail(DENGUE)

#FEATURE CONSTRUCTION

library(fpp2)

#create time series
dengue.ts<-ts(DENGUE[,3], start=c(2014, 1), frequency = 53)
print(dengue.ts)

#create time plot
autoplot(dengue.ts) +
  ggtitle("Time Plot: Dengue Cases in Singapore between 2014-2018") +
  xlab("Date") +
  ylab("Dengue Cases")

#check for seasonality
ggseasonplot(dengue.ts, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Dengue Cases") +
  ggtitle("Seasonal plot: Dengue Cases in Singapore between 2014-2018")

#FINDINGS: Data is seasonal and cyclical.
#FINDINGS: Graphically seen that data is not stationary, data transformation must be done.

###############################################################
#FEATURE SELECTION AND TRANSFORMATION
###############################################################

library(urca)

#unit root test: to determine if differencing is needed 
dengue.ts%>% ur.kpss()%>% summary()

#FINDINGS: test statistic: 1.939 > 0.739 
#FINDINGS: hence data is not stationary.


#unit root test: for differenced data
dengue.ts %>% diff() %>% ur.kpss() %>% summary()

#FINDINGS: test statistic: 0.0344 < 0.739. 
#FINDINGS: hence data will be stationary after differencing.

#determine appropriate number of differencing
ndiffs(dengue.ts)

#FINDINGS:[1] 1. Thus we need to do first order differencing.

###################################
#DIFFERENCING
###################################

#Take the first difference of the data to remove the trend.
DY <- diff(dengue.ts)

#create time plot of transformed data
autoplot(DY) +
  ggtitle("Time Plot: Change in Dengue Cases in Singapore between 2014-2018") +
  xlab("Date") +
  ylab("Dengue Cases")

#seasonal plot appears trend
ggseasonplot(DY) +
  ggtitle("Seasonality Plot of First Difference Dengue Cases") +
  xlab("Date") +
  ylab("Dengue Cases")

#subseries plot
ggsubseriesplot(DY)+
  ggtitle("Subseries Plot of First Difference Dengue Cases") +
  xlab("Date") +
  ylab("Dengue Cases")

#FINDINGS: transformed data is now stationary.


###################################################################
#MODEL TRAINING
##################################################################

#Fit on Seasonal Naive Method
fit <- snaive(DY)
print(summary(fit))
checkresiduals(fit)

#FINDINGS: residual sd: 50.4621

#Fit on Exponential Smoothing
fit.ets <-ets(dengue.ts)
print(summary(fit.ets))
checkresiduals(fit.ets)

#FINDINGS: residual sd: 31.4814

#Fit on ARIMA
fit.arima <- auto.arima(dengue.ts, d=1, D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(fit.arima))
checkresiduals(fit.arima)

#Best model: ARIMA(1,1,0)(1,1,0)[53]
#FINDIGS: residual sd = 36.4692

###################################################################
#FORECASTING WITH ARIMA
##################################################################

#forecasting 5 months from 2018
forecast.arima <- forecast(fit.arima, h=20)
autoplot(forecast.arima)

#forecasting 1 year from 2018
forecast.arima2 <- forecast(fit.arima, h=53)
autoplot(forecast.arima2)
print(summary(forecast.arima1))

##########################################
#END
##########################################

