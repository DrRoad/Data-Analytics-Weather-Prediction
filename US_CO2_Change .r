#1. LOAD LIBRARIES
library(tseries)
library(fBasics)

#2. IMPORT DATA
#  Load data with variable names into the data frame "myd"
myd=read.csv("CO2_per_capita_1970-2013_dataset_of_CO2_report_2014.csv") 
uscodata = myd[,c(1,198)]
trainusco = head(uscodata,-5)
trainusco
testusco = tail(uscodata,5)
testusco
year = trainusco[,1]
usco = trainusco[,2]

# create time series object
uscots = ts(usco, start= 1970)

#3 COMPUTE SUMMARY STATISTICS
basicStats(usco)
basicStats(uscots)

#4 CREATE HISTOGRAM
# creates 2 by 2 display for 4 plots
par(mfcol=c(2,2)) 
hist(uscots, xlab="United States CO2 level", prob=TRUE, main="Histogram")
# add approximating normal density curve
xfit<-seq(min(usco),max(usco),length=40)
yfit<-dnorm(xfit,mean=mean(usco),sd=sd(usco))
lines(xfit, yfit, col="blue", lwd=2) 

#5 CREATE NORMAL PROBABILITY PLOT
qqnorm(usco)
qqline(usco, col = 2) 

#6 CREATE TIME PLOT 
# use time series object lnatts to draw time plot indexed with time
plot(usco)
plot(uscots, type='l', xlab='Years', ylab='Co2 Change')

#7. NORMALITY TESTS
# Perform Shapiro-Wilk test
shapiro.test(usco)
 
#8 COMPUTE ACF and PACF AND PLOT CORRELOGRAM
#prints acf to console
acf(usco, plot=F, lag=15)
# creates 2 by 1 display for 2 plots
par(mfcol=c(2,1)) 
#plots acf (correlogram)
acf(usco, plot=T, lag=15)
# plots pacf values up to lag 15. 
pacf(usco, lag = 15)

#9 COMPUTE LJUNG-BOX TEST FOR WHITE NOISE (NO AUTOCORRELATION)
# to Lag 6
Box.test(usco,lag=6,type='Ljung')
# to Lag 12
Box.test(usco,lag=12,type='Ljung')
# to Lag 18
Box.test(usco,lag=18,type='Ljung')

#10 COMPUTE FIRST DIFFERENCE OF USA CO2
dusco = diff(usco)
# time series object
duscots=diff(uscots)

#11 COMPUTE ACF and PACF AND PLOT CORRELOGRAM OF FIRST DIFFERENCE
plot(duscots, type='l', xlab='time', ylab='Co2 Change')
#prints acf to console
acf(dusco, plot=F, lag=15)
# creates 2 by 1 display for 2 plots
par(mfcol=c(2,1)) 
#plots acf (correlogram)
acf(dusco, plot=T, lag=15)
# plots pacf values up to lag 15. 
pacf(dusco, lag = 15)

#12 FIT AN AR(1) MODEL 
#
# Fit an ARIMA model to a univariate time series.
# arima(x, order = c(p, 0, q))

model_ar= arima(dusco, order=c(1,0,0), method='ML', include.mean=T)
model_ar
# T-tests on coefficients
print('T-test table for model coefficients')
m1= model_ar
matrix(c(m1$coef,sqrt(diag(m1$var.coef)), 2 * pnorm(-abs(m1$coef / sqrt(diag(m1$var.coef))))), 
		nrow=length(m1$coef), ncol=3, byrow=F,
		dimnames = list(names(m1$coef), c("estimates", "test stats", "p-values")))

# RESIDUAL ANALYSIS
Box.test(model_ar$residuals,lag=6,type='Ljung')
Box.test(model_ar$residuals,lag=12,type='Ljung')
Box.test(model_ar$residuals,lag=18,type='Ljung')
acf(model_ar$residuals)

#13 FIT A MA(1) MODEL 

model_ma= arima(dusco, order=c(0,0,1), method='ML', include.mean=T)
model_ma
# T-tests on coefficients
print('T-test table for model coefficients')
model=model_ma
matrix(c(model$coef,sqrt(diag(model$var.coef)), 2 * pnorm(-abs(model$coef / sqrt(diag(model$var.coef))))), 
		nrow=length(model$coef), ncol=3, byrow=F,
		dimnames = list(names(model$coef), c("estimates", "test stats", "p-values")))

# RESIDUAL ANALYSIS
Box.test(model_ma$residuals,lag=6,type='Ljung')
Box.test(model_ma$residuals,lag=12,type='Ljung')
Box.test(model_ma$residuals,lag=18,type='Ljung')
acf(model_ma$residuals)


#14 FIT AN ARMA(1,1) MODEL 

model_arma= arima(dusco, order=c(1,0,1), method='ML', include.mean=T)
model_arma
# T-tests on coefficients
print('T-test table for model coefficients')
model=model_arma
matrix(c(model$coef,sqrt(diag(model$var.coef)), 2 * pnorm(-abs(model$coef / sqrt(diag(model$var.coef))))), 
		nrow=length(model$coef), ncol=3, byrow=F,
		dimnames = list(names(model$coef), c("estimates", "test stats", "p-values")))

# RESIDUAL ANALYSIS
Box.test(model_arma$residuals,lag=6,type='Ljung')
Box.test(model_arma$residuals,lag=12,type='Ljung')
Box.test(model_arma$residuals,lag=18,type='Ljung')
acf(model_arma$residuals)
		
#15 FIT AN ARIMA() MODEL using auto.arima
		
library(forecast)
model_autoar=auto.arima(usco, max.P=8, max.Q=8, ic="aic")
model_autoar

# RESIDUAL ANALYSIS
Box.test(residuals(model_autoar),lag=6,type='Ljung')
Box.test(residuals(model_autoar),lag=12,type='Ljung')
Box.test(residuals(model_autoar),lag=18,type='Ljung')

acf(residuals(model_autoar), na.action=na.remove)

#16 COMPUTE PREDICTIONS USING ALL the Model

# computes predictions 1-step to 4-step ahead 
# predict(object, n.ahead = 1, newxreg = NULL, se.fit = TRUE, ...)
#	object is the result of an arima fit.
#	n.ahead is the number of steps ahead for which prediction is required.
#	se.fit = T --> standard errors of forecasts are printed

# For AR model
pr_ar=predict(model_ar,n.ahead=5, se.fit=T)
pr_ar
predar = pr_ar$pred

# For MA model
pr_ma=predict(model_ma,n.ahead=5, se.fit=T)
pr_ma
predma = pr_ma$pred

# For ARMA model
pr_arma=predict(model_arma,n.ahead=5, se.fit=T)
pr_arma
predarma = pr_arma$pred

# For ARIMA model
pr_autoar=predict(model_autoar, n.ahead=5, se.fit=T)
pr_autoar
predautoar=pr_autoar$pred

#17 Storing test data in variable
testdata= testusco$United.States

#18 Moving predicted value into .csv file 
write.csv(predar, file = "Myusco1.csv", row.names=FALSE)
write.csv(predma, file = "Myusco2.csv", row.names=FALSE)
write.csv(predarma, file = "Myusco3.csv", row.names=FALSE)
write.csv(predautoar, file = "Myusco4.csv", row.names=FALSE)
write.csv(testdata, file = "Myusco5.csv", row.names=FALSE)

#19 Moving undifferenced predicted value into R
mydata=read.csv("US_CO2_Predicted_Data.csv")
mydata=mydata[-1,]
arpred= mydata[,3]
mapred= mydata[,5]
armapred= mydata[,7]
autoarpred= mydata[,8]
testdata=mydata[,1]

#20 Model Evaluation Technique 
library(Metrics)

mae(testdata, arpred)
mae(testdata, mapred)
mae(testdata, armapred)
mae(testdata, autoarpred)

rmse(testdata, arpred)
rmse(testdata, mapred)
rmse(testdata, armapred)
rmse(testdata, autoarpred)

mse(testdata, arpred)
mse(testdata, mapred)
mse(testdata, armapred)
mse(testdata, autoarpred)

#21 PLOT PREDICTIONS FOR 10 STEPS AHEAD FOR USA CO2
# FORECAST OF UNITED STATES 5 YEAR CO2 
library(forecast)
model_ma= Arima(duscots, order = c(0,0,1), include.drift = T)
future = forecast(model_ma, h = 10)
plot(future)
future
