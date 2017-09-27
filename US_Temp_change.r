#1. LOAD LIBRARIES
library(dplyr)
library(tidyr)

#2. IMPORT DATA
#  Load data with variable names into the data frame "my_data"
my_data= read.csv("GlobalLandTemperaturesByState.csv")
my_data = na.omit(my_data)

#3. FILTER DATA CONTAINING COUNTRY AS UNITED STATES 
dataus = my_data %>% filter(Country=="United States") %>% separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE) 
dataus2 <- dataus %>%filter(Year>=1900)%>% select(Year,AverageTemperature,State) %>% group_by(Year,State) %>% dplyr::summarise(value=mean(AverageTemperature))
dataustemp= dataus2[,c(1,3)]
colnames(dataustemp)[2]<- "Temperature"
# TRAINING DATA
dataustrain = dataustemp %>%filter(Year<=2003)
# TRAINING DATA MOVED INTO VARIABLE
temp = dataustrain$Temperature
year= dataustrain$Year
# TESTING DATA 
dataustest = dataustemp %>%filter(Year>2003)

#4 create time series object
tempts = ts(temp, start=c(1900,1), freq=12)

library(fBasics)
#5 COMPUTE SUMMARY STATISTICS
basicStats(temp)
basicStats(tempts)

#6 CREATE HISTOGRAM
# creates 2 by 2 display for 4 plots
par(mfcol=c(2,2)) 
hist(tempts, xlab="Temperature change", prob=TRUE, main="Histogram")
# add approximating normal density curve
xfit<-seq(min(temp),max(temp),length=40)
yfit<-dnorm(xfit,mean=mean(temp),sd=sd(temp))
lines(xfit, yfit, col="blue", lwd=2) 

#7 CREATE NORMAL PROBABILITY PLOT
qqnorm(temp)
qqline(temp, col = 2) 

#8 CREATE TIME PLOT 
# use time series object lnatts to draw time plot indexed with time
plot(temp)
plot(tempts, type='l', xlab='time', ylab='Temperature Change')

#9. NORMALITY TESTS
# Perform Jarque-Bera normality test.
normalTest(temp,method=c('jb'))  

#10 COMPUTE ACF and PACF AND PLOT CORRELOGRAM
#prints acf to console
acf(temp, plot=F, lag=15)
# creates 2 by 1 display for 2 plots
par(mfcol=c(2,1)) 
#plots acf (correlogram)
acf(temp, plot=T, lag=15)
# plots pacf values up to lag 15. 
pacf(temp, lag = 15)

#11 COMPUTE LJUNG-BOX TEST FOR WHITE NOISE (NO AUTOCORRELATION)
# to Lag 6
Box.test(temp,lag=6,type='Ljung')
# to Lag 12
Box.test(temp,lag=12,type='Ljung')
# to Lag 18
Box.test(temp,lag=18,type='Ljung')

#12  FIT AN AR(1) MODEL 
#
# Fit an ARIMA model to a univariate time series.
# arima(x, order = c(p, 0, q))

model_ar= arima(temp, order=c(1,0,0), method='ML', include.mean=T)
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

model_ma= arima(temp, order=c(0,0,1), method='ML', include.mean=T)
model_ma
# T-tests on coefficients
print('T-test table for model coefficients')
model= model_ma
matrix(c(model$coef,sqrt(diag(model$var.coef)), 2 * pnorm(-abs(model$coef / sqrt(diag(model$var.coef))))), 
		nrow=length(model$coef), ncol=3, byrow=F,
		dimnames = list(names(model$coef), c("estimates", "test stats", "p-values")))


# RESIDUAL ANALYSIS
Box.test(model_ma$residuals,lag=6,type='Ljung')
Box.test(model_ma$residuals,lag=12,type='Ljung')
Box.test(model_ma$residuals,lag=18,type='Ljung')
acf(model_ma$residuals)

#14  FIT AN ARMA(1,1) MODEL 

model_arma= arima(temp, order=c(1,0,1), method='ML', include.mean=T)
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

#15  FIT AN ARIMA MODEL 
library(forecast)
model_arima=auto.arima(temp, max.P=8, max.Q=8, ic="aic")
model_arima
# RESIDUAL ANALYSIS
Box.test(residuals(model_arima),lag=6,type='Ljung')
Box.test(residuals(model_arima),lag=12,type='Ljung')
Box.test(model_arima$residuals,lag=18,type='Ljung')
acf(residuals(model_arima), na.action=na.remove)

#16 COMPUTE PREDICTIONS USING all the Models

# computes predictions 1-step to 4-step ahead 
# predict(object, n.ahead = 1, newxreg = NULL, se.fit = TRUE, ...)
#	object is the result of an arima fit.
#	n.ahead is the number of steps ahead for which prediction is required.
#	se.fit = T --> standard errors of forecasts are printed

#For AR(1) Model
pr_ar=predict(model_ar,n.ahead=510, se.fit=T)
pr_ar
predar = pr_ar$pred

#For MA(1) Model
pr_ma=predict(model_ma,n.ahead=510, se.fit=T)
pr_ma
predma = pr_ma$pred

#For ARMA(1,1) Model 
pr_arma=predict(model_arma,n.ahead=510, se.fit=T)
pr_arma
predarma = pr_arma$pred

#For ARIMA Model
pr_autoar=predict(model_arima, n.ahead=510, se.fit=T)
pr_autoar
predautoar=pr_autoar$pred

#17 Moving test data into variable
testdata= dataustest$Temperature

#18 Model Evaluation Technique 
library(Metrics)

mae(testdata, predar)
mae(testdata, predma)
mae(testdata, predarma)
mae(testdata, predautoar)

rmse(testdata, predar)
rmse(testdata, predma)
rmse(testdata, predarma)
rmse(testdata, predautoar)

mse(testdata, predar)
mse(testdata, predma)
mse(testdata, predarma)
mse(testdata, predautoar)

#19.PLOT PREDICTIONS FOR 15 STEPS AHEAD 
# FORECAST OF UNITED STATES 5 YEAR TEMPERATURE  
dataus_temp <- dataustrain %>%filter(Year>=1900)%>% select(Year,Temperature) %>% group_by(Year) %>% dplyr::summarise(value=mean(Temperature))
colnames(dataus_temp)[2]<- "Temperature"
temp_us = dataus_train$Temperature
us_ts<-ts(temp_us,start=1900)

model_ar= Arima(us_ts, order = c(1,0,0), include.drift = T)
library(forecast)
future = forecast(model_ar, h = 15)
plot(future)
future





