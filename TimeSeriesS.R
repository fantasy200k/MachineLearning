# -------------  Demo on fitting non-stationary data ----

# import statsmodels.tsa.stattools import adfuller

# Read the time series data 
df <- read.csv("D:/stu/nifty_ts.csv")
attach(df)
head(df)
# Data Exploration
plot(Prices)
View(df)
# 
library(tseries)
# Forecasting Return
ar.ols()
# ------------------------Step 1 Model Estimation -------------------
# Stationarity check - Augmented Dicky-Fuller Test
adf.test(Prices,alternative = "stationary")

# p value is .95 hence v accept the null hyp , ie., its a non stationary 
# if the data is non stationary , what needs to be done ?
# v need to do differencing


# what should b the order of differencing ?
d.Prices <- diff(Prices)
summary(Prices)
summary(d.Prices)
plot(d.Prices)
adf.test(d.Prices,alternative = "stationary")
# now the p value is .01, that means we r accepting the alter hyp
# that it is stationary 
# The 1st level of differencing itself makes it as stationary, 
# no need of going to next level of order of differencing  
acf(d.Prices)
# only the 1st order lag is significant and the rest are all insignificant
# Now lets use pacf
pacf(d.Prices)

# Now the lags are significant , will inclue some of these mse lags 
# just to c whether it improves or doesnt improves
library(forecast)
auto.arima(d.Prices)
# ------------------------Step 2 Model Estimation -------------------
# Arima(1,1,0)
# c(1,0,0) - the middle must have 0 coz v hv already obtained differencing
# using diff 
arima(d.Prices, order=c(1,0,0))
# Arima(2,1,0)
arima(d.Prices, order=c(2,0,0))
# the AIC value is increasing there is no point in increasing AR term
# Arima(1,1,1)
arima(d.Prices, order=c(1,0,1))
# Arima(1,1,2)
arima(d.Prices, order=c(1,0,2))

# -------------------------Step 3: Diagnostics ----------------------
arima.final <- arima(d.Prices,c(1,0,1))
par(mfrow=c(2,2))
tsdiag(arima.final)

# Choose the one that has least AIC
# and significant co-efficients
# Forecasting using final model
# arima.final <- arima(d.Prices,c(1,0,0))
arima.final <- arima(d.Prices,c(0,1,2))
predicted <- predict(arima.final,n.ahead=5)
# v r going to preidct the next 5 observations
predicted

arima.final <- arima(Prices, c(1,1,1))
predicted <- predict(arima.final,n.ahead=5)
# v r going to preidct the next 5 observations
predicted


# -------------  Demo on fitting stationary data ----

# Data exploration
plot(Returns)


# forecasting - Return
#------------------ Step 1 Model Identification ---------------
# v need to identify which model is suitable 

# first v will do this Stationary check - Dicky fuller test
adf.test(Returns,alternative="stationary")
# alternative hypothesis
# p value < .05. Hence the data is stationary 
# since p value is .01 , v r rejecting the null hyp
# that it is non stationary , v r accepting the 
# alternative hyp that it is stationary 

# 
acf(Returns)
pacf(Returns)
# what is acf & pacf , these are the functional form of data  which
# tells us the order of AR and MA terms 

# if you look at the chart , the only significant term is the 1st (2nd one) one
# don't confuse with the very first one it is the correlation 
# anything lie above hte blue line is the significant one.
# another thing what v notice here is it is decreasing over time 

# Evidence of AR(1)

# -------------Step 2 Model estimation -----------------

# the 1st term is AR, 2nd term is differencing , v r not doing any differencing
# hence the 2nd term is 0 and also v r not using 
# MA term (which is the 3rd 0) in the 1st model , will include that later on 
arima(Returns,order=c(1,0,0))
View(df)
# Lets try to include another AR term
arima(Returns,order=c(2,0,0))


arima(Returns,order=c(1,0,1))

auto.arima(Returns)

#-------------- Step 4 Model Diagnosis --------------


arima.final <- arima(Returns,c(1,1,1))

predicted <- predict(arima.final, n.ahead=10)
predicted

par(mfrow=(c(3,3)))
par(mar= rep(2,4))
tsdiag(arima.final)
acf(d.Prices)
