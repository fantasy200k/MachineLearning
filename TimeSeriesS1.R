library(caret)

# -------------  Demo on fitting non-stationary data ----

# Read the time series data 
df <- read.csv("D:/stu/nifty_ts.csv")
str(df)
attach(df)
head(df)


# 
library(tseries)
# Forecasting Return

# ------------------------Step 1 Model Estimation -------------------
# Stationarity check - augemented Dicky-Fuller Test
adf.test(Prices,alternative = "stationary")



# what should b the order of differencing ?
d.Prices <- diff(Prices)
mode(d.Prices)
summary(Prices)
summary(d.Prices)
plot(d.Prices)
adf.test(d.Prices,alternative = "stationary")
plot(d.Prices)
# now the p value is .01, that means we r accepting the alter hyp
# that it is stationary 


acf(d.Prices)
# only the 1st order lag is significant and the
# rest are all insignificant
# Now lets use pacf
pacf(d.Prices)



# ------------------------Step 2 Model Estimation -------------------
# Arima(1,1,0)
# c(1,0,0) - the middle must have 0 coz v hv 
# already obtained differencing
# using diff 
# arima(p,d,q)
# p => AR , d => differencing q=> moving Averages

arima(d.Prices, order=c(1,0,0))

# Arima(2,1,0)  ar(2)
arima(d.Prices, order=c(2,0,0))
# the AIC value is increasing there is no point
# in increasing AR term
# Arima(1,1,1)
arima(d.Prices, order=c(1,0,1))
# Arima(1,1,2)
arima(d.Prices, order=c(0,1,2))
library(forecast)
auto.arima(d.Prices)
# -------------------------Step 3: Diagnostics ----------------------
arima.final <- arima(d.Prices,c(0,1,2))
arima.final

plot(arima.final)

par(mfrow=c(2,2))
tsdiag(arima.final)

# Choose the one that has least AIC
# and significant co-efficients
# Forecasting using final model
# arima.final <- arima(d.Prices,c(1,0,0))
arima.final <- arima(d.Prices,c(0,1,2))
predicted <- predict(arima.final,d.Prices)
test <- d.Prices[40:100]
predicted <- predict(arima.final,d.Prices[40:100])


str(df)
  
library(DMwR)
accmeasures1=regr.eval(test, predicted)


arima.final <- arima(Prices, c(1,1,1))

predicted <- predict(arima.final,n.ahead=5)


# -------------  Demo on fitting stationary data ----
dev.off()
# Data exploration
plot(Returns)


# forecasting - Return
#------------------ Step 1 Model Identification ---------------
# v need to identify which model is suitable 

# first v will do this Stationary check - 
# Dicky fuller test
adf.test(Returns,alternative="stationary")



# 
acf(Returns)
pacf(Returns)


# Evidence of AR(1)

# -------------Step 2 Model estimation -----------------

arima(Returns,order=c(1,0,0))

# Lets try to include another AR term
arima(Returns,order=c(2,0,0))

auto.arima(Returns)
arima(Returns,order=c(1,1,1))


#-------------- Step 4 Model Diagnosis --------------


arima.final <- auto.arima(Returns)
arima.final <- arima(Returns,order=c(1,1,1))


predicted <- predict(arima.final, n.ahead=5)
predicted




