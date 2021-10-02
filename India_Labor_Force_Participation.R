install.packages("ggplot2")
library(ggplot2)

install.packages("forecast")
library(forecast)

#Data of India
# 70.5	70.6	70.7	70.8	71	70.9	70.8	70.6	70.5	70.4	70.2	70.6	
# 71	71.4	71.8	72.2	71.3	70.4	69.4	68.5	67.7	67.1	66.6	66.7	
# 66.8	66.9	67	66.9	66.8	66.7	66.6	66.6	66.5	66.4	66.3	66.2	
# 66.2	66.1	66	65.9	65.8

# Import data with Scan
india = scan()

# Conversion to time series
india = ts(india, start=1990)

# About Exponential smoothing with library(forecast)
# Simple Exponential Smoothing: ses()
# Holt's linear trend model: holt(), damped
# Holt-Winters seasonal method: hw()
# Automated Exponential smoothing: ets()

### Holt Linear Trend Model ###
# holt(data, h=forecast length)
?holt # help to understand other parameters

holt_trend = holt(india, h=5)
summary(holt_trend)
plot(holt_trend)

# Phi auto generated
plot(holt(india, h=15, damped=T))
# To find the generated value of Phi
summary(holt(india, h=15, damped=T))
# Manual setting of Phi
summary(holt(india, h=15, damped=T, phi=0.9))


### ARIMA MODEL ###
# AR-autoregressive (seasonality, trend)
# I-integration (differencing of the dataset)
# MA-moving average (movement around a constant mean)

# Arima auto generate
india_arima = auto.arima(india)
summary(india_arima)

plot(forecast(india_arima, h=55))

# Exact calculation of Arima parameter
auto.arima(india, stepwise = F, approximation = F)
# Overview plot - models
holttrend = holt(india, h=10)
holtdamped = holt(india, h=10, damped=T)
arimafore = forecast(auto.arima(india), h=10)

library(ggplot2)
# 3 forecast lines as comparison 
autoplot(india)+
  forecast::autolayer(holttrend$mean, series="Holt Linear Trend")+
  forecast::autolayer(holtdamped$mean, series="Holt Damped Trend")+
  forecast::autolayer(arimafore$mean, series="ARIMA")+
  xlab("year") + ylab("Labour Force Participation Rate Age 25-54")+
  guides(color=guide_legend(title = "Forecast Method")) + theme(legend.position = c(0.8,0.8))+
  ggtitle("India") + theme(plot.title=element_text(family="Times", hjust=0.5, color="red",face="bold", size=18))

# 4 lines showing the trends one over the other
autoplot(india) + geom_line(size=2)+
  forecast::autolayer(holttrend$fitted, series="Holt Linear Trend", size=1.1)+
  forecast::autolayer(holtdamped$fitted, series="Holt Damped Trend", size=1.1)+
  forecast::autolayer(arimafore$fitted, series="ARIMA")+
  xlab("year") + ylab("Labour Force Participation Rate Age 25-54")+
  guides(color=guide_legend(title = "Forecast Method")) + 
    theme(legend.position = c(0.8,0.8))+
  ggtitle("India") + 
    theme(plot.title=element_text(family="Times", hjust=0.5,face="bold", size=18))



