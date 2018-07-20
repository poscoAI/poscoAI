setwd("C:/Users/jk000/Desktop/poscoAI/Data")

data2<-read.csv("test_mimi.csv")

str(data2)

sum(is.na(data2$철광석_가격))

na.omit(data2$철광석_가격)

library(forecast)
fit <- auto.arima(data2$철광석_가격)
fit

??ndiff
ndiffs(data2$철광석_가격)

data2$z<-diff(data2$철광석_가격)

??adf.test
library(tseries)
str(data2)
adf.test(data2$철광석_가격, alternative = "stationary")


plot(fit)

fore <- forecast(fit, h = 60)
plot(fore, xlixm=c(250, 300))

library('ggplot2')
library('forecast')
library('tseries')


a<-decompose(data2$철광석_가격)


tsdiag(fit)
