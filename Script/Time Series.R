setwd("C:/Users/jk000/Documents")
data2<-read.csv("data2.csv")

str(data2)

library(forecast)
fit <- auto.arima(data2$f)
fit

??ndiff
ndiffs(data2$f)

data2<-diff(data2$f)
adf.test(data2$f, alternative = "stationary")


plot(fit)

fore <- forecast(fit, h = 60)
plot(fore)

library('ggplot2')
library('forecast')
library('tseries')


a<-decompose(data2$f)


tsdiag(fit)
