data_m <- read.csv('Monthly_data.csv',fileEncoding = 'CP949')

data_y <- read.csv('data.csv')
data_y

head(data_y)
data2 <- as.data.frame(data_y)
fit = lm(f ~ b+c+d+g+h , data= data2)

summary(fit)

install.packages("car")
library("car")
vif(fit)
sqrt(vif(fit))
plot(fit)
z
z<-shapiro.test(data2$f)
plot(z)
