install.packages("ridge")
library(ridge)
setwd("C:/Users/jk000/Desktop/poscoAI/Data")

data<-read.csv("2018_07_18_final")

str(data)

sum(is.na(data))
data<-na.omit(data)

sum(is.na(data1))
dim(data1)

head(data1)

data2<-data1[,c(-1,-2)]

str(data2)

mod <- linearRidge(ì² ê´‘?„_ê°€ê²? ~ ., data = as.data.frame(data2))
summary(mod)


head(data2)

data3<-data2[,c(1,2,3,6,10,12,13,16)]

head(data3)

mod3 <- linearRidge(ì² ê´‘?„_ê°€ê²? ~ ., data = as.data.frame(data3))

summary(mod3)



