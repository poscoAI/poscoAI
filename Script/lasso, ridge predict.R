setwd("C:/Users/jk000/Desktop/poscoAI/Data")

data<-read.csv("2018_07_20_final.csv")

str(data)
head(data)

sum(is.na(data))

data1<-na.omit(data)

sum(is.na(data1))
dim(data1)

head(data1)

data2<-data1[,c(-1,-2)]

names(data2)[1] <- "y"

head(data2)


data<-data2
set.seed(1)
trainRows = sample(dim(data)[1], ceiling(dim(data)[1]/2))
train = is.element(c(1:dim(data)[1]),trainRows)
test = !train

head(data2)



#Linear Regression
fit = lm(y~., data=data[train, ])
fit.pred = predict(fit, data[test, ])
fit.pred
mean((data2[test, ][, "y"] - fit.pred)^2)

summary(fit)
plot(fit)
shapiro.test(fit)
shapiro.test(resid(fit))


??cv.glmnet
library(glmnet)

#Ridge Regression
trainMat = model.matrix(y~., data=data[train, ])
testMat = model.matrix(y~., data=data[test, ])
grid = 10 ^ seq(10, -10, length=100)
ridgeModel = cv.glmnet(trainMat, data[train, ][, "y"], alpha=0, 
                       lambda=grid)
optLambda = ridgeModel$lambda.min
optLambda
summary(testMat)


#Ridge MSE
ridgePred = predict(ridgeModel, newx=testMat, s=optLambda)
mean((data[test, ][, "y"] - ridgePred)^2)


#Lasso Model
lassoModel = cv.glmnet(trainMat, data[train, ][, "y"], alpha=1, 
                       lambda=grid)
optLambda = lassoModel$lambda.min
optLambda




#Test MSE - Lasso
lassoPred = predict(lassoModel, newx=testMat, s=optLambda)
mean((data[test, ][, "y"] - lassoPred)^2)


########
# predict

#Linear Regression
fit = lm(y~., data=data)
fit.pred = predict(fit)
fit.pred

summary(fit)
plot(fit)
shapiro.test(fit)
shapiro.test(resid(fit))


??cv.glmnet
library(glmnet)

#Ridge Regression
trainMat = model.matrix(y~., data=data)
testMat = model.matrix(y~., data=data)
grid = 10 ^ seq(10, -10, length=100)
ridgeModel = cv.glmnet(trainMat, data[, "y"], alpha=0, 
                       lambda=grid)
optLambda = ridgeModel$lambda.min
optLambda
summary(testMat)


#Ridge MSE
ridgePred = predict(ridgeModel, newx=testMat, s=optLambda)
ridgePred
# mean((data[test, ][, "y"] - ridgePred)^2)


#Lasso Model
lassoModel = cv.glmnet(trainMat, data[, "y"], alpha=1, 
                       lambda=grid)
optLambda = lassoModel$lambda.min
optLambda




#Test MSE - Lasso
lassoPred = predict(lassoModel, newx=testMat, s=optLambda)
# mean((data[test, ][, "y"] - lassoPred)^2)

lassoPred












