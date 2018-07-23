setwd("C:/Users/jk000/Desktop/poscoAI/Data")

data<-read.csv("2018_07_23_final.csv")

head(data)
data<-data[,c(-1,-2)]


head(data)
str(data)
names(data)[1] <- "y"

sum(is.na(data))

# data<-na.omit(data)

train<-data[1:177,]
test<-data[178:182,]
test
test<-test[,-1]

train<-as.data.frame(train)
test<-as.data.frame(test)


#Linear Regression
fit = lm(y~., data=train)
predict(fit, as.data.frame(test))
fit.pred = predict(fit, test, interval="predict")


fit.pred
mean((data2[test, ][, "y"] - fit.pred)^2)

summary(fit)
plot(fit)
shapiro.test(fit)
shapiro.test(resid(fit))


??cv.glmnet
library(glmnet)

#Ridge Regression
trainMat = model.matrix(y~., data=train)
testMat = model.matrix(y~., data=test)
grid = 10 ^ seq(10, -10, length=100)
ridgeModel = cv.glmnet(trainMat, train[, "y"], alpha=0, 
                       lambda=grid)
optLambda = ridgeModel$lambda.min
optLambda
summary(testMat)


#Ridge MSE
predict(ridgeModel, newx=as.data.frame(test), s=optLambda)
ridgePred = predict(ridgeModel, newx=testMat, s=optLambda)

as.matrix(cbind(const=1,test))%*%coef(ridgeModel)

mean((data[test, ][, "y"] - ridgePred)^2)


#Lasso Model
lassoModel = cv.glmnet(trainMat, train[, "y"], alpha=1, 
                       lambda=grid)
optLambda = lassoModel$lambda.min
optLambda

as.matrix(cbind(const=1,test))%*%coef(lassoModel)



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



library(glmnet)
#Test MSE - Lasso
lassoPred = predict(lassoModel, newx=testMat, s=optLambda)
# mean((data[test, ][, "y"] - lassoPred)^2)

lassoPred












