# Ridge Regression 
data = na.omit(data) 
head(data)


data<-data[,c(-1,-2)]

colnames(data)<-c("y", colnames(data[,-1]))



head(data)

x = model.matrix(y ~ ., data)[, -1] 
y = data$y 
library(glmnet)

grid = 10^seq(10, -2, length = 100) 


ridge.mod = glmnet(x, y, alpha = 0, lambda = grid) 
dim(coef(ridge.mod))

which.min(ridge.mod$lambda)

ridge.mod$lambda[100]

coef(ridge.mod)[, 100]

sqrt(sum(coef(ridge.mod)[-1, 100]^2))


predict(ridge.mod, s = 100, type = "coefficients")


set.seed(1) 
train = sample(1:nrow(x), nrow(x)/2) 
test = (-train) 
y.test = y[test] 
ridge.mod = glmnet(x[train, ], y[train], alpha = 0, lambda = grid, thresh = 1e-12) 
ridge.pred = predict(ridge.mod, s = 4, newx = x[test, ]) 
mean((ridge.pred - y.test)^2)

mean((mean(y[train]) - y.test)^2)

ridge.pred = predict(ridge.mod, s = 1e+10, newx = x[test, ]) 
mean((ridge.pred - y.test)^2)

ridge.pred = predict(ridge.mod, s = 0, newx = x[test, ]) 
mean((ridge.pred - y.test)^2)

lm(y ~ x, subset = train)

predict(ridge.mod, s = 0, type = "coefficients")


set.seed(1) 
cv.out = cv.glmnet(x[train, ], y[train], alpha = 0) 
plot(cv.out) 
bestlam = cv.out$lambda.min 
bestlam


ridge.pred = predict(ridge.mod, s = bestlam, newx = x[test, ]) 
mean((ridge.pred - y.test)^2)


out = glmnet(x, y, alpha = 0) 
predict(out, type = "coefficients", s = bestlam)
