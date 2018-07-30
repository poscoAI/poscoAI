# Ridge Regression 
data <- read.csv('2018_07_23_pred_final2.csv',fileEncoding = 'CP949')
data = na.omit(data) 
head(data)

data = data[,-1:-2]

head(data)

x = model.matrix(철광석_가격 ~ ., data)[, -1] 
y = data$철광석_가격
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

# 아래 방식이 가장 좋다.
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
tail(data)

 
out = glmnet(x, y, alpha = 0) 
predict(out, type = "coefficients", s = bestlam)
