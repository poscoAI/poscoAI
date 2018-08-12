#install.packages('lmridge')

library(lmridge)
data <- read.csv('yaya.csv',fileEncoding = 'CP949')
data <- data[,c(-1,-2)]

tail(data)

# lmridge
#install.packages('lmridge')
sum(is.na(data))
head(data)
trainMat = model.matrix(철광석_가격~., data[1:181,])
testMat = model.matrix(철광석_가격~., data[182:186,])
grid = 10 ^ seq(10, -10, length=100)

ridgeModel <- lmridge(철광석_가격 ~., data, lambda = 0.05)
summary(ridgeModel)
optLambda = ridgeModel$lambda.min
optLambda
summary(testMat)

# Fit the ridge regression model:
testset <- data[182:186,]
trainset <-data[1:181,]


ridgeModel <- lmridge(철광석_가격 ~., data = trainset,lambda = 0.05)
summary(ridgeModel)


ridgePred <- predict(ridgeModel, newdata = as.data.frame(testset))
y.pred <- as.matrix(cbind(const=1,trainset[,-1])) %*% coef(ridgeModel)
dim(coef(ridgeModel))
dim(as.matrix(cbind(const=1,trainset)))
head(trainset)

mean((data[182:186, ][, "철광석_가격"] - ridgePred)^2)
tail(data)


# Lasso
library("glmnet")
library("mvtnorm") 

## generate the data
signal <- sqrt(mean((x %*% beta)^2))
sigma <- as.numeric(signal / sqrt(10))  # SNR=10
y <- x %*% beta + rnorm(n)

## Lasso estimator

library(glmnet)
library("mvtnorm") 


## Lasso estimator
#install.packages('simulator')
library(simulator)
library(glmnet)
lasso <- new_method("lasso", "Lasso",
                    method = function(model, draw, lambda = NULL) {
                      if (is.null(lambda))
                        fit <- glmnet(x = model$x, y = draw, nlambda = 50,
                                      intercept = FALSE)
                      else {
                        fit <- glmnet(x = model$x, y = draw, lambda = lambda,
                                      intercept = FALSE)
                      }
                      list(beta = fit$beta, yhat = model$x %*% fit$beta,
                           lambda = fit$lambda, df = fit$df)
                    })

ridge <- new_method("ridge", "Ridge",
                    method = function(model, draw, lambda = NULL) {
                      sv <- svd(model$x)
                      df_fun <- function(lam) {
                        # degrees of freedom when tuning param is lam
                        sum(sv$d^2 / (sv$d^2 + lam))
                      }
                      if (is.null(lambda)) {
                        nlambda <- 50
                        get_lam <- function(target_df) {
                          f <- function(lam) df_fun(lam) - target_df
                          uniroot(f, c(0, 100 * max(sv$d^2)))$root
                        }
                        lambda <- sapply(seq(1, nrow(model$x),
                                             length = nlambda), get_lam)
                      }
                      df <- sapply(lambda, df_fun)
                      beta <- sapply(lambda, function(r) {
                        d <- sv$d / (sv$d^2 + r)
                        return(sv$v %*% (d * crossprod(sv$u, draw)))
                      })
                      list(beta = beta, yhat = model$x %*% beta,
                           lambda = lambda, df = df)
                    })
?lasso
new_method(y ~ ., data, Method = 'lasso')
