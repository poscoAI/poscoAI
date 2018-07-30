data <- read.csv('2018_07_23_final.csv',fileEncoding = 'CP949')
data[is.na(data)] <- 0


data(GenCont)
mod <- linearRidge(Phenotypes ~ ., data = as.data.frame(GenCont))
summary(mod)

head(data)

# lmridge

install.packages('lmridge')
library(lmridge)

nn <-nrow(data$철광석_가격)
index <- 1:nrow(data$철광석_가격)
testindex <- sample(index, trunc(length(index)/3))
testset <- data$철광석_가격[testindex,]
trainset <-data$철광석_가격[-testindex,]

if(!is.null(data) && length(data) > 0)
# Fit the ridge regression model:

mod <- lmridge(철광석_가격 ~., data = as.data.frame(trainset), lambda = 0.661)
summary(mod)

# Predict and evaluate it by using MAE function:
pred <- predict(mod, newdata = as.data.frame(testset))
mod
mean(pred)
tail(data)
