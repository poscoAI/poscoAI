total <- read.csv('/Users/statstics/Desktop/DataAnalysis/poscoAI/Data/2018_07_20_hohyun.csv',fileEncoding = 'CP949')
total
head(total)
library(tree)
total <- na.omit(total[, -1:-2])
total
str(total)
sum(is.na(total))
n <- dim(total)[1] 
set.seed(12)
train <- sample(n)
par(family="NanumGothic")
htree <- tree(Price ~ .,total)
plot(htree) + text(htree,pretty=T)
htree


library(rpart)
hpart <- rpart(Price ~ .,total)
plot(hpart) + text(hpart,pretty=T)

library(party)


# Random Forest
install.packages("randomForest")
library(randomForest)
library(MASS)
n <- dim(total)[1]
set.seed(1)
train <- sample(n, n/2)
total.test <- total$Price[-train]
dim(total)
set.seed(1)
bag.total = randomForest(Price ~ ., data = total, mtry = 4, importance = TRUE)
bag.total
plot(bag.total)


