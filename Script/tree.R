total <- read.csv('/Users/statstics/Desktop/DataAnalysis/poscoAI/Data/2018_07_20_hohyun_final.csv',fileEncoding = 'CP949')
head(total);tail(total)
library(tree)
total <- total[-185,]
total <- total[,-1:-2]
total <- total[,-2]
total <- total[-181,]
tail(total)

dim(total)
str(total)
total <- na.omit(total)
sum(is.na(total))
tail(total)
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

write.

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

importance(bag.total)

ntree <- c(400,500,600)
mtry <- c(3:5)
param <- data.frame(n = ntree, m = mtry)
param

for(i in param$n){
  cat('ntree=',i,'\n')
  for(j in param$m){
    cat('mtry')
    model_total = randomForest(Price ~ ., data = total, ntree = i, mtry = j, importance = TRUE)
    
  print(model_total)
    
  }
  
}


model_total = randomForest(Price ~ ., data = total, ntree = 600, mtry = 5, importance = TRUE)
model_total

varImpPlot(model_total)
total
train <- total[-174:-178,]
total.test <- total[174:178,]
dim(train)
dim(total.test)
head(train)

summary(model_total)
names(model_total)
head(model_total)

total$medv.hat = predict(model_total,newdata= total.test)
mean((total$medv-total$medv.hat)^2) #mean square error(mse)


dim(total)
total[178,]
pred4 = predict(model_total, newdata = total.test)
pred4
t4 = table(total$Class, pred4)
diag(t4)
sum(diag(t4)) / sum(t4)