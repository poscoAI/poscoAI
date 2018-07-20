total <- read.csv('/Users/statstics/Desktop/DataAnalysis/poscoAI/Data/hohyun.csv',fileEncoding = 'CP949')
total
head(total)
library(tree)
total <- na.omit(total[, -1])
total
str(total)
sum(is.na(total))
n <- dim(total)[1] 
set.seed(12)
train <- sample(n, n/2)
par(family="NanumGothic")
htree <- tree(철광석_가격 ~ .,total, subset = train)
plot(htree) + text(htree,pretty=T)
htree


library(rpart)
hpart <- rpart(철광석_가격 ~ .,total, subset = train)
plot(hpart) + text(hpart,pretty=T)

library(party)


# Random Forest
library(randomForest)
library(MASS)
n <- dim(total)[1]
set.seed(1)
train <- sample(n, n/2)
total.test <- total$철광석_가격[-train]
set.seed(1)
bag.total = randomForest(철광석_가격 ~ ., data = total, subset = train, mtry = 4,
                          importance = TRUE)
bag.total
plot(bag.total)


