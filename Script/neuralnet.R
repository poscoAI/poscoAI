posco <- read.csv('english.csv',fileEncoding = 'CP949')
library(nnet)
data(posco)
dat <- posco

install.packages('neuralnet')
library(neuralnet)
set.seed(631560)
hd <- dat[, -1:-2]
n <- dim(hd)[1]

hd <- na.omit(hd)
str(hd)
sum(is.na(hd))
train <- sample(1:n, n/2)
fith <- neuralnet(Price ~ US_income+aluminium+copper+yeon+a.yeon+juseok+nikel+BHP_close+RIO_close+VALE_close+PDS, data = hd[train,], hidden = c(10, 10), err.fct = "sse", act.fct = "tanh", linear.output = FALSE)
par(mfrow = c(1,2))
par(mar=c(5,5,4,2)+0.4)
par(mfcol=c(2,2), mar=c(4,4,0.5,0.5), oma=c(1.5,2,1,1))
plot(fith,rep = 'best', ylab = 'babo')
plot(hd)
fith$net.result

summary(fith)
predh <- compute(fith, hd[-train,] )$net.result
pclh <- max.col(predh)
predclh <- rep("Yes", length(pclh))
predclh[pclh == 2] <- "No"
table(pred = predclh, true = hdata$AHD[-train])
