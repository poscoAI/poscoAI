setwd("C:/Users/sungho/Documents/poscoAI/Data")

data<-read.csv("Monthly_data.csv",header=T)

data_w<-read.csv("Weekly_data.csv",header=T)

head(data)
head(data_w)

data[,1]

day_m<-vector()
day_m<-strptime(as.character(data[,1]),"%Y-%m-%d")

day_m
day_w<-vector()
day_w<-strptime(as.character(data_w[,1]),"%Y-%m-%d")
day_w
months(day_m)
months(day_w)

months(day[1])
months(day_w[1])

months(day[1])==months(day_w[1])
j<-1
data_t<-matrix(NA,nrow=263,ncol=31)
for (i in 1:length(day_w)) {
  if(months(day_m[j])==months(day_w[i])){
    data_t[i,]<-as.matrix(cbind(data[j,],data_w[i,]))
  }
  else{  
    j<-j+1  
    if(months(day_m[j])==months(day_w[i])){
      data_t[i,]<-as.matrix(cbind(data[j,],data_w[i,]))
    }
  }
}
colnames(data_t)<-c(colnames(data),colnames(data_w))


as.matrix(cbind(data[200,],data_w[200,]))
data_w[200,]

data[,3]
?write.csv
write.csv(data_t,file="total.csv")
head(data_t)

length(data_t[,1])

data_t[262,]


library("vars")

head(data)
data<-data[,-1]
data<-data[,-2]
data<-data[,-3]
data<-data[,-4]
data<-data[,-6]
colnames(data)<-c("x1","y","x2","x3","x4")

data<-data[,c("y","x1","x2","x3","x4")]
data<-data[-250,]

VARselect(data, lag.max = 8, type = "both")



p1ct <- VAR(data, p = 2, type = "both")
p1ct
summary(p1ct, equation = "e")
plot(p1ct, names = "e")

predict(p1ct,n.ahead = 5)
predict(p1ct)

