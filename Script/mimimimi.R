setwd("C:/Users/sungho/Documents/poscoAI/Data")

data<-read.csv("Monthly_data.csv",header=T)

data_w<-read.csv("Weekly_data.csv",header=T)

data<-data[,-3]
data<-data[,-4]
data<-data[,-5]
data<-data[,-11]
data<-data[,-11]
data<-data[-1,]
data<-data[-1,]
#data<-data[,-2]#결측값 제거
#data<-data[-59,]
#data<-data[-59,]
data[60,]
head(data)
names(data)
length(data[,1])

data_w<-data_w[,-5]
data_w<-data_w[,-6]
data_w<-data_w[,-7]
data_w<-data_w[,-8]
data_w<-data_w[,-9]
data_w<-data_w[,-10]
names(data_w)
head(data_w)
data_w[258,1]


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
length(data[1,])
length(data_w[1,])

number_m<-0
current_m<-1
data_t<-matrix(NA,nrow=263,ncol=19)
current_w<-1
while(current_w<=length(day_w)){
 if(months(day_m[current_m])==months(day_w[current_w])){
    number_m<-number_m+1
    current_w<-current_w+1
  }
  else{
    dif<-(data[current_m+1,-1]-data[current_m,-1])/number_m
    first_value<-(data[current_m,-1]-((number_m*(number_m-1))/2)*dif)/number_m
    for (same_m in number_m:1) {
      data_t[(current_w-same_m),]<-as.matrix(cbind(data_w[(current_w-same_m),1],(first_value+(number_m-same_m)*dif),data_w[(current_w-same_m),-1]))
    }
    current_m<-current_m+1
    number_m<-0
  }
  if(current_w==263){
    dif<-(data[current_m+1,-1]-data[current_m,-1])/number_m
    first_value<-(data[current_m,-1]-((number_m*(number_m-1))/2)*dif)/number_m
    for (same_m in number_m:1) {
      data_t[(current_w-same_m),]<-as.matrix(cbind(data_w[(current_w-same_m),1],(first_value+(number_m-same_m)*dif),data_w[(current_w-same_m),-1]))
    }
  }
}
colnames(data_t)<-c("날짜",colnames(data[,-1]),colnames(data_w[,-1]))


head(data_t)


write.csv(data_t,file="test_mimi.csv")

