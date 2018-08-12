head(weather)

weather[is.na(weather)] <- 0
sum(is.na(weather))

weather1 <- lapply(weather,fun)
head(weather1)

write.csv(weather1,file='weather1.csv',fileEncoding = 'CP949')
fun <- function(x) {
  rep(x, each = 3)
}


weather <- read.csv('weather_final.csv',fileEncoding = 'CP949')
lunch <- read.csv('lunch.csv',fileEncoding = 'CP949')


dinner <- dinner_final[,-1:-2]

head(dinner)

lunch_final