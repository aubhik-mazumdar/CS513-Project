Car_data <- read.csv("https://raw.githubusercontent.com/aubhik-mazumdar/CS513-Project/master/datasets/data.csv")
Car_data<- na.omit(Car_data)
Car_data <- Car_data[,-10]
c<-Car_data


index <- sample(nrow(c),as.integer(.75*nrow(c)))

Test_dataset<- c[-index,]
Training_dataset<- c[index, ]
library("kknn")

predict_8<-kknn(MSRP~., Training_dataset, Test_dataset, k=3, kernel="rectangular")
fit_8 <- fitted(predict_8)

wrong_8<- (Test_dataset[,15]!=fit_8)

rate_8<-sum(wrong_8)/length(wrong_8)

table(Test= Test_dataset[,15],fit_8)
rate_8
for(i in c(1:30)){
  predict <- kknn(MSRP~., Training_dataset , Test_dataset , kernel="rectangular", k=i)
  fit <- fitted(predict)
  wrong<- (Test_dataset[,15]!=fit)
  rate<-sum(wrong)/length(wrong)
  rate
  print(rate)
}
K <- c(1:30)
ERROR_RATE <-c(0.8696682464,0.9177386594,0.932295193, 0.9363574814, 0.9390656737, 0.9454976303, 0.9488828707,0.9502369668, 0.9546377793,0.9587000677,
 0.9627623561,0.9668246445, 0.9705484089, 0.9729180772, 0.9766418416, 0.9769803656, 0.9786729858, 0.98070413,0.9820582261, 0.9840893703, 0.9847664184,
 0.9854434665,0.9871360867, 0.9874746107, 0.9871360867, 0.9874746107, 0.9895057549,0.990521327, 0.9911983751, 0.9918754232 )
plot(K,ERROR_RATE, col = 'blue')
lo<-loess(ERROR_RATE~K)
lines(predict(lo),col='red',lwd = 2)

########################################################
rm(list=ls())


Car_data <- read.csv("https://raw.githubusercontent.com/aubhik-mazumdar/CS513-Project/master/datasets/data.csv")
Car_data<- na.omit(Car_data)
Car_data <- Car_data[,-10]
x <- kmeans(Car_data[,15],10)

View(x)
?kmeans

Car_data$price_cluster <- as.factor(x$cluster)

write.csv(Car_data[,c(15,16)],file = "E://Car_price.data2.csv" )
summary(Car_data$price_cluster)
?write.csv
View(Car_data[,c(15,16)])
View(Car_data[,16 == "1"])
##c <- as.matrix(sapply(Car_data,as.numeric))
## c1 <- scale(c)
c<-Car_data


index <- sample(nrow(c),as.integer(.75*nrow(c)))

Test_dataset<- c[-index,]
Training_dataset<- c[index, ]
library("kknn")

##
for(i in c(1:30)){
  predict <- kknn(price_cluster~., Training_dataset [,-15], Test_dataset [-15], kernel="rectangular", k=i)
  fit <- fitted(predict)
  wrong<- (Test_dataset[,16]!=fit)
  rate<-sum(wrong)/length(wrong)
  rate
  print(rate)
}
View(rate)

predict <- kknn(price_cluster~., Training_dataset [,-15], Test_dataset [-15], kernel="rectangular", k=2)
fit <- fitted(predict)
wrong<- (Test_dataset[,16]!=fit)
rate<-sum(wrong)/length(wrong)
rate
table(actual= Test_dataset[,16],fit)
#####################################################################

c1 <- as.matrix(sapply(Car_data,as.numeric))

index1 <- sample(nrow(c1),as.integer(.75*nrow(c1)))

Test_dataset1<- c1[-index,-15]
Training_dataset1<- c1[index,-15 ]

library("neuralnet")

net_bc<- neuralnet(price_cluster~Make+Model+Year+Engine.Fuel.Type+Engine.HP+Engine.Cylinders+Transmission.Type+Driven_Wheels+Number.of.Doors+
                     Vehicle.Style+highway.MPG+city.mpg+Popularity
                   ,Training_dataset1, hidden=5, threshold=0.1)

plot(net_bc)

net_bc2<-compute(net_bc, Test_dataset1[,c(-10,-15)])

net_bc2
ANN=as.numeric(net_bc2$net.result)
ANN_round <- round(ANN)
View(ANN)



table(Actual= Test_dataset1[,15],ANN_round)

wrong<- (Test_dataset1[,15]!=ANN_round)
rate<-sum(wrong)/length(wrong)
rate
## error rate == 0.703 not good
