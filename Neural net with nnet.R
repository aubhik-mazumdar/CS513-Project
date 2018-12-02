Car_data <- read.csv("data.csv")
Car_data<- na.omit(Car_data)
Car_data <- Car_data[,-10]
c <- as.matrix(sapply(Car_data,as.numeric))
c1 <- scale(c)



index <- sample(nrow(c),as.integer(.75*nrow(c)))

Test_dataset<- c[-index,]
Training_dataset<- c[index, ]

library("neuralnet")

net_bc<- neuralnet(MSRP~Make+Model+Year+Engine.Fuel.Type+Engine.HP+Engine.Cylinders+Transmission.Type+Driven_Wheels+Number.of.Doors+
                     Vehicle.Style+highway.MPG+city.mpg+Popularity
                   ,Training_dataset, hidden=5, threshold=0.1)
?sapply
plot(net_bc)

################################################################################################
library(nnet)
res <- nnet(MSRP ~ .,
            data=Training_dataset,
            size=10, linout=TRUE, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)
pred <- predict(res, newdata=Test_dataset[,-15])
pred <- round(pred)
# Mean error
mean_error <- sum(abs(pred-Test_dataset[,15]))/length(pred)
library(Metrics)
err <- rmse(Test_dataset[,15],pred)
err
mean_error
mean_percent <- mean_error/mean(Test_dataset[,15])
mean_percent
error <- as.data.frame(pred-test_set[,16])
################################################################################################

net_bc2<-compute(net_bc, Test_dataset[,c(-10,-15)])

net_bc2
ANN=as.numeric(net_bc2$net.result)
View(ANN)

table(ANN)


ANN_round<-round(ANN)

table(ANN_round)
ANN




table(Actual= Test_dataset[,15],ANN_round)