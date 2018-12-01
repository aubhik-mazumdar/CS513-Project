Car_data <- read.csv("https://raw.githubusercontent.com/aubhik-mazumdar/CS513-Project/master/datasets/data.csv")
Car_data<- na.omit(Car_data)
Car_data <- Car_data[,-10]
c <- as.matrix(sapply(Car_data,as.numeric))
c1 <- scale(c)



index <- sample(nrow(c1),as.integer(.75*nrow(c1)))

Test_dataset<- c1[-index,]
Training_dataset<- c1[index, ]

library("neuralnet")

net_bc<- neuralnet(MSRP~Make+Model+Year+Engine.Fuel.Type+Engine.HP+Engine.Cylinders+Transmission.Type+Driven_Wheels+Number.of.Doors+
                     Vehicle.Style+highway.MPG+city.mpg+Popularity
                   ,Training_dataset, hidden=5, threshold=0.1)
?sapply
plot(net_bc)

net_bc2<-compute(net_bc, Test_dataset[,c(-10,-15)])

net_bc2
ANN=as.numeric(net_bc2$net.result)
View(ANN)

table(ANN)


ANN_round<-round(ANN)

table(ANN_round)
ANN




table(Actual= Test_dataset[,15],ANN_round)