rm(list = ls())
setwd('~/Desktop/Fall_2018/KDD/CS513-Project/datasets')
data = read.csv('data.csv',na.strings = 'N/A')

# Remove the column for Market.Category, Vehicle.Size
data <- data[,c(-10,-11)]


data <- na.omit(data)
data$Make <- factor(data$Make)

set.seed(100)
idx <- sample(c(1:nrow(data)),0.75*nrow(data))
train <- data[idx,]
test <- data[-idx,]

library(kknn)
model <- kknn(Make ~.,train,test,k=5)
fit <- fitted(model)

t <- table(actual = test$Make,fit)
View(t)

wrong <- (test$Make != fit)
rate <- sum(wrong)/length(wrong)
rate

#Got 96% accuracy in predicting the make!
accuracy <- 1 - rate
accuracy

