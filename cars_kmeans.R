rm(list = ls())
setwd('~/Desktop/Fall_2018/KDD/CS513-Project/datasets')
data = read.csv('data.csv',na.strings = 'N/A')

# Remove the column for Vehicle.Size
data <- data[,-10]

data <- na.omit(data)
data$Make <- factor(data$Make)
data$Make

set.seed(100)
idx <- sample(c(1:nrow(data)),0.75*nrow(data))
train <- data[idx,]
test <- data[-idx,]

#average_price_by_fuel_type
library(caroline)
# Calculating the average price for each car type
average_price_by_fuel_type <- groupBy(data,by='Engine.Fuel.Type',clmns='MSRP',
                                      aggregation = 'mean',na.rm = TRUE,full.names = TRUE)

# Create the table for average price
average_price_engine <- data.frame(Engine.Type = levels(data$Engine.Fuel.Type),
                                   average_price=average_price_by_fuel_type[,1])

# Calculating the average price for each car type
average_price_by_make <- groupBy(data,by='Make',clmns='MSRP',
                                      aggregation = 'mean',na.rm = TRUE,full.names = TRUE)

# Create the table for average price
average_price_make <- data.frame(Make = levels(data$Make),
                                   average_price=average_price_by_make[,1])


clusters <- kmeans(data[,c(5,6,9,12,13,14,15)],47,iter.max = 100)
data$MakeCluster <- as.factor(clusters$cluster)
str(clusters)

# Average price for each cluster
library(caroline)
g <- groupBy(data,by='MakeCluster',aggregation = c('mean'),clmns='MSRP',na.rm=TRUE, full.names = TRUE)
g
