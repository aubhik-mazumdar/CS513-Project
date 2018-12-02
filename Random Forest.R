df <- read.csv('data.csv')
df3 <- read.csv('data.csv')
# Without make n model
df2<- df[,c(-1,-2)]
df2 <- na.omit(df2)
df <- na.omit(df)

# Split into training and test
library(caTools)
set.seed(123)
split = sample.split(df$MSRP, SplitRatio = 0.7)
training_set = subset(df, split == TRUE)
training_set <- data.matrix(training_set)
test_set = subset(df, split == FALSE)
test_set <- data.matrix(test_set) 

# Applying random forest
library(randomForest)
regressor <- randomForest(MSRP ~ ., data = training_set, ntree = 1000, importance = T)
importance(regressor)
varImpPlot(regressor)
pred <- predict(regressor, test_set[,-16])
pred2 <- predict(regressor, training_set[,-16])
# Mean error
mean_error <- sum(abs(pred-test_set[,16]))/length(pred)
mean_error
library(Metrics)
rmse_train <- rmse(training_set[,16], pred2)
rmse_test <- rmse(test_set[,16],pred)
err
error <- as.data.frame(test_set[,16]-pred)

# Mean percent error
mean_percent <- mean_error/mean(test_set[,16])
mean_percent

# Manually extracting the top 10 overpriced references
overpriced <- df3[c(6351,2922,9685,4025,7024,1399,5070,1627,474,2897),c(1,2,3,16)]
overpriced

# Manually extracting the top 10 underpriced references
underpriced <- df3[c(7058,7059,7056,10529,7560,617,4668,9432,599,8296),c(1,2,3,16)]
underpriced

# Let's look at the popularity of each Make
library(dplyr)
Make_n_Popularity <- select(df, Make, Popularity)
Make_n_Popularity <- unique(Make_n_Popularity[,1:2])
Make_n_Popularity <- as.data.frame(Make_n_Popularity)
Make_n_Popularity$Popularity <- as.numeric(Make_n_Popularity$Popularity)
Make_n_Popularity <- Make_n_Popularity[order(Make_n_Popularity$Popularity),]

# Plotting the graph
library(ggplot2)
ggplot(data = Make_n_Popularity, aes(x=Make, y=Popularity) ) +
  geom_segment( aes(x=Make ,xend=Make, y=0, yend=Popularity), color="grey", stat = 'identity') +
  geom_point(size=3, color="purple1") +
  coord_flip()  +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none",
    plot.title = element_text(color = 'purple4', hjust = 0.5, size=14, face="bold"),
    axis.title.x = element_text(color = 'purple4',size=14, face="bold"),
    axis.title.y = element_text(color = 'purple4',size=14, face="bold"),
    axis.text.x=element_text(colour="purple4"),
    axis.text.y=element_text(colour="purple4")
  ) +
  scale_x_discrete(limits=Make_n_Popularity$Make) +
  xlab("Make") +
  ggtitle("Popularity Index by Make")

###############################################################################################

# Here we are trying to see how much make n model affects prices
# Splitting dataset
index <- sample(nrow(df2),as.integer(.75*nrow(df2)))
Test_dataset <- df2[-index,]
Test_dataset <- data.matrix(Test_dataset)
Training_dataset <- df2[index, ]
Training_dataset <- data.matrix(Training_dataset)

# Applying random forest
library(randomForest)
regressor2 <- randomForest(MSRP ~ ., data = Training_dataset, ntree = 1000, importance = T)
importance(regressor2)
varImpPlot(regressor2)

# Predicting MSRP 
pred3 <- predict(regressor2, Test_dataset[,-14])
pred4 <- predict(regressor2, Training_dataset[,-14])
# Mean error
mean_error2 <- sum(abs(pred3-Test_dataset[,14]))/length(pred3)
mean_percent2 <- mean_error/mean(Test_dataset[,14])
mean_percent2
library(Metrics)
rmse_train2 <- rmse(Training_dataset[,14], pred4)
rmse_test2 <- rmse(Test_dataset[,14],pred3)
