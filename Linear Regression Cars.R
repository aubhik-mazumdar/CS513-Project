df <- read.csv('data.csv')
# Remove Tesla
df$Make <- gsub('Tesla', NA, df$Make)
df <- na.omit(df)
df$Make <- factor(df$Make)
df <- df[,c(-2,-10)]
df$Year <- factor(df$Year)
levels(df$Year)
library(caTools)
set.seed(123)
split = sample.split(df$MSRP, SplitRatio = 0.7)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)
reg <- lm(MSRP ~ ., data = training_set)
pred <- predict(reg,test_set)
summary(reg)
a<- as.numeric(test_set$MSRP)
b<-sum(pred-a)
b/3226

