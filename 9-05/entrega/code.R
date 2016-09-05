# Load libraries
library(rpart)

# Set the seed
set.seed(1111)

# Set working directory
setwd("/Users/nicolaslopezj/Code/r/bi/9-05")

# Set data
data <- read.csv("cmc.data", header = FALSE, sep=",")
data$V10 <- factor(data$V10)

# Generate training and testing set
sub <- sample(nrow(data), floor(nrow(data) * 0.7))
trainData<-data[sub, ] #70 % for training
testData<-data[-sub, ] #30 % for testing

# Create the tree
tree <- rpart(V10 ~ ., data=trainData)
plot(tree,margin=0.2)
text (tree, use.n = T, pretty = TRUE)
title("Training tree")

# Predictions
predictions <- predict(tree, testData, type="class")

# Compare
table(testData$V10, predictions)

# Prune
prune.rpart.tree <- prune(rpart.tree, cp=0.01)
plot(prune.rpart.tree,margin=0.2)
text (prune.rpart.tree, use.n = T, pretty = TRUE)




