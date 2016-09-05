# Load libraries
library(rpart)

# Set the seed
set.seed(1111)

# Set working directory
setwd("/Users/nicolaslopezj/Code/r/bi/9-05")

# Set data
data <- read.csv("cmc.data", header = TRUE, sep=",")
data$wife.education <- as.factor(data$wife.education)
data$husband.education <- as.factor(data$husband.education)
data$wife.islam <- as.factor(data$wife.islam)
data$wife.work <- as.factor(data$wife.work)
data$husband.occupation <- as.factor(data$husband.occupation)
data$living.standard <- as.factor(data$living.standard)
data$media.exposure <- as.factor(data$media.exposure)
data$contraceptive.method <- as.factor(data$contraceptive.method)

# Generate training and testing set
sub <- sample(nrow(data), floor(nrow(data) * 0.7))
trainData<-data[sub, ] #70 % for training
testData<-data[-sub, ] #30 % for testing

# Create the tree
tree <- rpart(contraceptive.method ~ ., data=trainData)
plot(tree,margin=0.2)
text (tree, use.n = T, pretty = TRUE)
title("Training tree")

# Predictions
predictions <- predict(tree, testData, type="class")

# Compare
results <- table(testData$contraceptive.method, predictions)
accuracy <- sum(diag(results)) / sum(results) # accuracy 57%
accuracy1 <- results[1] / sum(results[1,]) # accuracy 69%
accuracy2 <- results[2] / sum(results[2,]) # accuracy 27%
accuracy3 <- results[3] / sum(results[3,]) # accuracy 30%




