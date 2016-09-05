# Load libraries
library(rpart)
library(e1071)
library(discretization)
library(infotheo)

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
text(tree, use.n = T, pretty = TRUE)
title("Training tree")

# Predictions
predictions <- predict(tree, testData, type="class")

# Compare
results <- table(testData$contraceptive.method, predictions)
accuracy <- sum(diag(results)) / sum(results) # accuracy 57%
accuracy1 <- results[1] / sum(results[1,]) # accuracy 69%
accuracy2 <- results[2] / sum(results[2,]) # accuracy 27%
accuracy3 <- results[3] / sum(results[3,]) # accuracy 30%


# Child ever born: >= 1
# Wife's age: < 37,5
# Wifes education: Case 1,2,3
#   => child ever born: >= 3
# Wifes education: Case 4
#   => child ever born: case 2 or less
#      => husband ocupation: not 1
#   => child ever born: 3 or more
#      => Wife's age < 33,5


# with naiveBayes
bayes <- naiveBayes(contraceptive.method ~ ., data=trainData)

# Predictions
predictionsBayes <- predict(bayes, testData, type="class")

# Compare
resultsBayes <- table(testData$contraceptive.method, predictionsBayes)
accuracyBayes <- sum(diag(resultsBayes)) / sum(resultsBayes) # accuracy 50%
accuracyBayes1 <- resultsBayes[1] / sum(resultsBayes[1,]) # accuracy 52%
accuracyBayes2 <- resultsBayes[2] / sum(resultsBayes[2,]) # accuracy 19%
accuracyBayes3 <- resultsBayes[3] / sum(resultsBayes[3,]) # accuracy 22%


