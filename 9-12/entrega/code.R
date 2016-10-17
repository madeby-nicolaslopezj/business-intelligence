# Load libraries
library(mlbench)
library(rpart)

# Load data
data(BreastCancer)

BreastCancer$Id <- NULL

# Set training and testing sets
sub <- sample(nrow(BreastCancer), floor(nrow(BreastCancer) * 0.7))
train<-BreastCancer[sub, ] #train set 70 %
test<-BreastCancer[-sub, ] #test set 30%

##
# Naive Bayes
##
naiveAlgo <- naiveBayes(Class~.,data=train)
output <- predict(naiveAlgo, test[, -10], type="raw")
predictNaive <- max.col(output)
confusionMatrix <- table(predictNaive, test[, 10]) #confusion matrix
confusionMatrix

# Accuracy
accuracyNaive <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
accuracyNaive

# ROC Curve
pred <- prediction(output[,2],test[, 10])
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
plot(perf, col=rainbow(10))
plot(perf, col=rainbow(10), print.cutoffs.at=cutpoint)

##
# Desition Tree
##
rpart.tree <- rpart(Class ~ ., data=train)
rpart.predictions <- predict(rpart.tree, test, type="class")
rpart.predictCol <- max.col(output)
plot(rpart.tree, margin=0.05)
text (rpart.tree, use.n = T, pretty = TRUE)
title("Training Set's Classification Tree")

# Accuracy
rpart.confusion <- table(test$Class, rpart.predictions)
rpart.accuracy <- sum(diag(rpart.confusion)) / sum(rpart.confusion)
rpart.accuracy

# ROC Curve
rpart.pred <- prediction(rpart.predictCol, test[, 10])
rpart.perf <- performance(rpart.pred, measure = "tpr", x.measure = "fpr") 
plot(rpart.perf, col=rainbow(10))
plot(rpart.perf, col=rainbow(10), print.cutoffs.at=cutpoint)

